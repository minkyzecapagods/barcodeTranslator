module Main where

import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hPutStrLn, stderr, writeFile)
import System.IO.Error (tryIOError)
import System.Directory (doesFileExist)
import Data.Char (isDigit, intToDigit, digitToInt)
import Data.List (foldl')
import Text.Read (readMaybe)

-- Argumentos opcionais
data Options = Options
  { margin :: Int
  , area   :: Int
  , height :: Int
  , title  :: String }
    deriving (Show)

data PBMImage = PBMImage
  { totalWidth :: Int
  , totalHeight :: Int
  , filename :: String
  , ean8Code :: String
  , barcodeLine :: String } 
    deriving (Show)

lCodes :: [[Char]]
lCodes =
  [ "0001101", "0011001", "0010011", "0111101", "0100011"
  , "0110001", "0101111", "0111011", "0110111", "0001011"
  ]

rCodes :: [[Char]]
rCodes =
  [ "1110010", "1100110", "1101100", "1000010", "1011100"
  , "1001110", "1010000", "1000100", "1001000", "1110100"
  ]

defaultOptions :: Options
defaultOptions = Options { margin = 4, area = 3, height = 50, title = "" }

codeLen :: Int
codeLen = 67

maxSize :: Int
maxSize = 1024

options :: [OptDescr (Options -> Options)]
options =
  [ Option "m" ["margin"] 
    (ReqArg 
      (\arg opt -> opt { margin = read arg })
      "MARGEM") 
    "Define a margem da imagem em pixels"
  , Option "a" ["area"] 
    (ReqArg (\arg opt -> opt { area = read arg })
      "AREA") 
    "Define a area das barras em pixels"
  , Option "h" ["height"] 
    (ReqArg 
      (\arg opt -> opt { height = read arg })
      "ALTURA") 
    "Define a altura de cada barra em pixels"
  , Option "n" ["name"] 
    (ReqArg 
      (\arg opt -> opt { title = arg }) 
      "NOME") 
    "Define o nome do arquivo criado"
  ]
{-
main :: IO ()
main = pure ()
-}
main :: IO ()
main = do
  args <- getArgs
  processArgs args

processArgs :: [String] -> IO ()
processArgs [] = generatorUsage
processArgs ("gen":rest) = mainGenerator rest
processArgs ("ext":rest) = mainExtractor rest
processArgs _ = putStrLn "ERRO: Comando inválido. Use 'gen' ou 'ext'."

mainExtractor :: [String] -> IO ()
mainExtractor args = case args of
  [filename] -> extractFromFile filename
  _          -> extractorUsage

extractFromFile :: String -> IO ()
extractFromFile filename = do
  result <- tryIOError (readFile filename)
  case result of
    Left err -> hPutStrLn stderr $ "Erro ao ler o arquivo: " ++ show err
    Right content -> processContent content

processContent :: String -> IO ()
processContent content = case checkFormat (lines content) of
  Left err -> hPutStrLn stderr err
  Right (width, lines) -> case getIdentifier width lines of
    Left err -> hPutStrLn stderr err
    Right identifier -> putStrLn $ "Identificador: " ++ identifier

mainGenerator :: [String] -> IO ()
mainGenerator args = 
  case parseOptions args of
    Left err -> hPutStrLn stderr err
    Right (opts, identifier) -> handleValidInput opts identifier

handleValidInput :: Options -> String -> IO ()
handleValidInput opts identifier = 
  case validateIdentifier identifier of
    Left err -> hPutStrLn stderr err
    Right validId -> processPBMGeneration opts validId

processPBMGeneration :: Options -> String -> IO ()
processPBMGeneration opts validId = 
  case createPBMInfo opts validId of
    Left err -> hPutStrLn stderr err
    Right pbmImage -> createAndSavePBMImage pbmImage opts

createAndSavePBMImage :: PBMImage-> Options -> IO ()
createAndSavePBMImage pbmImage opts = do
  result <- createPBMImage pbmImage (height opts) (margin opts)
  either (hPutStrLn stderr) (const $ putStrLn "Arquivo criado com sucesso!") result
{-
mainGenerator2 :: [String] -> IO ()
mainGenerator2 args = do
  case parseOptions args of
    Left err   -> hPutStrLn stderr err
    Right (opts, identifier) -> do
      case validateIdentifier identifier of
        Left err      -> hPutStrLn stderr err
        Right validId -> case createPBMInfo opts validId of
            Left err      -> hPutStrLn stderr err
            Right pbmImage -> do
              result <- createPBMImage pbmImage (height opts) (margin opts)
              either (hPutStrLn stderr) (const $ putStrLn "Arquivo criado com sucesso!") result

mainExtractor2 :: [String] -> IO ()
mainExtractor args = do
  if length args /= 1
    then extractorUsage
    else do
      let filename = head args
      content <- try (readFile filename)
      case content of
        Left err -> hputStrLn stderr $ "Erro ao ler o arquivo: " ++ show err
        Right content -> case checkSize (lines content) of
                            Left err -> hputStrLn stderr err
                            Right (width, height, lines) -> case getIdentifier width height lines of
                                      Left err -> hputStrLn stderr err
                                      Right identifier -> putStrLn $ "Identificador: " ++ identifier
-}

-- EXTRATOR FUNCIAONA
extractDigits :: String -> String
extractDigits barcode =
  let chunks = chunksOf 7 barcode  -- Divide o código em blocos de 7 caracteres
  in (map (findDigit lCodes) (take 4 chunks)) ++ (map (findDigit rCodes) (drop 4 chunks))-- Converte cada bloco em um dígito

-- EXTRATOR Funciona
chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- EXTRATOR funciona
findDigit :: [String] -> String -> Char
findDigit codes chunk =
  let digit = length (takeWhile (/= chunk) codes)
  in if digit < 10
       then intToDigit digit
       else error "ERRO: Bloco de código inválido."


--EXTRATOR funciona
getThinBarcode :: String -> Int -> String
getThinBarcode [] n = []
getThinBarcode ls@(l:_) n = l : (getThinBarcode (drop n ls) n)

--EXTRATOR funciona
getArea :: String -> Int 
getArea [] = 0
getArea (l:ls) = if l == '1' then 1 + (getArea ls) else 0

--EXTRATOR funciona
getMargin :: [String] -> Int
getMargin [] = 0
getMargin (l:ls) = if all (== '0') l then (1 + getMargin ls) else 0  

--EXTRATOR funciona
checkFormat :: [String] -> Either String (Int, [String])
checkFormat (f:d:ls)
  | f /= "P1" = Left "ERRO: Formato do arquivo invalido."
  | ls == []  = Left "ERRO: Arquivo não possui codigo para leitura."
  | otherwise = case checkDimensions (words d) of
      Nothing     -> Left "ERRO: Dimensoes invalidas."
      Just (w, _) -> Right (w, ls)
checkFormat _ = Left "ERRO: Arquivo nao possui conteudo suficiente."

checkArea :: String -> Either String Int
checkArea barcode = case getArea barcode of
  0 -> Left "ERRO: A margem do documento está fora do padrão ou o documento não possui um código de barras."
  a -> Right a

getIdentifier :: Int -> [String] -> Either String String
getIdentifier w ls = do
  let margin = getMargin ls
  let noMarginBarcode = take (w - margin * 2) (drop margin (ls !! margin))
  area <- checkArea noMarginBarcode
  let barcode = getThinBarcode noMarginBarcode area
  leftNRightCode <- checkBarcode barcode
  let identifier = extractDigits leftNRightCode
  validateIdentifier identifier

{-
getIdentifier :: [String] -> Int -> Int -> Either String String
getIdentifier ls w h = let m = getMargin ls
                           extendedBarcode = ls !! m
                           a = getArea (take (w-m*2) (drop m extendedBarcode))
                           barcode = getOriginalCode a extendedBarcode
                        in if length barcode \= codeLen
                          then Left "ERRO: Algo deu errado na conversao para EAN8."
                          else cases checkBarcode barcode of
                                Nothing -> Left "ERRO: Codigo EAN8 invalido."
                                Just code -> do
                                              let identifier = extractDigits code
                                              case validateIdentifier identifier of
                                                Left err -> Left err
                                                Right id -> Right id

                                             
-}

-- EXTRATOR funciona??
checkBarcode :: String -> Either String String
checkBarcode bs =
                 let (start, rest) = splitAt 3 bs
                     (leftCode, rest') = splitAt 28 rest
                     (middle, rest'') = splitAt 5 rest'
                     (rightCode, end) = splitAt 28 rest''
                 in if start == "101" && end == "101" && middle == "01010"
                      then Right (leftCode ++ rightCode)
                      else Left ("ERRO: O codigo nao esta de acordo com os padroes EAN8.")

--EXTRATOR funciona
checkDimensions :: [String] -> Maybe (Int, Int)
checkDimensions [w, h] =
  let width = readMaybe w :: Maybe Int
      height = readMaybe h :: Maybe Int
  in case (width, height) of
    (Just w, Just h) -> if h > maxSize || w > maxSize || h < 1 && w < codeLen 
                          then Nothing
                          else Just (w, h)
    _ -> Nothing
checkDimensions _ = Nothing

--funciona
createPBMInfo :: Options -> String -> Either String PBMImage
createPBMInfo info id =
  let heightPBM = height info + 2 * margin info
      widthPBM = codeLen * area info + 2 * margin info
      filenamePBM = case title info of
        "" -> "barcode-output/" ++ id ++ ".pbm"
        t  -> "barcode-output/" ++ t ++ ".pbm"
      ean8CodePBM = toEAN8 id
      barcodeLinePBM = createBarcodeLine (area info) ean8CodePBM
  in if heightPBM > maxSize  || widthPBM > maxSize 
        || height info < 1 || area info < 1 || margin info < 1
          then Left "ERRO: Dimensões inválidas."
          else Right (PBMImage widthPBM heightPBM filenamePBM ean8CodePBM barcodeLinePBM)

--funciona
createPBMImage :: PBMImage -> Int -> Int -> IO (Either String ())
createPBMImage pbmImage height margin = do
  let marginLine = replicate (totalWidth pbmImage) '0'
  let marginColumn = replicate margin '0'
  let content = unlines $
        ["P1", show (totalWidth pbmImage) ++ " " ++ show (totalHeight pbmImage)] ++
        replicate margin marginLine ++
        replicate height (marginColumn ++ barcodeLine pbmImage ++ marginColumn) ++
        replicate margin marginLine
  tryWriteFile (filename pbmImage) content

--funciona
tryWriteFile :: FilePath -> String -> IO (Either String ())
tryWriteFile path content = do
  exists <- doesFileExist path
  if exists
    then do
      result <- getCharAndProcess
      case result of
        Nothing -> return (Left "Operacao cancelada.")
        Just _  -> writeFile path content >> return (Right ())
    else writeFile path content >> return (Right ())

--funciona
getCharAndProcess :: IO (Maybe ())
getCharAndProcess = do
    putStr "\nAVISO: O arquivo ja existe. Você deseja sobreescreve-lo? (s/n) "
    c <- getChar
    _ <- getLine
    case c of
        's' -> return (Just ())
        'n' -> return Nothing
        _   -> do 
              putStrLn "Entrada inválida. Tente novamente."
              getCharAndProcess

createBarcodeLine :: Int -> String -> String
createBarcodeLine area ean8Code = concatMap (\c -> replicate area c) ean8Code

--funciona
toEAN8 :: String -> String
toEAN8 id =
  let left = concatMap (\c -> lCodes !! digitToInt c) (take 4 id)
      middle = "01010"
      right = concatMap (\c -> rCodes !! digitToInt c) (drop 4 id)
  in "101" ++ left ++ middle ++ right ++ "101"

--funciona
validateIdentifier :: String -> Either String String
validateIdentifier id
  | length id /= 8                     = Left "ERRO: O identificador deve conter exatamente 8 digitos."
  | not (all isDigit id)               = Left "ERRO: O identificador deve conter apenas numeros."
  | last id /= getVerificationDigit id = Left ("ERRO: Digito verificador invalido. O ultimo digito deve ser " ++ [getVerificationDigit id] ++ ".")
  | otherwise                          = Right id

--funciona
getVerificationDigit :: String -> Char
getVerificationDigit id =
  let sum = foldl' (\acc (i, c) -> acc + (digitToInt c) * (if odd i then 3 else 1)) 0 (zip [1..] (take 7 id))
      nextMulTen = ((sum + 9) `div` 10) * 10
      digit = nextMulTen - sum
  in intToDigit digit

-- funciona
parseOptions :: [String] -> Either String (Options, String)
parseOptions args = 
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
    in if  not (null errors)
      then Left (unlines errors)
      else case nonOptions of
        []           -> Left "ERRO: Argumento identificador nao informado."
        [identifier] -> Right ((foldl' (flip id) defaultOptions actions), identifier)
        _            -> Left "ERRO: Mais de um argumento identificador informado ou o identificador nao foi inserido no final do comando."

generatorUsage :: IO ()
generatorUsage = putStrLn "Uso ./gen <opcoes> <identificador>"

extractorUsage :: IO ()
extractorUsage = putStrLn "Uso: 'ext <nome_do_arquivo>'"
