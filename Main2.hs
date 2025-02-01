module Main where

import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hPutStrLn, stderr, writeFile)
import System.Directory (doesFileExist)
import Data.Char (isDigit, intToDigit, digitToInt)
import Data.List (foldl')
import Control.Monad (when)
import Text.Read (readMaybe)
import Control.Exception (try, SomeException)

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

-- Códigos L e R para EAN-8
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

-- Opções padrão
defaultOptions :: Options
defaultOptions = Options { margin = 4, area = 3, height = 50, title = "" }

-- Constantes
codeLen :: Int
codeLen = 67

maxSize :: Int
maxSize = 1024

-- Definição das opções de linha de comando
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

-- Função principal
main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Uso: ./barcode gen <opções> <identificador> | ./barcode ext <arquivo.pbm>"
    else case head args of
      "gen" -> mainGenerator (tail args)
      "ext" -> mainExtractor (tail args)
      _     -> putStrLn "ERRO: Comando inválido. Use 'gen' ou 'ext'."

-- Gerador de código de barras
mainGenerator :: [String] -> IO ()
mainGenerator args = do
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

-- Extrator de código de barras
mainExtractor :: [String] -> IO ()
mainExtractor args = do
  if length args /= 1
    then extractorUsage
    else do
      let filename = head args
      content <- try (readFile filename) :: IO (Either SomeException String)
      case content of
        Left err -> hPutStrLn stderr $ "Erro ao ler o arquivo: " ++ show err
        Right content -> case checkSize (lines content) of
                            Left err -> hPutStrLn stderr err
                            Right (width, height, ls) -> case getIdentifier width height ls of
                                      Left err -> hPutStrLn stderr err
                                      Right identifier -> putStrLn $ "Identificador: " ++ identifier

-- Função para extrair dígitos de uma parte do código de barras
extractDigits :: [[Char]] -> String -> Either String String
extractDigits codes barcodePart =
  let chunks = chunksOf 7 barcodePart  -- Divide o código em blocos de 7 caracteres
  in sequence $ map (findDigit codes) chunks  -- Converte cada bloco em um dígito

-- Função para dividir uma lista em sublistas de tamanho fixo
chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Função para encontrar o dígito correspondente a um bloco de código
findDigit :: [[Char]] -> String -> Either String Char
findDigit codes chunk =
  let digit = length (takeWhile (/= chunk) codes)  -- Encontra o índice do bloco na lista de códigos
  in if digit < 10
       then Right (intToDigit digit)  -- Converte o índice para um caractere numérico
       else Left "ERRO: Bloco de código inválido."

-- Função para verificar o tamanho do arquivo PBM
checkSize :: [String] -> Either String (Int, Int, [String])
checkSize (f:d:ls)
  | f /= "P1" = Left "ERRO: Formato do arquivo inválido."
  | ls == []  = Left "ERRO: Arquivo não possui código para leitura."
  | otherwise = case checkDimensions (words d) of
      Nothing     -> Left "ERRO: Dimensões inválidas."
      Just (w, h) -> Right (w, h, ls)
checkSize _ = Left "ERRO: Arquivo não possui conteúdo suficiente."

-- Função para extrair o identificador do código de barras
getIdentifier :: Int -> Int -> [String] -> Either String String
getIdentifier w h ls = 
  let m = getMargin ls
      extendedBarcode = ls !! m
      a = getArea (take (w - m * 2) (drop m extendedBarcode))
      barcode = getOriginalCode a extendedBarcode
  in if length barcode /= codeLen
        then Left "ERRO: Algo deu errado na conversão para EAN8."
        else case checkBarcode barcode of
              Nothing -> Left "ERRO: Código EAN8 inválido."
              Just code -> extractDigits (lCodes ++ rCodes) code

-- Função para verificar as dimensões do arquivo PBM
checkDimensions :: [String] -> Maybe (Int, Int)
checkDimensions [w, h] =
  let width = readMaybe w :: Maybe Int
      height = readMaybe h :: Maybe Int
  in case (width, height) of
    (Just w, Just h) -> if h > maxSize || w > maxSize || h < 1 || w < codeLen 
                          then Nothing
                          else Just (w, h)
    _ -> Nothing
checkDimensions _ = Nothing

-- Função para criar a estrutura PBMImage
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
        || height info < 1 || area info < 1 || margin info < 0
          then Left "ERRO: Dimensões inválidas."
          else Right $ PBMImage widthPBM heightPBM filenamePBM ean8CodePBM barcodeLinePBM

-- Função para criar o conteúdo do arquivo PBM
createPBMImage :: PBMImage -> Int -> Int -> IO (Either String ())
createPBMImage pbmImage height margin = do
  let content = createPBMContent pbmImage height margin
  tryWriteFile (filename pbmImage) content

-- Função para criar o conteúdo do arquivo PBM (pura)
createPBMContent :: PBMImage -> Int -> Int -> String
createPBMContent pbmImage height margin =
  let marginLine = replicate (totalWidth pbmImage) '0'
      marginColumn = replicate margin '0'
  in unlines $
       ["P1", show (totalWidth pbmImage) ++ " " ++ show (totalHeight pbmImage)] ++
       replicate margin marginLine ++
       replicate height (marginColumn ++ barcodeLine pbmImage ++ marginColumn) ++
       replicate margin marginLine

-- Função para escrever o arquivo PBM
tryWriteFile :: FilePath -> String -> IO (Either String ())
tryWriteFile path content = do
  exists <- doesFileExist path
  if exists
    then do
      result <- getCharAndProcess
      case result of
        Nothing -> return (Left "Operação cancelada.")
        Just _  -> Right <$> writeFile path content
    else Right <$> writeFile path content

-- Função para processar a confirmação do usuário
getCharAndProcess :: IO (Maybe ())
getCharAndProcess = do
    putStr "\nAVISO: O arquivo já existe. Você deseja sobrescrevê-lo? (s/n) "
    c <- getChar
    _ <- getLine
    case c of
        's' -> return (Just ())
        'n' -> return Nothing
        _   -> do 
              putStrLn "Entrada inválida. Tente novamente."
              getCharAndProcess

-- Função para criar a linha do código de barras
createBarcodeLine :: Int -> String -> String
createBarcodeLine area ean8Code = concatMap (\c -> replicate area c) ean8Code

-- Função para converter um identificador para o formato EAN-8
toEAN8 :: String -> String
toEAN8 id =
  let left = concatMap (\c -> lCodes !! digitToInt c) (take 4 id)
      middle = "01010"
      right = concatMap (\c -> rCodes !! digitToInt c) (drop 4 id)
  in "101" ++ left ++ middle ++ right ++ "101"

-- Função para validar o identificador
validateIdentifier :: String -> Either String String
validateIdentifier id
  | length id /= 8                     = Left "ERRO: O identificador deve conter exatamente 8 dígitos."
  | not (all isDigit id)               = Left "ERRO: O identificador deve conter apenas números."
  | last id /= getVerificationDigit id = Left ("ERRO: Dígito verificador inválido. O último dígito deve ser " ++ [getVerificationDigit id] ++ ".")
  | otherwise                          = Right id

-- Função para calcular o dígito verificador
getVerificationDigit :: String -> Char
getVerificationDigit id =
  let sum = foldl' (\acc (i, c) -> acc + (digitToInt c) * (if odd i then 3 else 1)) 0 (zip [1..] (take 7 id))
      nextMulTen = ((sum + 9) `div` 10) * 10
      digit = nextMulTen - sum
  in intToDigit digit

-- Função para processar as opções de linha de comando
parseOptions :: [String] -> Either String (Options, String)
parseOptions args = 
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  in if not (null errors)
      then Left (unlines errors)
      else case nonOptions of
        []           -> Left "ERRO: Argumento identificador não informado."
        [identifier] -> Right ((foldl' (flip id) defaultOptions actions), identifier)
        _            -> Left "ERRO: Mais de um argumento identificador informado."

-- Função para exibir o uso do gerador
generatorUsage :: IO ()
generatorUsage = putStrLn "Uso: ./gen <opções> <identificador>"

-- Função para exibir o uso do extrator
extractorUsage :: IO ()
extractorUsage = putStrLn "Uso: ./ext <arquivo.pbm>"

getOriginalCode :: String -> Int -> Maybe String
getOriginalCode [] n = Nothing
getOriginalCode ls@(l:_) n = Just (l : getOriginalCode (drop n ls))

checkBarcode :: String -> Maybe String
checkBarcode bs =
                 let (start, rest) = splitAt 3 bs
                     (leftCode, rest') = splitAt 28 rest
                     (middle, rest'') = splitAt 5 rest'
                     (rightCode, end) = splitAt 28 rest''
                 in if start == "101" || end == "101" || middle == "01010"
                      then Just (leftCode ++ rightCode)
                      else Nothing

