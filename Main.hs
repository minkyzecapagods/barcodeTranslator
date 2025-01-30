module Main where

import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hPutStrLn, stderr, writeFile)
import System.Directory (doesFileExist)
import Data.Char (isDigit, intToDigit, digitToInt)
import Data.List (foldl')

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
  if null args
    then generatorUsage
  else case head args of
    "gen" -> mainGenerator (tail args)
 --   "ext" -> mainExtractor (tail args)
    _     -> putStrLn "ERRO: Comando invalido. Use 'gen' ou 'ext'."


mainGenerator :: [String] -> IO ()
mainGenerator args = do
  case parseOptions args of
    Left err   -> hPutStrLn stderr err
    Right (opts, identifier) -> do
      case validateIdentifier identifier of
        Left err      -> hPutStrLn stderr err
        Right validId -> do
          let pbmImage = createPBMInfo opts validId
          result <- createPBMImage pbmImage (height opts) (margin opts)
          either (hPutStrLn stderr) (const $ putStrLn "Arquivo criado com sucesso!") result
          

createPBMInfo :: Options -> String -> PBMImage
createPBMInfo info id =
  let heightPBM = height info + 2 * margin info
      widthPBM = codeLen * area info + 2 * margin info
      filenamePBM = case title info of
        "" -> "barcode-output/" ++ id ++ ".pbm"
        t  -> "barcode-output/" ++ t ++ ".pbm"
      ean8CodePBM = toEAN8 id
      barcodeLinePBM = createBarcodeLine (area info) ean8CodePBM
  in PBMImage widthPBM heightPBM filenamePBM ean8CodePBM barcodeLinePBM


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

toEAN8 :: String -> String
toEAN8 id =
  let left = concatMap (\c -> lCodes !! digitToInt c) (take 4 id)
      middle = "01010"
      right = concatMap (\c -> rCodes !! digitToInt c) (drop 4 id)
  in "101" ++ left ++ middle ++ right ++ "101"

validateIdentifier :: String -> Either String String
validateIdentifier id
  | length id /= 8                     = Left "ERRO: O identificador deve conter exatamente 8 digitos."
  | not (all isDigit id)               = Left "ERRO: O identificador deve conter apenas numeros."
  | last id /= getVerificationDigit id = Left ("ERRO: Digito verificador invalido. O ultimo digito deve ser " ++ [getVerificationDigit id] ++ ".")
  | otherwise                          = Right id

getVerificationDigit :: String -> Char
getVerificationDigit id =
  let sum = foldl' (\acc (i, c) -> acc + (digitToInt c) * (if odd i then 3 else 1)) 0 (zip [1..] (take 7 id))
      nextMulTen = ((sum + 9) `div` 10) * 10
      digit = nextMulTen - sum
  in intToDigit digit

-- Mudei aq
parseOptions :: [String] -> Either String (Options, String)
parseOptions args = 
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
    in if  not (null errors)
      then Left (unlines errors)
      else case nonOptions of
        []           -> Left "ERRO: Argumento identificador nao informado."
        [identifier] -> Right ((foldl' (flip id) defaultOptions actions), identifier)
        _            -> Left "ERRO: Mais de um argumento identificador informado."

generatorUsage :: IO ()
generatorUsage = putStrLn "Uso ./gen <opcoes> <identificador>"
