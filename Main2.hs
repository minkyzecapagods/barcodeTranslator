module Main where

import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hPutStrLn, stderr)
import System.Directory (doesFileExist)
import Data.Char (isDigit, intToDigit, digitToInt)
import Data.List (foldl', unfoldr)
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
  { totalWidth  :: Int
  , totalHeight :: Int
  , filename    :: String
  , ean8Code   :: String
  , barcodeLine :: String }
  deriving (Show)

lCodes, rCodes :: [String]
lCodes = ["0001101", "0011001", "0010011", "0111101", "0100011", "0110001", "0101111", "0111011", "0110111", "0001011"]
rCodes = ["1110010", "1100110", "1101100", "1000010", "1011100", "1001110", "1010000", "1000100", "1001000", "1110100"]

defaultOptions :: Options
defaultOptions = Options { margin = 4, area = 3, height = 50, title = "" }

codeLen, maxSize :: Int
codeLen = 67
maxSize = 1024

options :: [OptDescr (Options -> Options)]
options =
  [ Option "m" ["margin"] (ReqArg (\arg opt -> opt { margin = read arg }) "MARGEM") "Define a margem da imagem em pixels"
  , Option "a" ["area"] (ReqArg (\arg opt -> opt { area = read arg }) "AREA") "Define a area das barras em pixels"
  , Option "h" ["height"] (ReqArg (\arg opt -> opt { height = read arg }) "ALTURA") "Define a altura de cada barra em pixels"
  , Option "n" ["name"] (ReqArg (\arg opt -> opt { title = arg }) "NOME") "Define o nome do arquivo criado"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("gen":rest) -> mainGenerator rest
    ("ext":rest) -> mainExtractor rest
    _             -> putStrLn "ERRO: Comando inválido. Use 'gen' ou 'ext'."

mainGenerator :: [String] -> IO ()
mainGenerator args = case parseOptions args of
  Left err -> hPutStrLn stderr err
  Right (opts, identifier) -> case validateIdentifier identifier of
    Left err      -> hPutStrLn stderr err
    Right validId -> case createPBMInfo opts validId of
      Left err      -> hPutStrLn stderr err
      Right pbmImg -> putStrLn "Arquivo criado com sucesso!"

mainExtractor :: [String] -> IO ()
mainExtractor [filename] = do
  content <- try (readFile filename) :: IO (Either SomeException String)
  case content of
    Left err -> hPutStrLn stderr $ "Erro ao ler o arquivo: " ++ show err
    Right txt -> case checkSize (lines txt) of
      Left err -> hPutStrLn stderr err
      Right (w, h, ls) -> putStrLn "Identificação concluída."
mainExtractor _ = extractorUsage

extractDigits :: [String] -> String -> String
extractDigits codes = map (findDigit codes) . chunksOf 7

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . unfoldr (Just . splitAt n)

findDigit :: [String] -> String -> Char
findDigit codes chunk = maybe (error "ERRO: Bloco de código inválido.") intToDigit (lookup chunk (zip codes [0..9]))

checkSize :: [String] -> Either String (Int, Int, [String])
checkSize ("P1":d:ls) = case checkDimensions (words d) of
  Just (w, h) -> Right (w, h, ls)
  Nothing     -> Left "ERRO: Dimensões inválidas."
checkSize _ = Left "ERRO: Formato de arquivo inválido."

checkDimensions :: [String] -> Maybe (Int, Int)
checkDimensions [w, h] = do
  width  <- readMaybe w
  height <- readMaybe h
  if height > maxSize || width > maxSize || height < 1 || width < codeLen then Nothing else Just (width, height)
checkDimensions _ = Nothing

extractorUsage :: IO ()
extractorUsage = putStrLn "Uso: ./ext <arquivo.pbm>"
