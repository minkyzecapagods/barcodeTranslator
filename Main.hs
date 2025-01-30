module Main where

import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hPutStrLn, stderr)
import Data.Char (isDigit, intToDigit, digitToInt)
import Data.List (foldl')

-- Argumentos opcionais
data Options = Options
  { margin :: Int
  , area   :: Int
  , height :: Int
  , title  :: String }
    deriving (Show)

defaultOptions :: Options
defaultOptions = Options { margin = 4, area = 3, height = 50, title = "" }

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
main :: IO ()
main = pure ()
{-
main :: IO ()
main = do
  args <- getArgs
  if null args
    then generatorUsage
  else case head args of
    "gen" -> mainGenerator (tail args)
    "ext" -> mainExtractor (tail args)
    _     -> putStrLn "ERRO: Comando inválido. Use 'gen' ou 'ext'."


mainGenerator :: [String] -> IO ()
mainGenerator args = do
  case parseOptions args of
    Left err -> hPutStrLn stderr err
    Right opts -> do
      let identifier = head (nonOptions args)
      case validateIdentifier identifier of
        Left err -> hPutStrLn stderr err
        Right validId -> do
          
-}

validateIdentifier :: String -> Either String String
validateIdentifier id
  | length id /= 8 = Left "ERRO: O identificador deve conter exatamente 8 digitos."
  | not (all isDigit id) = Left "ERRO: O identificador deve conter apenas numeros."
  | last id /= getVerificationDigit id = Left ("ERRO: Digito verificador invalido. O ultimo digito deve ser " ++ [getVerificationDigit id] ++ ".")
  | otherwise = Right id

getVerificationDigit :: String -> Char
getVerificationDigit id =
  let sum = foldl' (\acc (i, c) -> acc + (digitToInt c) * (if odd i then 3 else 1)) 0 (zip [1..] (take 7 id))
      nextMulTen = ((sum + 9) `div` 10) * 10
      digit = nextMulTen - sum
  in intToDigit digit

-- Mudei aq
parseOptions :: [String] -> Either String Options
parseOptions args = 
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
    in if  not (null errors)
      then Left (unlines errors)
      else if null nonOptions || length nonOptions > 1
        then Left "ERRO: E necessario informar exatamente um argumento identificador."
        else Right (foldl' (flip id) defaultOptions actions)

generatorUsage :: IO ()
generatorUsage = putStrLn "Uso ./gen <opcoes> <identificador>"
