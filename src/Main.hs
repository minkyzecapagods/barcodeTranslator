import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)
import Data.List (foldl')
import Data.Char (isDigit)

import Defs
import Generator
import Extractor

main :: IO ()
main = do
  args <- getArgs
  processArgs args

processArgs :: [String] -> IO ()
processArgs [] = mainUsage
processArgs ("gen":rest) = mainGenerator rest
processArgs ("ext":rest) = mainExtractor rest
processArgs _ = putStrLn "ERRO: Comando inválido. Use 'gen' ou 'ext'."

mainGenerator :: [String] -> IO ()
mainGenerator [] = generatorUsage
mainGenerator args = 
  case parseOptions args of
    Left err -> hPutStrLn stderr err
    Right (opts, identifier) -> handleValidInput opts identifier

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
  Right (width, codeLines) -> case getIdentifier width codeLines of
    Left err -> hPutStrLn stderr err
    Right identifier -> case validateIdentifier identifier of
      Left err -> hPutStrLn stderr err
      Right verifiedId -> putStrLn $ "Identificador: " ++ verifiedId

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

parseOptions :: [String] -> Either String (Options, String)
parseOptions args = 
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
    in if  not (null errors)
      then Left (unlines errors)
      else case nonOptions of
        []           -> Left "ERRO: Argumento identificador nao informado."
        [identifier] -> Right (foldl' (flip id) defaultOptions actions, identifier)
        _            -> Left "ERRO: Mais de um argumento identificador informado ou o identificador nao foi inserido no final do comando."

validateIdentifier :: String -> Either String String
validateIdentifier identifier
  | length identifier /= 8 = Left "ERRO: O identificador deve conter exatamente 8 digitos."
  | not (all isDigit identifier) = Left "ERRO: O identificador deve conter apenas numeros."
  | last identifier /= getVerificationDigit identifier = Left ("ERRO: Digito verificador invalido. O ultimo digito deve ser " ++ [getVerificationDigit identifier] ++ ".")
  | otherwise = Right identifier

mainUsage :: IO ()
mainUsage = do
    putStrLn "TRADUTOR DE CODIGO DE BARRAS"
    putStrLn "\tGera um codigo de barras a partir de um identificador ou extrai um identificador a partir de um codigo de barras."
    putStrLn "Uso:"
    putStrLn "\t./barcodeTranslator <opcao>"
    putStrLn "Opcoes:"
    putStrLn "\tgen"
    putStrLn "\t\tGera um arquivo PBM baseado no input do usuario."
    putStrLn "\text"
    putStrLn "\t\tExtrai o identificador do codigo de barras no arquivo .pbm informado."
