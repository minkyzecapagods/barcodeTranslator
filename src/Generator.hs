module Generator where

import Data.List (foldl')
import System.Directory (doesFileExist)
import Data.Char (intToDigit, digitToInt)
import System.IO (hFlush, stdout)

import Defs

createPBMInfo :: Options -> String -> Either String PBMImage
createPBMInfo info identifier =
  let heightPBM = height info + 2 * margin info
      widthPBM = codeLen * area info + 2 * margin info
      filenamePBM = case title info of
        "" -> "barcode-output/" ++ identifier ++ ".pbm"
        t  -> "barcode-output/" ++ t ++ ".pbm"
      ean8CodePBM = toEAN8 identifier
      barcodeLinePBM = createBarcodeLine (area info) ean8CodePBM
  in if heightPBM > maxSize  || widthPBM > maxSize 
        || height info < 1 || area info < 1 || margin info < 1
          then Left "ERRO: Dimensões inválidas."
          else Right (PBMImage widthPBM heightPBM filenamePBM ean8CodePBM barcodeLinePBM)

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
    hFlush stdout
    c <- getChar
    _ <- getLine
    case c of
        's' -> return (Just ())
        'n' -> return Nothing
        _   -> do 
              putStrLn "Entrada inválida. Tente novamente."
              getCharAndProcess

createBarcodeLine :: Int -> String -> String
createBarcodeLine area = concatMap (replicate area)

toEAN8 :: String -> String
toEAN8 identifier =
  let left = concatMap (\c -> lCodes !! digitToInt c) (take 4 identifier)
      middle = "01010"
      right = concatMap (\c -> rCodes !! digitToInt c) (drop 4 identifier)
  in "101" ++ left ++ middle ++ right ++ "101"

getVerificationDigit :: String -> Char
getVerificationDigit identifier =
  let weightedSum = foldl' (\acc (i, c) -> acc + digitToInt c * (if odd i then 3 else 1)) 0 (zip [1..] (take 7 identifier))
      nextMulTen = ((weightedSum + 9) `div` 10) * 10
      digit = nextMulTen - weightedSum
  in intToDigit digit

generatorUsage :: IO ()
generatorUsage = do
    putStrLn "GERADOR DE CODIGO DE BARRAS"
    putStrLn "\tGera um arquivo PBM baseado no input do usuario."
    putStrLn "Uso:"
    putStrLn "\t./barcodeTranslator gen<opcao> ... <identificador>"
    putStrLn "Opcoes:"
    putStrLn "\t-m <pixels>"
    putStrLn "\t\tDeixa o usuario definir a margem baseado no input em <pixels>"
    putStrLn "\t\tSem -m, a margem sera de 4px."
    putStrLn "\t-a <pixels>"
    putStrLn "\t\tDeixa o usuario definir a area das barras baseado no input em <pixels>"
    putStrLn "\t\tSem -a, a area sera de 3px."
    putStrLn "\t-h <pixels>"
    putStrLn "\t\tDeixa o usuario definir a altura do codigo de barras baseado no input em <pixels>"
    putStrLn "\t\tSem -h, a altura sera de 50px."
    putStrLn "\t-n <file_name>"
    putStrLn "\t\tDeixa o usuario definir o nome do arquivo .pbm gerado baseado no input <file_name>"
    putStrLn "\t\tSem -n, o nome do arquivo sera o identificador fornecido."
