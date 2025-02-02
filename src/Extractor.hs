module Extractor where

import Data.Char (intToDigit)
import Text.Read (readMaybe)

import Defs

extractDigits :: String -> String
extractDigits barcode =
  let chunks = chunksOf 7 barcode  -- Divide o código em blocos de 7 caracteres
  in map (findDigit lCodes) (take 4 chunks) ++ map (findDigit rCodes) (drop 4 chunks)-- Converte cada bloco em um dígito

chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

findDigit :: [String] -> String -> Char
findDigit codes chunk =
  let digit = length (takeWhile (/= chunk) codes)
  in if digit < 10
       then intToDigit digit
       else error "ERRO: Bloco de código inválido."


getThinBarcode :: String -> Int -> String
getThinBarcode [] _ = []
getThinBarcode ls@(l:_) n = l : getThinBarcode (drop n ls) n

getArea :: String -> Int 
getArea [] = 0
getArea (l:ls) = if l == '1' then 1 + (getArea ls) else 0

getMargin :: [String] -> Int
getMargin [] = 0
getMargin (l:ls) = if all (== '0') l then 1 + getMargin ls else 0  

checkFormat :: [String] -> Either String (Int, [String])
checkFormat (f:d:ls)
  | f /= "P1" = Left "ERRO: Formato do arquivo invalido."
  | null ls  = Left "ERRO: Arquivo não possui codigo para leitura."
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
  case checkBarcode barcode of
    Left err -> Left err
    Right nakedCode -> Right (extractDigits nakedCode)

checkBarcode :: String -> Either String String
checkBarcode bs =
                 let (start, rest) = splitAt 3 bs
                     (leftCode, rest') = splitAt 28 rest
                     (middle, rest'') = splitAt 5 rest'
                     (rightCode, end) = splitAt 28 rest''
                 in if start == "101" && end == "101" && middle == "01010"
                      then Right (leftCode ++ rightCode)
                      else Left "ERRO: O codigo nao esta de acordo com os padroes EAN8."

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

extractorUsage :: IO ()
extractorUsage = putStrLn "Uso: 'ext <nome_do_arquivo>'"
