module Defs where

import System.Console.GetOpt

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
