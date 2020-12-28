{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Melodics.ByteString.Ukrainian.Arr
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Maintainer  :  olexandr543@yahoo.com
--
-- Functions provide functionality of a musical instrument synthesizer or for Ukrainian speech synthesis
-- especially for poets, translators and writers. Is rewritten from the module Melodics.Ukrainian from the
-- @mmsyn6ukr@ package : 'https://hackage.haskell.org/package/mmsyn6ukr'
-- Phonetic material is taken from the :
--
-- Solomija Buk, Ján Mačutek, Andrij Rovenchak. Some properties of
-- the Ukrainian writing system. [Electronic resource] https://arxiv.org/ftp/arxiv/papers/0802/0802.4198.pdf

module Melodics.ByteString.Ukrainian.Arr (
  -- * Basic functions
  convertToProperUkrainianS
  , convertToProperUkrainianB
  , isUkrainianL
  , linkFileName
  , showInteresting
) where

import qualified Data.String as S
import Data.Maybe (fromJust)
import Data.Char
import GHC.Arr
import CaseBi.Arr
import qualified Data.ByteString.Char8 as B

{-
-- Inspired by: https://mail.haskell.org/pipermail/beginners/2011-October/008649.html
-}

data Triple = Z | O | T
  deriving (Eq,Ord,Show)

convertToProperUkrainianS :: String -> String
convertToProperUkrainianS = correctB . correctA . applyChanges . bsToCharUkr . createTuplesByAnalysis . secondConv . filterUkr . changeIotated .
   filter (\x -> isUkrainianL x || isSpace x || isControl x || isPunctuation x) . map toLower

isUkrainianL :: Char -> Bool
isUkrainianL y | (y >= '\1070' && y <= '\1097') = True
               | otherwise = getBFstLSorted' False (map (\x -> (x, True)) "'-\700\1028\1030\1031\1068\1100\1102\1103\1108\1110\1111\1168\1169\8217") y

changeIotated :: String -> String
changeIotated (x:y:zs)
  | (y `elem` ("\1102\1103\1108\1110"::String)) && isConsNotJ x = x:'\1100':(case y of { '\1102' -> '\1091' ; '\1103' -> '\1072' ; '\1108' -> '\1077' ; ~r -> '\1110' }):changeIotated zs
  | otherwise = x:changeIotated (y:zs)
changeIotated xs = xs

isConsNotJ :: Char -> Bool
isConsNotJ = getBFstLSorted' False (zip "\1073\1074\1075\1076\1078\1079\1082\1083\1084\1085\1087\1088\1089\1090\1092\1093\1094\1095\1096\1097\1169" (repeat True))

filterUkr :: String -> B.ByteString
filterUkr = B.pack . map toBSUkr

toBSUkr :: Char -> Char
toBSUkr x = getBFstLSorted' x (zip "\700\1072\1073\1074\1075\1076\1077\1078\1079\1080\1081\1082\1083\1084\1085\1086\1087\1088\1089\1090\1091\1092\1093\1094\1095\1096\1097\1100\1102\1103\1108\1110\1111\1169\8217" "LabvhdeBzyCklmnoprstufxcEFGqHIJiKgM") x

secondConv :: B.ByteString -> B.ByteString
secondConv = B.concatMap f
  where f y
         | isSpace y || isControl y = B.singleton '1'
         | otherwise = getBFstLSorted' (B.singleton y) (zip "'-GHIJKLM" [B.singleton '0',B.singleton '0',"FE","Cu","Ca","Ce","Ci",B.singleton '0',B.singleton '0']) y

createTuplesByAnalysis :: B.ByteString -> [(B.ByteString, Triple)]
createTuplesByAnalysis x
  | B.null x = []
  | getBFstLSorted' False (zip "BEFcdfhknpstxz" (repeat True)) . B.head $ x = initialA x
  | not (B.null . B.tail $ x) && (B.index x 1 == 'C' && isConsNotJ (B.head x)) = (B.copy . B.singleton . B.head $ x, T):(B.singleton 'q', Z):createTuplesByAnalysis (B.drop 2 x)
  | otherwise = (B.copy . B.singleton . B.head $ x, Z):createTuplesByAnalysis (B.tail x)


initialA :: B.ByteString -> [(B.ByteString, Triple)]
initialA t1
  | B.null t1 = []
  | canChange t == O = (B.singleton '1', Z):initialA ts
  | canChange t == Z = (B.singleton t, Z):initialA ts
  | getBFstLSorted' False (zip "cdnstx" (repeat True)) t =
     let (us,vs) = B.splitAt 2 t1 in
       if getBFstLSorted' False (zip ["cq","dB","dz","nt","sq","st","tq","ts","xh"] (repeat True)) us
        then (B.copy us, T):initialA vs
        else (B.singleton t, T):initialA ts
  | otherwise = (B.singleton t, T):initialA ts
      where (t,ts) = fromJust . B.uncons $ t1

canChange :: Char -> Triple
canChange x
  | isSpace x || isControl x || x == '-' = O
  | getBFstLSorted' False (zip "BEFcdfhknpstxz" (repeat True)) x = T
  | otherwise = Z

bsToCharUkr :: [(B.ByteString,Triple)] -> [(Char,Triple)]
bsToCharUkr zs
 | null zs = []
 | otherwise = map g zs
     where g (ts,k)
             | B.null ts = ('0',Z)
             | otherwise = (getBFstLSorted' (B.head ts) (zip ["cq","dB","dz","nt","sq","st","tq","ts","xh"]  "wjANDOPch") ts,k)

applyChanges :: [(Char, Triple)] -> [(Char, Triple)]
applyChanges [] = []
applyChanges ys = foldr f v ys
  where v = []
        f x xs
          | null xs = (:[]) . (\(y,_) -> (y, Z)) $ x
          | snd x == T =
               getBFstLSorted' (fst x, Z) (zip "ABDEFNOPcdfhkpstwxz" [дзT xs, жT xs,  сьT xs, чT xs, шT xs, нтT xs, стT xs, тьT xs,
                 цT xs, дT xs, фT xs, гT xs, кT xs, пT xs, сT xs, тT xs, цьT xs, хT xs, зT xs]) (fst x):xs
          | otherwise = x:xs

isVoicedObstruent :: B.ByteString -> Bool
isVoicedObstruent = getBFstLSorted' False [("A",True),("B",True),("Q",True),("R",True),("T",True),("b",True),("d",True),("g",True),("h",True),
  ("j",True),("z", True)] . B.take 1

isVoicedObstruentH :: B.ByteString -> Bool
isVoicedObstruentH = getBFstLSorted' False [("A",True),("B",True),("b",True),("d",True),("g",True),("h",True),("j",True),("z", True)] . B.take 1

isVoicedObstruentS :: B.ByteString -> Bool
isVoicedObstruentS = (\u -> any (== u) ["Q","R","T"]) . B.take 1

isSoftDOrL :: [(Char, Triple)] -> Bool
isSoftDOrL xs = getBFstLSorted' False (zip ["bq","cq","dq","fq","lq","mq","nq","pq","sq","tq","vq"] (repeat True)) (takeFromFT_ 2 xs) ||
  getBFstLSorted' False (zip ["P","Q","R","S","T"] . repeat $ True) (takeFromFT_ 1 xs)

isSoftDen :: [(Char, Triple)] -> Bool
isSoftDen xs = getBFstLSorted' False (zip ["Aq","cq","dq","lq","nq","sq","tq","zq"] . repeat $ True) (takeFromFT_ 2 xs) ||
  getBFstLSorted' False (zip ["P","Q","R","S","T"] . repeat $ True) (takeFromFT_ 1 xs)

-- in the further ??T functions the last (, T) means that it must be afterwards be separated with the soft sign into two tuples (1 additional function in the composition)
-- need further processing means that there should be additional checks and may be transformations. May be they can be omitted

гT :: [(Char, Triple)] -> (Char, Triple)
гT (t:_) | fst t == 'k' || fst t == 't' = ('x', Z)
         | otherwise = ('h', Z)
гT _ = ('h', Z)

дT :: [(Char, Triple)] -> (Char, Triple)
дT t1@(_:_) | takeFromFT_ 1 t1 `elem` ["B","E","F"] = ('j', Z) -- need further processing д дж
            | takeFromFT_ 2 t1 `elem` ["sq","cq"] = ('Q', T) -- need further processing д дзь
            | takeFromFT_ 1 t1 `elem` ["D","w"] = ('Q', T) -- need further processing д дзь
            | takeFromFT_ 1 t1 `elem` ["z","s","c"] = ('A', Z) -- need further processing  д дз
            | otherwise = ('d', Z)
дT _ = ('d', Z)

дзT :: [(Char, Triple)] -> (Char, Triple)
дзT t1@(_:_) | isSoftDOrL t1 = ('Q', T)
             | otherwise = ('A', Z)
дзT _ = ('A', Z)

жT :: [(Char, Triple)] -> (Char, Triple)
жT t1@(_:_) | takeFromFT 2 t1 `elem` ["sq","cq"] = ('R', T)
            | takeFromFT 1 t1 `elem` ["D","w"] = ('R', T)
            | otherwise = ('B', Z)
жT _ = ('B', Z)

зT :: [(Char, Triple)] -> (Char, Triple)
зT t1@(_:_) | takeFromFT_ 1 t1 `elem` ["B","E","F"] || takeFromFT_ 2 t1 == "dB" || takeFromFT_ 1 t1 == "j" = ('B', Z)
            | isSoftDOrL t1 = ('R', T)
            | takeFromFT 1 t1 `elem` ["E","F"] = ('F', Z) -- need further processing з ш
            | takeFromFT 1 t1  `elem` ["s","c"] || takeFromFT_ 1 t1 `elem` ["k","p","t","f","x"] = ('s', Z) -- need further processing з с
            | otherwise = ('z', Z)
зT _ = ('z', Z)

кT :: [(Char, Triple)] -> (Char, Triple)
кT t1@(_:_) | isVoicedObstruent .  takeFromFT_ 1 $ t1 = ('g', Z)
            | otherwise = ('k', Z)
кT _ = ('k', Z)

нтT :: [(Char, Triple)] -> (Char, Triple)
нтT t1@(_:_) | takeFromFT 2 t1 == "st" || takeFromFT 1 t1 == "O" = ('n', Z)
             | takeFromFT 3 t1 == "sqk" || takeFromFT 2 t1 == "Dk" = ('S', T)
             | otherwise = ('N', T)
нтT _ = ('N', T)

пT :: [(Char, Triple)] -> (Char, Triple)
пT t1@(_:_) | isVoicedObstruent .  takeFromFT_ 1 $ t1 = ('b', Z)
            | otherwise = ('p', Z)
пT _ = ('p', Z)

сT :: [(Char, Triple)] -> (Char, Triple)
сT t1@(_:_) | ((isVoicedObstruentH .  takeFromFT_ 1 $ t1) && B.drop 1 (takeFromFT_ 2 t1) == "q") || isVoicedObstruentS (takeFromFT_ 1 t1) = ('R', T)
            | isVoicedObstruentH .  takeFromFT_ 1 $ t1 = ('z', Z)
            | isSoftDOrL t1 = ('D', Z)
            | takeFromFT_ 1 t1 == "F" = ('F', Z)
            | otherwise = ('s', Z)
сT _ = ('s', Z)

стT :: [(Char, Triple)] -> (Char, Triple)
стT t1@(_:_) | isVoicedObstruent .  takeFromFT_ 1 $ t1  = ('z', Z)
             | takeFromFT_ 3 t1 == "sqk" || (takeFromFT_ 2 t1 `elem` ["Dk","cq"]) || takeFromFT_ 1 t1 == "w" = ('D', Z)
             | takeFromFT_ 1 t1 `elem` ["s","n"] = ('s', Z)
             | takeFromFT_ 1 t1 == "E" = ('F', Z)
             | otherwise = ('O', T)
стT _ = ('O', T)

сьT :: [(Char, Triple)] -> (Char, Triple)
сьT t1@(_:_) | isVoicedObstruent .  takeFromFT_ 1 $ t1 = ('R', T)
             | otherwise = ('D', Z)
сьT _ = ('D', Z)

тT :: [(Char, Triple)] -> (Char, Triple)
тT t1@(_:_) | ((isVoicedObstruentH .  takeFromFT_ 1 $ t1) && B.drop 1 (takeFromFT_ 2 t1) == "q") || isVoicedObstruentS (takeFromFT_ 1 t1) = ('T', T)
            | isVoicedObstruentH .  takeFromFT_ 1 $ t1 = ('d', Z)
            | takeFromFT_ 2 t1 == "cq" || takeFromFT_ 1 t1 == "w"  = ('w', Z)
            | takeFromFT_ 1 t1 == "c" = ('c', Z)
            | isSoftDen t1 = ('P', T)
            | takeFromFT_ 1 t1 `elem` ["E","F"] = ('E', Z)
            | otherwise = ('t', Z)
тT _ = ('t', Z)

тьT :: [(Char, Triple)] -> (Char, Triple)
тьT t1@(_:_) | isVoicedObstruent .  takeFromFT_ 1 $ t1 = ('T', T)
             | takeFromFT_ 3 t1 == "sqa" || takeFromFT_ 2 t1 == "Da" = ('w', Z)
             | otherwise = ('P', T)
тьT _ = ('P', T)

фT :: [(Char, Triple)] -> (Char, Triple)
фT t1@(_:_) | isVoicedObstruent .  takeFromFT_ 1 $ t1 = ('v', Z)
            | otherwise = ('f', Z)
фT _ = ('f', Z)

хT :: [(Char, Triple)] -> (Char, Triple)
хT t1@(_:_) | isVoicedObstruent .  takeFromFT_ 1 $ t1 = ('h', Z)
            | otherwise = ('x', Z)
хT _ = ('х', Z)

цT :: [(Char, Triple)] -> (Char, Triple)
цT t1@(_:_) | ((isVoicedObstruentH .  takeFromFT_ 1 $ t1) && B.drop 1 (takeFromFT_ 2 t1) == "q") || isVoicedObstruentS (takeFromFT_ 1 t1) = ('Q', T)
            | isSoftDOrL t1 = ('w', Z)
            | isVoicedObstruentH .  takeFromFT_ 1 $ t1 = ('A', Z)
            | otherwise = ('c', Z)
цT _ = ('c', Z)

цьT :: [(Char, Triple)] -> (Char, Triple)
цьT t1@(_:_) | (isVoicedObstruent .  takeFromFT_ 1 $ t1) && B.drop 1 (takeFromFT_ 2 t1) == "q" = ('Q', T)
             | otherwise = ('w', Z)
цьT _ = ('w', Z)

чT :: [(Char, Triple)] -> (Char, Triple)
чT t1@(_:_) | takeFromFT_ 2 t1 `elem` ["sq","cq"] || takeFromFT_ 1 t1 `elem` ["D","w"] = ('w', Z)
            | isVoicedObstruent .  takeFromFT_ 1 $ t1 = ('j', Z)
            | otherwise = ('E', Z)
чT _ = ('E', Z)

шT :: [(Char, Triple)] -> (Char, Triple)
шT t1@(_:_) | takeFromFT_ 2 t1 `elem` ["sq","cq"] || takeFromFT_ 1 t1 `elem` ["D","w"] = ('D', Z)
            | isVoicedObstruent .  takeFromFT_ 1 $ t1 = ('B', Z)
            | otherwise = ('F', Z)
шT _ = ('F', Z)

takeFromFT :: Int -> [(Char, Triple)] -> B.ByteString
takeFromFT n ts | if compare 0 n /= LT then True else null ts = B.empty
                | compare 1 n /= LT = B.singleton k
                | otherwise = k `B.cons` takeFromFT (n - 1) (take (n - 1) ts)
    where k = fst (head ts)

takeFromFT2 :: Int -> [Char] -> [Char]
takeFromFT2 n ts | if compare 0 n /= LT then True else null ts = []
                 | compare 1 n /= LT = [ks]
                 | otherwise = ks:takeFromFT2 (n - 1) (tail ts)
    where ks = head ts

dropFromFT2 :: Int -> [Char] -> [Char]
dropFromFT2 n ts | if compare 0 n /= LT then True else null ts = []
                 | compare 1 n /= LT = tail ts
                 | otherwise = dropFromFT2 (n - 1) (tail ts)

takeFromFT_ :: Int -> [(Char, Triple)] -> B.ByteString
takeFromFT_ n = takeFromFT n . filter (\(x, _) -> x /= '1' && x /= '0')

correctA :: [(Char, Triple)] -> [Char]
correctA = correctSomeW . separateSoftS

separateSoftS :: [(Char, Triple)] -> [Char]
separateSoftS = concatMap divideToParts

correctSomeW :: [Char] -> [Char]
correctSomeW (x:y:z:xs@(t:ys))
 | x == 't' && y == 'q' && z == 'D' && t == 'a' = 'w':'w':'a':correctSomeW ys
 | (x == '1' || x == '0') && y == 'C' && z == 'a' =
  if take 2 xs == "En"
    then x:y:z:'F':correctSomeW ys
    else x:correctSomeW (y:z:xs)
                        | otherwise = x:correctSomeW (y:z:xs)
correctSomeW zs = zs

divideToParts :: (Char, Triple) -> [Char]
divideToParts (x, z) = getBFstLSorted' [x] (zip "NOPQRST" ["nt", "st", "tq", "Aq", "zq", "nq", "dq"]) . fst $ (x, z)

correctB :: [Char] -> [Char]
correctB ys@(x:xs)
  | compare (length . filter (== '1') . takeFromFT2 6 $ ys) 1 == GT = map (\t -> if t == '1' || isPunctuation t then '-' else t) (takeFromFT2 6 ys) ++ correctB (dropFromFT2 6 ys)
  | otherwise = (if isPunctuation x then '-' else x):correctB xs
correctB _ = []

-- | A variant of the 'convertToProperUkrainian' with the 'B.ByteString' result.
convertToProperUkrainianB :: String -> B.ByteString
convertToProperUkrainianB = B.pack . convertToProperUkrainianS

linkFileName :: Char -> Char
linkFileName x = getBFstLSorted' x (zip "ABCDEFLMabcdefghijklmnopqrstuvwxyz" "GILUbc00ABZEHXfDeFMNOPQRdSTVWCaYKJ") x

showInteresting :: String -> B.ByteString
showInteresting = S.fromString . convertToProperUkrainianS
