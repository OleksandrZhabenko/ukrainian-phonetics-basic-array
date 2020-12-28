{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

-- |
-- Module      :  Languages.Phonetic.Ukrainian.Syllable.Arr
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- This module works with syllable segmentation in Ukrainian. It is rewritten
-- module MMSyn7.Syllable from the @mmsyn7s@ package : https://hackage.haskell.org/package/mmsyn7s
--

module Languages.Phonetic.Ukrainian.Syllable.Arr where

import Prelude hiding (mappend)
import Data.Monoid
import Data.Typeable
import qualified Data.List as L (groupBy)
import Melodics.ByteString.Ukrainian.Arr
import CaseBi.Arr
import Data.List.InnToOut.Basic (mapI)
import Data.Maybe (mapMaybe)

-- Inspired by: https://github.com/OleksandrZhabenko/mm1/releases/tag/0.2.0.0

-- CAUTION: Please, do not mix with the show7s functions, they are not interoperable.

data UZPP a b = UZ a b deriving ( Eq, Typeable )

instance (Ord a, Ord b) => Ord (UZPP a b) where
  compare (UZ x1 y1) (UZ x2 y2) =
    case compare x1 x2 of
      EQ -> compare y1 y2
      ~z -> z

data PhoneticType = W | S | O | D | K | L | M | N | E deriving ( Eq, Ord, Typeable )

type UZPP2 = UZPP Char PhoneticType

instance Show (UZPP Char PhoneticType) where
  show (UZ x y)
   | y `notElem` [O,K,M] =
       getBFstLSorted' "" [('-'," "),('0'," "),('1'," "),('A',"дз"),('B',"ж"),('C',"й"),('D',"сь"),('E',"ч"),('F',"ш"),('G',"щ"),('L',"\700"),('M',"\8217"),
        ('a',"а"),('b',"б"),('c',"ц"),('d',"д"),('e',"е"),('f',"ф"),('g',"ґ"),('h',"г"),('i',"і"),('j',"дж"),('k',"к"),('l',"л"),('m',"м"),('n',"н"),('o',"о"),('p',"п"),('q',"ь"),
          ('r',"р"),('s',"с"),('t',"т"),('u',"у"),('v',"в"),('w',"ць"),('x',"х"),('y',"и"),('z',"з")] x
   | otherwise =
       getBFstLSorted' "" [('-'," "),('0'," "),('1'," "),('A',"дзь"),('B',"жь"),('E',"чь"),('F',"шь"),('G',"щь"),('b',"бь"),('d',"дь"),('f',"фь"),('g',"ґь"),
        ('h',"гь"),('j',"джь"),('k',"кь"),('l',"ль"),('m',"мь"),('n',"нь"),('p',"пь"),('q',"ь"),('r',"рь"),('t',"ть"),('v',"вь"),('x',"хь"),('z',"зь")] x

phoneType :: UZPP2 -> PhoneticType
phoneType (UZ _ y) = y
{-# INLINE phoneType #-}

charUkr :: UZPP2 -> Char
charUkr (UZ x _) = x
{-# INLINE charUkr #-}

str2UZPP2s :: String -> [UZPP2]
str2UZPP2s (y:ys)
  | getBFstLSorted' False [('a',True),('e',True),('i',True),('o',True),('u',True),('y',True)] y = UZ y W:str2UZPP2s ys
  | y == 'D' || y == 'w' = UZ y N:str2UZPP2s ys
  | (null ys || head ys /= 'q') && getBFstLSorted' False [('C',True),('l',True),('m',True),('n',True),('r',True),('v',True)] y = UZ y S:str2UZPP2s ys
  | (null ys || head ys /= 'q') &&
      getBFstLSorted' False [('A',True),('B',True),('b',True),('d',True),('g',True),('h',True),('j',True),('z',True)] y = UZ y D:str2UZPP2s ys
  | (null ys || head ys /= 'q') = UZ y L:str2UZPP2s ys
  | getBFstLSorted' False [('l',True),('m',True),('n',True),('r',True),('v',True)] y = UZ y O:str2UZPP2s (drop 1 ys)
  | getBFstLSorted' False [('A',True),('B',True),('b',True),('d',True),('g',True),('h',True),('j',True),('z',True)] y =
      UZ y K:str2UZPP2s (drop 1 ys)
  | otherwise = UZ y M:str2UZPP2s (drop 1 ys)
str2UZPP2s _ = []

-- | Function-predicate 'isVowel1' checks whether its argument is a vowel representation in the 'UZPP2' format.
isVowel1 :: UZPP2 -> Bool
isVowel1 = (== W) . phoneType
{-# INLINE isVowel1 #-}

-- | Function-predicate 'isVwl' checks whether its argument is a vowel representation in the 'Char' format.
isVwl :: Char -> Bool
isVwl = getBFstLSorted' False [('a',True),('e',True),('i',True),('o',True),('u',True),('y',True)]
{-# INLINE isVwl #-}

-- | Function-predicate 'isSonorous1' checks whether its argument is a sonorous consonant representation in the 'UZPP2' format.
isSonorous1 :: UZPP2 -> Bool
isSonorous1 =  (`elem` [S,O]) . phoneType
{-# INLINE isSonorous1 #-}

-- | Function-predicate 'isVoicedC1' checks whether its argument is a voiced consonant representation in the 'UZPP2' format.
isVoicedC1 ::  UZPP2 -> Bool
isVoicedC1 = (`elem` [D,K]) . phoneType
{-# INLINE isVoicedC1 #-}

-- | Function-predicate 'isVoiceless1' checks whether its argument is a voiceless consonant representation in the 'UZPP2' format.
isVoicelessC1 ::  UZPP2 -> Bool
isVoicelessC1 =  (`elem` [L,M]) . phoneType
{-# INLINE isVoicelessC1 #-}

-- | Binary function-predicate 'isNotVowel2' checks whether its arguments are both consonant representations in the 'UZPP2' format.
isNotVowel2 :: UZPP2 -> UZPP2 -> Bool
isNotVowel2 x y
  | phoneType x == W || phoneType y == W = False
  | otherwise = True
{-# INLINE isNotVowel2 #-}

-- | Binary function-predicate 'notEqC' checks whether its arguments are not the same consonant sound representations (not taking palatalization into account).
notEqC :: UZPP2 -> UZPP2 -> Bool
notEqC x y
  | charUkr x == 's' || charUkr x == 'D' =
      case charUkr y of
        's' -> False
        'D' -> False
        _   -> True
  | charUkr x == 'w' || charUkr x == 'c' =
      case charUkr y of
        'w' -> False
        'c' -> False
        _   -> True
  | otherwise = charUkr x /= charUkr y

-- | Function 'sndGroups' converts a Ukrainian word being a list of 'UZPP2' to the list of phonetically similar (consonants grouped with consonants and each vowel separately)
-- sounds representations in 'UZPP2' format.
sndGroups :: [UZPP2] -> [[UZPP2]]
sndGroups ys@(_:_) = L.groupBy isNotVowel2 ys
sndGroups _ = []

groupSnds :: [UZPP2] -> [[UZPP2]]
groupSnds = L.groupBy (\x y -> ((== W) . phoneType $ x) == ((== W) . phoneType $ y))

-- | Function 'divCnsnts' is used to divide groups of Ukrainian consonants into two-elements lists that later are made belonging to
-- different neighbour syllables if the group is between two vowels in a word. The group must be not empty, but this is not checked.
-- The phonetical information for the proper performance is taken from the:
-- https://msn.khnu.km.ua/pluginfile.php/302375/mod_resource/content/1/%D0%9B.3.%D0%86%D0%86.%20%D0%A1%D0%BA%D0%BB%D0%B0%D0%B4.%D0%9D%D0%B0%D0%B3%D0%BE%D0%BB%D0%BE%D1%81.pdf
divCnsnts :: [UZPP2] -> ([UZPP2] -> [UZPP2],[UZPP2] -> [UZPP2])
divCnsnts xs@(x:ys@(_:_:_:_))
  | (isSonorous1 x) || (isVoicedC1 x) = ((`mappend` [x]),(ys `mappend`))
  | otherwise = ((id),(xs `mappend`))
divCnsnts xs@(x:ys@(y:zs@(_:_)))
  | isSonorous1 x = ((`mappend` [x]),(ys `mappend`))
  | isSonorous1 y = ((`mappend` [x,y]),(zs `mappend`))
  | otherwise = ((id),(xs `mappend`))
divCnsnts xs@(x:ys@(y:_))
  | ((isSonorous1 x) && (x `notEqC` y)) || ((isVoicedC1 x) && (isVoicelessC1 y)) = ((`mappend` [x]),(ys `mappend`))
  | otherwise = ((id),(xs `mappend`))
divCnsnts xs = ((id),(xs `mappend`))

reSyllableCntnts :: [[UZPP2]] -> [[UZPP2]]
reSyllableCntnts (xs:ys:zs:xss)
  | (/= W) . phoneType . last $ ys = fst (divCnsnts ys) xs:reSyllableCntnts (snd (divCnsnts ys) zs:xss)
  | otherwise = reSyllableCntnts ((xs `mappend` ys):zs:xss)
reSyllableCntnts (xs:ys:_) = [(xs `mappend` ys)]
reSyllableCntnts xss = xss

divVwls :: [[UZPP2]] -> [[UZPP2]]
divVwls = mapI (\ws -> (length . filter ((== W) . phoneType) $ ws) > 1) h3
  where h3 us = [ys `mappend` take 1 zs] `mappend` (L.groupBy (\x y -> phoneType x == W && phoneType y /= W) . drop 1 $ zs)
                  where (ys,zs) = span (\t -> phoneType t /= W) us

createSyllablesUkrS :: String -> [[[UZPP2]]]
createSyllablesUkrS = map (divVwls . reSyllableCntnts . groupSnds . str2UZPP2s) . words1 . mapMaybe g . convertToProperUkrainianS . map (\x -> if x == '-' then ' ' else x)
  where g x
          | x == '0' = Nothing
          | x /= '1' && x /= '-' = Just x
          | otherwise = Just ' '
        words1 xs = if null ts then [] else w : words1 s'' -- Practically this is an optimized version for this case 'words' function from Prelude.
          where ts = dropWhile (== ' ') xs
                (w, s'') = span (/= ' ') ts
        {-# NOINLINE words1 #-}
{-# INLINE createSyllablesUkrS #-}

-- | Function 'representProlonged' converts duplicated consequent in the syllable consonants
-- so that they are represented by just one 'UZPP2'. After applying the function to the list of 'UZPP2' being a syllable all groups of duplicated consequent consonants
-- in every syllable are represented with only one 'UZPP2' respectively.
representProlonged :: [UZPP2] -> [UZPP2]
representProlonged (x:y:xs)
  | isVowel1 x = x:representProlonged (y:xs)
  | not . notEqC x $ y = y:representProlonged xs
  | otherwise = x:representProlonged (y:xs)
representProlonged xs = xs

-- | Is inspired by the DobutokO.Sound.DIS5G6G module from @dobutokO2@ package.
-- See: 'https://hackage.haskell.org/package/dobutokO2-0.43.0.0/docs/DobutokO-Sound-DIS5G6G.html'. The 'Float' data are gotten from there.
str2Durat1 :: String -> Float
str2Durat1 = getBFstLSorted' 0.153016 [("-", (0.101995)), ("0", (0.051020)), ("1", (0.153016)), ("а", 0.138231), ("б", 0.057143),
  ("в", 0.082268), ("г", 0.076825), ("д", 0.072063), ("дж", 0.048934), ("дз", 0.055601), ("е", 0.093605), ("ж", 0.070658), ("з", 0.056054),
    ("и", 0.099955), ("й", 0.057143), ("к", 0.045351), ("л", 0.064036), ("м", 0.077370), ("н", 0.074240), ("о", 0.116463), ("п", 0.134830),
      ("р", 0.049206), ("с", 0.074603), ("сь", 0.074558), ("т", 0.110658), ("у", 0.109070), ("ф", 0.062268), ("х", 0.077188), ("ц", 0.053061),
        ("ць", 0.089342), ("ч", 0.057596), ("ш", 0.066077), ("ь", 0.020227), ("і", 0.094150), ("ґ", 0.062948)]

-- | Just another possible duration approximation obtained by usage of the @r-glpk-phonetic-languages-ukrainian-durations@ package
-- https://hackage.haskell.org/package/r-glpk-phonetic-languages-ukrainian-durations.
-- It is generated for the set of the words-durations pairs that the words contents ('Char') converts to the elements of the
-- \"ABCEFXYabcdefghijklmnopqrstuvxyz\" (for more information, pleas, refer to the
-- https://hackage.haskell.org/package/r-glpk-phonetic-languages-ukrainian-durations).
uzpp2Durat2 :: UZPP2 -> Float
uzpp2Durat2 = getBFstLSorted' 0.06408817 [(UZ 'A' D, 0.07729654), (UZ 'A' K, 0.07729654), (UZ 'B' D, 0.08048113), (UZ 'B' K, 0.08048113),
  (UZ 'C' S, 0.08226452), (UZ 'D' N, 0.07512999), (UZ 'E' L, 0.12541547), (UZ 'E' M, 0.12541547), (UZ 'F' L, 0.12838476), (UZ 'F' M, 0.12838476),
    (UZ 'a' W, 0.27161466), (UZ 'b' D, 0.10977617), (UZ 'b' K, 0.10977617), (UZ 'c' D, 0.05616409), (UZ 'd' D, 0.06586550), (UZ 'd' K, 0.06586550),
      (UZ 'e' W, 0.27192511), (UZ 'f' L, 0.15776219), (UZ 'f' M, 0.15776219), (UZ 'g' D, 0.07751571), (UZ 'g' K, 0.07751571), (UZ 'h' D, 0.05392745),
        (UZ 'h' K, 0.05392745), (UZ 'i' W, 0.20026538), (UZ 'j' D, 0.08900757), (UZ 'j' K, 0.08900757), (UZ 'k' L, 0.04917820), (UZ 'k' M, 0.04917820),
          (UZ 'l' S, 0.11159399), (UZ 'l' O, 0.11159399), (UZ 'm' S, 0.14303837), (UZ 'm' O, 0.14303837), (UZ 'n' S, 0.05639178),
            (UZ 'n' O, 0.05639178), (UZ 'o' W, 0.28539351), (UZ 'p' L, 0.09603085), (UZ 'p' M, 0.09603085), (UZ 'q' E, 0.02218624), (UZ 'r' S, 0.06354637),
              (UZ 'r' O, 0.06354637), (UZ 's' L, 0.05294375), (UZ 't' L, 0.05047358), (UZ 't' M, 0.05047358), (UZ 'u' W, 0.25250039),
                (UZ 'v' S, 0.08404524), (UZ 'v' O, 0.08404524), (UZ 'w' N, 0.07835033), (UZ 'x' L, 0.07905155), (UZ 'x' M, 0.07905155),
                  (UZ 'y' W, 0.20509350), (UZ 'z' D, 0.06099951), (UZ 'z' K, 0.06099951)]

uzpp2Durat1 :: UZPP2 -> Float
uzpp2Durat1 = getBFstLSorted' 0.051020 [(UZ 'A' D, 0.055601), (UZ 'A' K, 0.055601), (UZ 'B' D, 0.070658), (UZ 'B' K, 0.070658), (UZ 'C' S, 0.057143), (UZ 'D' N, 0.074558),
  (UZ 'E' L, 0.057596), (UZ 'E' M, 0.057596), (UZ 'F' L, 0.066077), (UZ 'F' M, 0.066077), (UZ 'a' W, 0.138231), (UZ 'b' D, 0.057143), (UZ 'b' K, 0.057143), (UZ 'c' D, 0.053061),
   (UZ 'd' D, 0.072063), (UZ 'd' K, 0.072063), (UZ 'e' W, 0.093605), (UZ 'f' L, 0.062268), (UZ 'f' M, 0.062268),  (UZ 'g' D, 0.062948), (UZ 'g' K, 0.062948), (UZ 'h' D, 0.076825),
    (UZ 'h' K, 0.076825), (UZ 'i' W, 0.094150), (UZ 'j' D, 0.048934), (UZ 'j' K, 0.048934), (UZ 'k' L, 0.045351), (UZ 'k' M, 0.045351), (UZ 'l' S, 0.064036), (UZ 'l' O, 0.064036),
     (UZ 'm' S, 0.077370), (UZ 'm' O, 0.077370), (UZ 'n' S, 0.074240), (UZ 'n' O, 0.074240), (UZ 'o' W, 0.116463), (UZ 'p' L, 0.134830), (UZ 'p' M, 0.134830),
      (UZ 'q' E, 0.020227), (UZ 'r' S, 0.049206), (UZ 'r' O, 0.049206), (UZ 's' L, 0.074603),  (UZ 't' L, 0.110658), (UZ 't' M, 0.110658), (UZ 'u' W, 0.109070), (UZ 'v' S, 0.082268),
       (UZ 'v' O, 0.082268), (UZ 'w' N, 0.089342), (UZ 'x' L, 0.077188), (UZ 'x' M, 0.077188), (UZ 'y' W, 0.099955), (UZ 'z' D, 0.056054), (UZ 'z' K, 0.056054)]

uzpp2Durat3 :: UZPP2 -> Float
uzpp2Durat3 = getBFstLSorted' 0.05779993 [(UZ 'A' D, 0.08453724), (UZ 'A' K, 0.08453724),
 (UZ 'B' D, 0.09996042), (UZ 'B' K, 0.09996042), (UZ 'C' S, 0.10975353), (UZ 'D' N, 0.08190674),
  (UZ 'E' L, 0.11906522), (UZ 'E' M, 0.11906522), (UZ 'F' L, 0.13985258), (UZ 'F' M, 0.13985258),
   (UZ 'a' W, 0.25872483), (UZ 'b' D, 0.13787716), (UZ 'b' K, 0.13787716), (UZ 'c' D, 0.05901357),
    (UZ 'd' D, 0.07437409), (UZ 'd' K, 0.07437409), (UZ 'e' W, 0.22876537), (UZ 'f' L, 0.15880087),
     (UZ 'f' M, 0.15880087), (UZ 'g' D, 0.07985903), (UZ 'g' K, 0.07985903), (UZ 'h' D, 0.10289067),
      (UZ 'h' K, 0.10289067), (UZ 'i' W, 0.19777405), (UZ 'j' D, 0.10039843), (UZ 'j' K, 0.10039843),
       (UZ 'k' L, 0.05893304), (UZ 'k' M, 0.05893304), (UZ 'l' S, 0.10906450), (UZ 'l' O, 0.10906450),
        (UZ 'm' S, 0.14576594), (UZ 'm' O, 0.14576594), (UZ 'n' S, 0.06084464), (UZ 'n' O, 0.06084464),
         (UZ 'o' W, 0.25423777), (UZ 'p' L, 0.10765654), (UZ 'p' M, 0.10765654), (UZ 'q' E, 0.01943042),
          (UZ 'r' S, 0.05937718), (UZ 'r' O, 0.05937718), (UZ 's' L, 0.06247632), (UZ 't' L, 0.06039120),
           (UZ 't' M, 0.06039120), (UZ 'u' W, 0.20243791), (UZ 'v' S, 0.07798724), (UZ 'v' O, 0.07798724),
            (UZ 'w' N, 0.07844400), (UZ 'x' L, 0.13526622), (UZ 'x' M, 0.13526622), (UZ 'y' W, 0.19849003),
             (UZ 'z' D, 0.06643842), (UZ 'z' K, 0.06643842)]

-- | General variant of the 'syllableDurations' function with the arbitrary 'uzpp2Durat1'-like function.
syllableDurationsG :: (UZPP2 -> Float) -> [[[UZPP2]]] -> [[Float]]
syllableDurationsG g = fmap (fmap (sum . fmap g))
{-# INLINABLE syllableDurationsG #-}

-- | Returns list of lists, every inner one of which contains approximate durations of the Ukrainian syllables.
syllableDurations :: [[[UZPP2]]] -> [[Float]]
syllableDurations = syllableDurationsG uzpp2Durat1

-- | Likewise 'syllableDurations', but uses 'uzpp2Durat2' instead of 'uzpp2Durat1'.
syllableDurations2 :: [[[UZPP2]]] -> [[Float]]
syllableDurations2 = syllableDurationsG uzpp2Durat2

-- | Likewise 'syllableDurations', but uses 'uzpp2Durat3' instead of 'uzpp2Durat1'.
syllableDurations3 :: [[[UZPP2]]] -> [[Float]]
syllableDurations3 = syllableDurationsG uzpp2Durat3
