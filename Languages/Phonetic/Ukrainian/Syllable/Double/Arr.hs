-- |
-- Module      :  Languages.Phonetic.Ukrainian.Syllable.Double.Arr
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- This module works with syllable segmentation in Ukrainian. Uses 'Double' whenever possible.
--

module Languages.Phonetic.Ukrainian.Syllable.Double.Arr where

import CaseBi.Arr
import Languages.Phonetic.Ukrainian.Syllable.Arr (UZPP(..),UZPP2,PhoneticType(..))

-- | Is inspired by the DobutokO.Sound.DIS5G6G module from @dobutokO2@ package.
-- See: 'https://hackage.haskell.org/package/dobutokO2-0.43.0.0/docs/DobutokO-Sound-DIS5G6G.html'. The 'Double' data are gotten from there.
str2DuratD1 :: String -> Double
str2DuratD1 = getBFstLSorted' 0.153016 [("-", (0.101995)), ("0", (0.051020)), ("1", (0.153016)), ("а", 0.138231), ("б", 0.057143),
  ("в", 0.082268), ("г", 0.076825), ("д", 0.072063), ("дж", 0.048934), ("дз", 0.055601), ("е", 0.093605), ("ж", 0.070658), ("з", 0.056054),
    ("и", 0.099955), ("й", 0.057143), ("к", 0.045351), ("л", 0.064036), ("м", 0.077370), ("н", 0.074240), ("о", 0.116463), ("п", 0.134830),
      ("р", 0.049206), ("с", 0.074603), ("сь", 0.074558), ("т", 0.110658), ("у", 0.109070), ("ф", 0.062268), ("х", 0.077188), ("ц", 0.053061),
        ("ць", 0.089342), ("ч", 0.057596), ("ш", 0.066077), ("ь", 0.020227), ("і", 0.094150), ("ґ", 0.062948)]

-- | Just another possible duration approximation obtained by usage of the @r-glpk-phonetic-languages-ukrainian-durations@ package
-- https://hackage.haskell.org/package/r-glpk-phonetic-languages-ukrainian-durations.
-- It is generated for the set of the words-durations pairs that the words contents ('Char') converts to the elements of the
-- \"ABCEFXYabcdefghijklmnopqrstuvxyz\" (for more information, pleas, refer to the
-- https://hackage.haskell.org/package/r-glpk-phonetic-languages-ukrainian-durations).
uzpp2DuratD2 :: UZPP2 -> Double
uzpp2DuratD2 = getBFstLSorted' 0.06408817 [(UZ 'A' D, 0.07729654), (UZ 'A' K, 0.07729654), (UZ 'B' D, 0.08048113), (UZ 'B' K, 0.08048113),
  (UZ 'C' S, 0.08226452), (UZ 'D' N, 0.07512999), (UZ 'E' L, 0.12541547), (UZ 'E' M, 0.12541547), (UZ 'F' L, 0.12838476), (UZ 'F' M, 0.12838476),
    (UZ 'a' W, 0.27161466), (UZ 'b' D, 0.10977617), (UZ 'b' K, 0.10977617), (UZ 'c' D, 0.05616409), (UZ 'd' D, 0.06586550), (UZ 'd' K, 0.06586550),
      (UZ 'e' W, 0.27192511), (UZ 'f' L, 0.15776219), (UZ 'f' M, 0.15776219), (UZ 'g' D, 0.07751571), (UZ 'g' K, 0.07751571), (UZ 'h' D, 0.05392745),
        (UZ 'h' K, 0.05392745), (UZ 'i' W, 0.20026538), (UZ 'j' D, 0.08900757), (UZ 'j' K, 0.08900757), (UZ 'k' L, 0.04917820), (UZ 'k' M, 0.04917820),
          (UZ 'l' S, 0.11159399), (UZ 'l' O, 0.11159399), (UZ 'm' S, 0.14303837), (UZ 'm' O, 0.14303837), (UZ 'n' S, 0.05639178),
            (UZ 'n' O, 0.05639178), (UZ 'o' W, 0.28539351), (UZ 'p' L, 0.09603085), (UZ 'p' M, 0.09603085), (UZ 'q' E, 0.02218624), (UZ 'r' S, 0.06354637),
              (UZ 'r' O, 0.06354637), (UZ 's' L, 0.05294375), (UZ 't' L, 0.05047358), (UZ 't' M, 0.05047358), (UZ 'u' W, 0.25250039),
                (UZ 'v' S, 0.08404524), (UZ 'v' O, 0.08404524), (UZ 'w' N, 0.07835033), (UZ 'x' L, 0.07905155), (UZ 'x' M, 0.07905155),
                  (UZ 'y' W, 0.20509350), (UZ 'z' D, 0.06099951), (UZ 'z' K, 0.06099951)]

uzpp2DuratD1 :: UZPP2 -> Double
uzpp2DuratD1 = getBFstLSorted' 0.051020 [(UZ 'A' D, 0.055601), (UZ 'A' K, 0.055601), (UZ 'B' D, 0.070658), (UZ 'B' K, 0.070658), (UZ 'C' S, 0.057143), (UZ 'D' N, 0.074558),
  (UZ 'E' L, 0.057596), (UZ 'E' M, 0.057596), (UZ 'F' L, 0.066077), (UZ 'F' M, 0.066077), (UZ 'a' W, 0.138231), (UZ 'b' D, 0.057143), (UZ 'b' K, 0.057143), (UZ 'c' D, 0.053061),
   (UZ 'd' D, 0.072063), (UZ 'd' K, 0.072063), (UZ 'e' W, 0.093605), (UZ 'f' L, 0.062268), (UZ 'f' M, 0.062268),  (UZ 'g' D, 0.062948), (UZ 'g' K, 0.062948), (UZ 'h' D, 0.076825),
    (UZ 'h' K, 0.076825), (UZ 'i' W, 0.094150), (UZ 'j' D, 0.048934), (UZ 'j' K, 0.048934), (UZ 'k' L, 0.045351), (UZ 'k' M, 0.045351), (UZ 'l' S, 0.064036), (UZ 'l' O, 0.064036),
     (UZ 'm' S, 0.077370), (UZ 'm' O, 0.077370), (UZ 'n' S, 0.074240), (UZ 'n' O, 0.074240), (UZ 'o' W, 0.116463), (UZ 'p' L, 0.134830), (UZ 'p' M, 0.134830),
      (UZ 'q' E, 0.020227), (UZ 'r' S, 0.049206), (UZ 'r' O, 0.049206), (UZ 's' L, 0.074603),  (UZ 't' L, 0.110658), (UZ 't' M, 0.110658), (UZ 'u' W, 0.109070), (UZ 'v' S, 0.082268),
       (UZ 'v' O, 0.082268), (UZ 'w' N, 0.089342), (UZ 'x' L, 0.077188), (UZ 'x' M, 0.077188), (UZ 'y' W, 0.099955), (UZ 'z' D, 0.056054), (UZ 'z' K, 0.056054)]

uzpp2DuratD3 :: UZPP2 -> Double
uzpp2DuratD3 = getBFstLSorted' 0.05779993 [(UZ 'A' D, 0.08453724), (UZ 'A' K, 0.08453724),
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

-- | General variant of the 'syllableDurationsD' function with the arbitrary 'uzpp2DuratD1'-like function.
syllableDurationsGD :: (UZPP2 -> Double) -> [[[UZPP2]]] -> [[Double]]
syllableDurationsGD g = fmap (fmap (sum . fmap g))
{-# INLINABLE syllableDurationsGD #-}

-- | Returns list of lists, every inner one of which contains approximate durations of the Ukrainian syllables.
syllableDurationsD :: [[[UZPP2]]] -> [[Double]]
syllableDurationsD = syllableDurationsGD uzpp2DuratD1

-- | Likewise 'syllableDurations', but uses 'uzpp2DuratD2' instead of 'uzpp2DuratD1'.
syllableDurationsD2 :: [[[UZPP2]]] -> [[Double]]
syllableDurationsD2 = syllableDurationsGD uzpp2DuratD2

-- | Likewise 'syllableDurations', but uses 'uzpp2DuratD3' instead of 'uzpp2DuratD1'.
syllableDurationsD3 :: [[[UZPP2]]] -> [[Double]]
syllableDurationsD3 = syllableDurationsGD uzpp2DuratD3
