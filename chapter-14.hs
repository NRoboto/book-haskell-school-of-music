import Data.Ratio (approxRational, (%))
import Euterpea
import System.Random
import System.Random.Distributions

sGen :: StdGen
sGen = mkStdGen 42

randFloats :: [Float]
randFloats = randomRs (-1, 1) sGen

randIntegers :: [Integer]
randIntegers = randomRs (-100, 100) sGen

randIO :: IO Float
randIO = randomRIO (0, 1)

convertFloatToAbsPitch :: Float -> AbsPitch
convertFloatToAbsPitch x = round $ 40 * x + 30

mkNote1 :: AbsPitch -> Music Pitch
mkNote1 = note tn . pitch

mkLine1 :: [AbsPitch] -> Music Pitch
mkLine1 rands = line $ take 32 $ map mkNote1 rands

m1 :: Music Pitch
m1 = mkLine1 $ randomRs (30, 70) sGen

m2 :: Music Pitch
m2 =
  let rs1 = rands linear sGen
   in mkLine1 $ map convertFloatToAbsPitch rs1

m3 :: Float -> Music Pitch
m3 lam =
  let rs1 = rands (exponential lam) sGen
   in mkLine1 $ map convertFloatToAbsPitch rs1

m4 :: Float -> Float -> Music Pitch
m4 sig mu =
  let rs1 = rands (gaussian sig mu) sGen
   in mkLine1 $ map convertFloatToAbsPitch rs1

-- Exercise 14.2
convertFloatToDuration :: Float -> Dur
convertFloatToDuration x = approxRational x 0.01

makeNoteFromDur :: Dur -> Music Pitch
makeNoteFromDur = flip note (C, 4)

randDurationMusic :: Float -> Float -> Music Pitch
randDurationMusic stdDev mean = line $ take 32 notes
  where
    notes = map (makeNoteFromDur . convertFloatToDuration) randNums
    randNums = rands (gaussian stdDev mean) sGen

-- End exercise

convertFloatToAbsPitch2 :: Float -> AbsPitch
convertFloatToAbsPitch2 x = round (5 * x)

mkLine2 :: AbsPitch -> [AbsPitch] -> Music Pitch
mkLine2 start rands = line $ take 64 $ map mkNote1 (scanl (+) start rands)

m5 :: Float -> Music Pitch
m5 sig =
  let rs1 = rands (gaussian sig 0) sGen
   in mkLine2 50 $ map convertFloatToAbsPitch2 rs1

m6 :: Float -> Music Pitch
m6 lam =
  let rs1 = rands (exponential lam) sGen
   in mkLine2 50 $ map (convertFloatToAbsPitch2 . subtract (1 / lam)) rs1