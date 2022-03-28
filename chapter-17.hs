{-# LANGUAGE Arrows #-}

import Euterpea
import FRP.UISF
import HSoM
import Text.Read (readMaybe)

uiTest = setSize (150, 150) $ proc _ -> do
  ap <- title "Absolute pitch" $ hiSlider 1 (0, 100) 0 -< ()
  title "Pitch" display -< pitch ap

uiOutputAbsPitch = proc _ -> do
  devId <- selectOutput -< ()
  ap <- title "Absolute pitch" $ hiSlider 1 (0, 100) 0 -< ()
  title "Pitch" display -< pitch ap
  uniqueAp <- unique -< ap
  midiOut -< (devId, fmap (\k -> [ANote 0 k 100 0.1]) uniqueAp)

muiWithTitle =
  runMUI
    ( defaultMUIParams
        { uiTitle = "MIDI Input / Output UI",
          uiSize = (600, 200)
        }
    )
    uiOutputAbsPitch

uiContinuousNote = proc _ -> do
  devId <- selectOutput -< ()
  ap <- title "Absolute pitch" $ hiSlider 1 (0, 100) 0 -< ()
  title "Pitch" display -< pitch ap
  freq <- title "Tempo" $ hSlider (1, 10) 1 -< ()
  tick <- timer -< 1 / freq
  midiOut -< (devId, fmap (const [ANote 0 ap 100 0.1]) tick)

iterateLogisticGrowth :: Double -> Double -> Double
iterateLogisticGrowth r n = r * n * (1 - n)

convertPopulationToNote :: Double -> [MidiMessage]
convertPopulationToNote x = [ANote 0 n 64 0.05]
  where
    n = truncate $ x * 127

bifurcateUI = proc _ -> do
  devId <- selectOutput -< ()
  freq <- title "Tempo" $ hSlider (1, 10) 1 -< ()
  tick <- timer -< 1 / freq
  r <- title "Growth Rate" $ withDisplay $ hSlider (2.4, 4.0) 2.4 -< ()
  population <- accum 0.1 -< fmap (const $ iterateLogisticGrowth r) tick
  _ <- title "Population" display -< population
  midiOut -< (devId, fmap (const $ convertPopulationToNote population) tick)

buttonLayoutUI = proc _ -> do
  b1 <- button "Button 1" -< ()
  (b2, b3) <-
    (|
      leftRight
        ( do
            b2 <- button "Button 2" -< ()
            title "b1" display -< b1
            b3 <- button "Button 3" -< ()
            returnA -< (b2, b3)
        )
    |)
  b4 <- button "Button 4" -< ()
  spacer -< ()
  title "Any Button is Pressed" display -< b1 || b2 || b3 || b4

-- Exercise 17.1
pitchStringToANote :: String -> SEvent [MidiMessage]
pitchStringToANote str = case ap of
  Just a -> Just [ANote 0 a 100 0.1]
  Nothing -> Nothing
  where
    parsedPitchString :: Maybe Pitch
    parsedPitchString = readMaybe str
    ap = fmap absPitch parsedPitchString

typePitchUI = proc _ -> do
  devId <- selectOutput -< ()
  pitchStr <- title "Pitch" $ textbox "(C,4)" -< Nothing
  playBtnPressed <- button "Play" -< ()
  shouldPlayNote <- unique -< playBtnPressed
  midiOut
    -<
      ( devId,
        case shouldPlayNote of
          Just a -> if a then Nothing else pitchStringToANote pitchStr
          Nothing -> Nothing
      )

-- Exercise 17.3
typePitchSliderUI = proc _ -> do
  devId <- selectOutput -< ()
  pitchStr <- title "Pitch" $ textbox "(C,4)" -< Nothing
  freq <- title "Tempo" $ withDisplay $ hSlider (1, 10) 1 -< ()
  tick <- timer -< 1 / freq
  midiOut
    -<
      ( devId,
        case tick of
          Just _ -> pitchStringToANote pitchStr
          Nothing -> Nothing
      )

-- Exercise 17.4
chromaticScale :: [PitchClass]
chromaticScale = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]

pseudoKeyboardUI =
  let chromaticScale = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
   in proc _ -> do
        devId <- selectOutput -< ()
        pc <- radio (map show chromaticScale) 1 -< ()
        uniquePc <- unique -< pc
        midiOut -< (devId, fmap (\pc -> [ANote 0 (absPitch (chromaticScale !! pc, 4)) 100 0.1]) uniquePc)

-- Exercise 17.5
pseudoKeyboardOctaveUI =
  let chromaticScale = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
   in proc _ -> do
        devId <- selectOutput -< ()
        pc <- radio (map show chromaticScale) 1 -< ()
        uniquePc <- unique -< pc
        octave <- title "Octave" $ withDisplay $ hiSlider 1 (1, 10) 4 -< ()
        midiOut -< (devId, fmap (\pc -> [ANote 0 (absPitch (chromaticScale !! pc, octave)) 100 0.1]) uniquePc)

-- End exercise

-- On mac the UI doesn't work in the interactive terminal, so must compile to binary first
main =
  runMUI
    ( defaultMUIParams
        { uiSize = (300, 400)
        }
    )
    pseudoKeyboardOctaveUI