{-# LANGUAGE NoMonomorphismRestriction #-}
module Music.Instrument.Guitar where

import Music.Diatonic
import Music.Diatonic.Note
import Music.Diatonic.Degree
import Music.Diatonic.Chord
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import qualified Data.Set

import Music.Instrument.Piano
import Music.Instrument.Common

findPositionPatterns chord tuning count = filter ( not . null  ) $ findPositionPatterns' chord tuning count
        
findPositionPatterns' chord tuning count =
  scanl1 (flip (\\)) ( map (\x-> findPositionPatterns'' chord tuning x count) [0..])
        
findPositionPatterns'' chord tuning from count = 
  map ( map ( (+from) . fromJust) . map (uncurry (flip elemIndex))) $ map (zipWith (,) (frettedGuitarStringsLengths from count tuning)) (notePatterns chord tuning from count)

notePatterns chord tuning from count = 
  mapM (filter (flip elem (chordToNotes chord))) (frettedGuitarStringsLengths from count tuning)

frettedGuitarStringsLengths from count = map (take count . drop from) . frettedGuitarStrings
frettedGuitarStrings tuning = map fret tuning
fret tune = map (\n -> canonize . applyNTimes sharp n $ tune) [0..]


positionsAndTuningToNotes tuning positions = zipWith tuneAndPositionToNote tuning positions
tuneAndPositionToNote tune position =  fret tune !! position

getPositionPatternRange = liftM2 (,) getPositionPatternMin getPositionPatternMax

getPositionPatternMin = minimum . map minimum 
getPositionPatternMax = maximum . map maximum


dropD = [D,A,D,G,B,E]

standardTuning = [E,A,D,G,B,E]

ukelele = [C,E,G,A]