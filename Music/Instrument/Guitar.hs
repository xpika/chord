{-# LANGUAGE NoMonomorphismRestriction #-}
module Music.Instrument.Guitar where

import Music.Diatonic
import Music.Diatonic.Note
import Music.Diatonic.Degree
import Music.Diatonic.Chord
import Music.Diatonic.Scale
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import qualified Data.Set

import Music.Instrument.Piano
import Music.Instrument.Common

{-
type Position = Int
type String = [Int]
type PostionPattern = [[Int]]
type PositonPattern = [[[Int]]]
type PositonPatternProgression = [[[[Int]]]]
-}

class PositionPatternProgression a where
  getPositionPatternProgressions :: a -> [Note] -> Int -> [[[[Int]]]]
  requiresSequence :: a -> Bool

instance PositionPatternProgression Chord where 
  getPositionPatternProgressions chord tuning maxHeight = filter (not . null) $ findPositionPatterns chord tuning maxHeight 
  requiresSequence _ = True

instance PositionPatternProgression Scale where 
  getPositionPatternProgressions scale tuning maxHeight = filter (not . null) $ findPositionPatterns scale tuning maxHeight 
  requiresSequence _ = False

instance PositionPatternProgression Note where 
  getPositionPatternProgressions note tuning maxHeight = filter (not . null) $ findPositionPatterns note tuning maxHeight 
  requiresSequence _ = False

findPositionPatterns chord tuning maxHeight =
  filter ( not . null  ) $ findPositionPatterns' chord tuning maxHeight
        
findPositionPatterns' chord tuning maxHeight =
  scanl1 (flip (\\)) (map (\x-> findPositionPatterns'' chord tuning x maxHeight) [0..])

findPositionPatterns'' chord tuning from maxHeight =
  map (zipWith ( ( (raise . concat) .) . findIndicess superEquiv) 
    (frettedGuitarStringsLengths from maxHeight tuning)) (notePatterns chord tuning from maxHeight)
  where
  raise = map ((+from))

notePatterns notable tuning from count = sequencer (guitarStringPatterns notable tuning from count)
  where sequencer | requiresSequence notable = deepenListOfLists . sequence
                  | otherwise = (:[]) . id


class NewNotes a where
  newNotes :: a -> [Note]

instance NewNotes Chord where
  newNotes = notes

instance NewNotes Scale where
  newNotes = notes

instance NewNotes Note where
  newNotes n = [n]

guitarStringPatterns notable tuning from count =
 map (filter (flip (any . superEquiv) (newNotes notable)))
   (frettedGuitarStringsLengths from count tuning)

frettedGuitarStringsLengths from maxHeight = map (take maxHeight . drop from) . frettedGuitarStrings
frettedGuitarStrings tuning = map fret tuning
fret tune = map (\n -> canonize . applyNTimes sharp n $ tune) [0..]

positionsAndTuningToNotes tuning positions = zipWith tuneAndPositionToNote tuning positions
tuneAndPositionToNote tune position = fret tune !! position

getPositionPatternRange = liftM2 (,) getPositionPatternMin getPositionPatternMax

getPositionPatternMin = minimum . map minimum
getPositionPatternMax = maximum . map maximum

getPositionMultiPatternMax = maximum . map maximum . map maximum
getPositionMultiPatternMin = minimum . map minimum . map minimum

dropD = [D,A,D,G,B,E]

standardTuning = [E,A,D,G,B,E]

ukelele = [C,E,G,A]

superEquiv a b = equiv a b || equiv b a
