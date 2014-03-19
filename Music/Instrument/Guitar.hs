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
  filter (not . null) $ findPositionPatterns' chord tuning maxHeight
        
findPositionPatterns' chord tuning maxHeight =
  scanl1 (flip (\\)) (map (\x-> findPositionPatterns'' chord tuning x maxHeight) [0..])

findPositionPatterns'' chord tuning from maxHeight = sequencer $
    map (\stringTune -> filter (positionInNoteable chord stringTune) (frettedGuitarStringPostionLength from maxHeight)) 
      tuning
  where sequencer | requiresSequence chord = deepenListOfLists . sequence
                  | otherwise = (:[]) . id

positionInNoteable noteable stringTuning pos = any (superEquiv note) (newNotes noteable)
  where note = tuningAndPosToNote stringTuning pos 

frettedGuitarStringPostionLength from maxHeight = [from..(from+maxHeight-1)]

class NewNotes a where
  newNotes :: a -> [Note]

instance NewNotes Chord where
  newNotes = notes

instance NewNotes Scale where
  newNotes = notes

instance NewNotes Note where
  newNotes n = [n]

getPositionPatternRange = liftM2 (,) getPositionPatternMin getPositionPatternMax

getPositionPatternMin = minimum . concat
getPositionPatternMax = maximum . concat

getPositionMultiPatternMax = getPositionPatternMax . concat
getPositionMultiPatternMin = getPositionPatternMin . concat

dropD = [D,A,D,G,B,E]

standardTuning = [E,A,D,G,B,E]

ukelele = [C,E,G,A]

superEquiv a b = equiv a b || equiv b a
