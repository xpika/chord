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
  getPositionPatternProgressions :: Bool -> a -> [Note] -> Int -> [[[[Int]]]]
  requiresSequence :: a -> Bool

instance PositionPatternProgression Chord where 
  getPositionPatternProgressions allowOpens chord tuning maxHeight = filter (not . null) $ findPositionPatterns allowOpens chord tuning maxHeight 
  requiresSequence _ = True

instance PositionPatternProgression Scale where 
  getPositionPatternProgressions allowOpens scale tuning maxHeight = filter (not . null) $ findPositionPatterns allowOpens scale tuning maxHeight 
  requiresSequence _ = False

instance PositionPatternProgression Note where 
  getPositionPatternProgressions allowOpens note tuning maxHeight = filter (not . null) $ findPositionPatterns allowOpens note tuning maxHeight 
  requiresSequence _ = False

findPositionPatterns allowOpens chord tuning maxHeight =
  filter (not . null) $ findPositionPatterns' allowOpens chord tuning maxHeight
        
findPositionPatterns' allowOpens chord tuning maxHeight =
  scanl1 (flip (\\)) (map (\x-> findPositionPatterns'' allowOpens chord tuning x maxHeight) [0..])

findPositionPatterns'' allowOpens chord tuning from maxHeight = applyIf allowOpens (nub . (++) openPatterns) patterns
  where patterns = findPositionPatterns''' False chord tuning from maxHeight
        openPatterns = filter (isOpened maxHeight) (findPositionPatterns''' True chord tuning from maxHeight)

isOpened maxHeight positionPattern = (>maxHeight) . getPositionPatternHeight $ positionPattern

getPositionPatternSpannedFrets positionPattern maxHeight
  | isOpened maxHeight positionPattern = 0 : ((uncurry enumFromTo) (getPositionPatternRange prunedPositionPattern))
  | otherwise = (uncurry enumFromTo) (getPositionPatternRange positionPattern)
  where prunedPositionPattern = map (filter (not.(==0)))  positionPattern

findPositionPatterns''' includeOpens chord tuning from maxHeight = sequencer $ findPositionPatterns'''' includeOpens chord tuning from maxHeight
  where sequencer | requiresSequence chord = filter ( not . null . concat ).  map (filter ( not.  null)) . sequence . applyIf False addEmpties . deepenListOfLists
                  | otherwise = (:[]) 

findPositionPatterns'''' includeOpens chord tuning from maxHeight =
    map (\stringTune -> filter (positionInNoteable chord stringTune) (applyIf includeOpens (nub . (0:)) (frettedGuitarStringPostionLength from maxHeight))) 
      tuning

positionInNoteable noteable stringTuning pos = any (superEquiv note) (newNotes noteable)
  where note = tuningAndPosToNote stringTuning pos

frettedGuitarStringPostionLength from maxHeight = [from..(from+maxHeight-1)]

getPositionPatternRange = liftM2 (,) getPositionPatternMin getPositionPatternMax

getPositionMultiPatternRange = liftM2 (,) getPositionMultiPatternMin getPositionMultiPatternMax

getPositionMultiPatternHeight = uncurry subtract . getPositionMultiPatternRange

getPositionPatternHeight = uncurry subtract . getPositionPatternRange

getPositionPatternMin = minimum . concat

getPositionPatternMax = maximum . concat

getPositionMultiPatternMax = getPositionPatternMax . concat
getPositionMultiPatternMin = getPositionPatternMin . concat

getPositionMultiPatternMinAdjusted maxHeight  = getPositionPatternMinAdjusted maxHeight   . concat

getPositionPatternMinAdjusted maxHeight positionPattern
  | isOpened maxHeight positionPattern =  head . drop 1 . nub . sort . concat  $ positionPattern
  | otherwise = getPositionPatternMin positionPattern

dropD = [D,A,D,G,B,E]

standardTuning = [E,A,D,G,B,E]

ukelele = [C,E,G,A]

superEquiv a b = equiv a b || equiv b a