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
import qualified Data.Map

import Music.Instrument.Piano
import Music.Instrument.Common
import qualified Debug.Trace

class PositionPatternProgression a where
  requiresSequence :: a -> Bool

instance PositionPatternProgression Chord where 
  requiresSequence _ = True

instance PositionPatternProgression Scale where 
  requiresSequence _ = False

instance PositionPatternProgression Note where 
  requiresSequence _ = False

findPositionPatterns allowOpens chord tuning maxHeight utilizeAllStrings selectionMask =
  filter (not . null) $ findPositionPatterns' allowOpens chord tuning maxHeight utilizeAllStrings selectionMask
        
findPositionPatterns' allowOpens chord tuning maxHeight utilizeAllStrings selectionMask =
  scanl1 (flip (\\)) (map (\x-> findPositionPatterns'' allowOpens chord tuning x maxHeight utilizeAllStrings selectionMask) [0..])

findPositionPatterns'' allowOpens chord tuning from maxHeight utilizeAllStrings selectionMask = applyIf allowOpens (nub . (++) openPatterns) patterns
  where patterns = findPositionPatterns''' False chord tuning from maxHeight utilizeAllStrings selectionMask
        openPatterns = filter (isOpened maxHeight) (findPositionPatterns''' True chord tuning from maxHeight utilizeAllStrings selectionMask)

getPositionPatternSpannedFrets positionPattern maxHeight
  = applyIf isOpened' (0:) ((uncurry enumFromTo) range)
  where 
  range | isOpened' = (getPositionPatternMin prunedPositionPattern,getPositionPatternMin prunedPositionPattern + maxHeight)
        | otherwise = (getPositionPatternMin positionPattern,getPositionPatternMin positionPattern + maxHeight)
  isOpened' = isOpened maxHeight positionPattern
  prunedPositionPattern = map (filter (not.(==0))) positionPattern
		
isOpened maxHeight positionPattern = (>maxHeight) . getPositionPatternHeight $ positionPattern

findPositionPatterns''' 
  includeOpens
  chord
  tuning
  from
  maxHeight
  utilizeAllStrings
  --rootNoteFirst
  selectionMask 
    = sequencer $ findPositionPatterns'''' includeOpens chord tuning from maxHeight 
    where sequencer | requiresSequence chord = ( \v -> ( filter ( not . null . concat )  
                                               . (if utilizeAllStrings then (filter (\x -> length (concat x) == length tuning))
											                           else (filter (\x -> length (concat x) == length (filter (not . (==[[]])) v))))
                                               . sequence
											   ) v)
									           . applyIf (not (null selectionMask)) (\j -> (zipWith (\c a -> if c then a else [[]]) selectionMask j))
											   . addEmpties
                                               . deepenListOfLists 
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

getPositionMultiPatternMinAdjusted maxHeight = getPositionPatternMinAdjusted maxHeight . concat

getPositionPatternMinAdjusted maxHeight positionPattern
  | isOpened maxHeight positionPattern =  head . drop 1 . nub . sort . concat  $ positionPattern
  | otherwise = getPositionPatternMin positionPattern

lightChord = [False,False,False,True,True,True]

dropD = [D,A,D,G,B,E]
ukelele = [C,E,G,A]
standardTuning = [E,A,D,G,B,E]

superEquiv a b = equiv a b || equiv b a
