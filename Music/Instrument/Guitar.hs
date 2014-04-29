{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Debug.Trace

class PositionPatternProgression a where
  requiresSequence :: a -> Bool

instance PositionPatternProgression Chord where
  requiresSequence _ = True

instance PositionPatternProgression Scale where 
  requiresSequence _ = False

instance PositionPatternProgression Note where
  requiresSequence _ = False

instance PositionPatternProgression [Note] where
  requiresSequence _ = True

findPositionPatterns allowOpens chord tuning maxHeight utilizeAllStrings rootNoteLowest selectionMask =
  filter (not . null) $ findPositionPatterns' allowOpens chord tuning maxHeight utilizeAllStrings rootNoteLowest selectionMask
        
findPositionPatterns' allowOpens chord tuning maxHeight utilizeAllStrings rootNoteLowest selectionMask =
  scanl1 (flip (\\)) (map (\x-> findPositionPatterns'' allowOpens chord tuning x maxHeight utilizeAllStrings rootNoteLowest selectionMask) [0..])

findPositionPatterns'' allowOpens chord tuning from maxHeight utilizeAllStrings rootNoteLowest selectionMask = applyIf allowOpens (nub . (++) openPatterns) patterns
  where patterns = findPositionPatterns''' False chord tuning from maxHeight utilizeAllStrings rootNoteLowest selectionMask
        openPatterns = filter (isOpened maxHeight) (findPositionPatterns''' True chord tuning from maxHeight utilizeAllStrings rootNoteLowest selectionMask)

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
  rootNoteLowest
  selectionMask 
    = sequencer $ findPositionPatterns'''' includeOpens chord tuning from maxHeight 
    where
      sequencer | requiresSequence chord
        = (\v -> (   filter ( not . null . concat )  
                   . (\x -> filter (\a -> length (strip a) == maximum (map length (map strip x))) x)
				   . applyIf utilizeAllStrings (filter (\x -> length (concat x) == length tuning))
                   . applyIf rootNoteLowest (filter (\x -> take 1 (concat (zipWith (\ps t -> map (tuningAndPosToNote t) ps) x tuning))  == take 1 (newNotes chord)))
				   . applyIf (not utilizeAllStrings && (not . null $ selectionMask)) (filter (\x ->
                       or (
                          map (\selectionMask'' ->
                            all (\(a,b) -> if a then b /= [] else b == []) (zip selectionMask'' x)
                         ) selectionMask
                       )
				     ))
                   . sequence
                   ) v) 
                   . addEmpties
                   . deepenListOfLists 
                | otherwise = (:[])
				
strip = filter (not . null)

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

lightChord = [[False,False,False,True,True,True]]
  
powerChord = [
              [True,True,True,False,False,False]
             ,[False,True,True,True,False,False]
             ]

dropD = [D,A,D,G,B,E]
ukelele = [C,E,G,A]
standardTuning = [E,A,D,G,B,E]
fifthChord n = [n , applyNTimes sharp 7 n]
superEquiv a b = equiv a b || equiv b a
