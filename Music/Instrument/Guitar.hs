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


findPositionPatterns 
  allowOpens 
  chord
  tuning 
  maxHeight
  utilizeAllStrings 
  rootNoteLowest 
  selectionMask 
  utilizeAllNotes 
  strictIntervals
  = 
  filter (not . null) $
  findPositionPatterns' 
  allowOpens 
  chord 
  tuning 
  maxHeight 
  utilizeAllStrings 
  rootNoteLowest 
  selectionMask
  utilizeAllNotes 
  strictIntervals
        
findPositionPatterns' 
  allowOpens 
  chord 
  tuning 
  maxHeight
  utilizeAllStrings 
  rootNoteLowest 
  selectionMask 
  utilizeAllNotes 
  strictIntervals 
  =
  map 
  (\x -> 
    findPositionPatterns'' 
    allowOpens 
    chord 
    tuning 
    x 
    maxHeight 
    utilizeAllStrings 
    rootNoteLowest 
    selectionMask 
    utilizeAllNotes
    strictIntervals
  ) 
  [0..]

findPositionPatterns'' 
  allowOpens 
  chord 
  tuning 
  from
  maxHeight 
  utilizeAllStrings 
  rootNoteLowest 
  selectionMask  
  utilizeAllNotes
  strictSteps
  = 
  applyIf 
  allowOpens
  (nub . (++) openPatterns) 
  patterns
  where 
  patterns = 
    findPositionPatterns''' 
    False 
    chord 
    tuning  
    from 
    maxHeight 
    utilizeAllStrings 
    rootNoteLowest 
    selectionMask 
    utilizeAllNotes
    strictSteps
  openPatterns = 
    filter 
    (isOpened maxHeight) 
    (
    findPositionPatterns''' 
    True 
    chord
    tuning
    from 
    maxHeight
    utilizeAllStrings 
    rootNoteLowest
    selectionMask 
    utilizeAllNotes
    strictSteps
    )

getPositionPatternSpannedFrets positionPattern maxHeight
  = applyIf isOpened' (0:) ((uncurry enumFromTo) range)
  where
  range = if isOpened' then (getPositionPatternMin prunedPositionPattern,getPositionPatternMin prunedPositionPattern + maxHeight)
                       else (getPositionPatternMin positionPattern,getPositionPatternMin positionPattern + maxHeight)
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
  utilizeAllNotes
  strictIntervals
    = sequencer $ findPositionPatterns'''' includeOpens chord tuning from maxHeight strictIntervals
    where
      sequencer | requiresSequence chord
        = (\v -> (   filter (not . null . concat)
                   . (\x -> filter (\a -> length (strip a) == maximum (map length (map strip x))) x)
		   . applyIf utilizeAllStrings (filter (\x -> length (concat x) == length tuning))
                   . applyIf utilizeAllNotes (filter (\x -> (nub $ sort $ concat (zipWith (\ps t -> map (tuningAndPosToNote t) ps) x tuning))  == (nub $ sort (newNotes chord))))
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

findPositionPatterns'''' 
  includeOpens 
  chord 
  tuning 
  from
  maxHeight
  strictIntervals 
  =
  map 
  (\interval -> 
    filter 
    (
    if strictIntervals 
    then positionInInterval chord interval 
    else positionInNoteable chord interval
    )
    (applyIf 
     includeOpens 
     (nub . (0:)) 
     (frettedGuitarStringPostionLength from maxHeight)
    )
  )
  (notesToSteps tuning)

positionInInterval 
  intervalable
  stringInterval
  pos
  =
  any 
  (==(pos + stringInterval))
  (getSteps $ lastIntervals intervalable)

positionInNoteable 
  noteable
  stringInterval 
  pos
  =
  any
  (superEquiv note)
  (newNotes noteable)
  where
  note =
   tuningAndPosToNote
   stringTuning
   pos
  stringTuning = stepToNote stringInterval 

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

getPositionPatternMinAdjusted maxHeight positionPattern =
  if isOpened maxHeight positionPattern then head . drop 1 . nub . sort . concat $ positionPattern
                                        else getPositionPatternMin positionPattern

lightChord = [
   [False,False,True,True,True,True]
  ,[False,False,False,True,True,True]
 ]
  
powerChord = [
              [True,True,True,False,False,False]
             ,[False,True,True,True,False,False]
             ]

dropD = [D,A,D,G,B,E]
ukelele = [C,E,G,A]
standardTuning = [E,A,D,G,B,E]
fifthChord n = [n , applyNTimes sharp 7 n]

sus2 chord = (x:canonize(flat(flat y)):xs)
  where (x:y:xs) = newNotes chord
sus4 chord = (x:canonize(sharp y):xs)
  where (x:y:xs) = newNotes chord

superEquiv a b = equiv a b || equiv b a
