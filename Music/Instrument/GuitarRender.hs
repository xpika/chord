module Music.Instrument.GuitarRender where

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

import Music.Instrument.Guitar (findPositionPatterns,getPositionPatternMin,getPositionMultiPatternMin,getPositionPatternProgressions,PositionPatternProgression)
import Music.Instrument.Piano
import Music.Instrument.Common (ControlAnnotation (..),tuningAndPosToNote,abbreviateNote,horizontalConcat)


renderGuitarChord :: PositionPatternProgression a => ControlAnnotation -> Bool -> Bool -> [Note] -> a -> Int -> Int -> [Char]
renderGuitarChord controlAnnotation firstTuningFirst orientationVertical tuning chord maxHeight from =
  head $
    renderGuitarChord' controlAnnotation firstTuningFirst orientationVertical tuning maxHeight from positionPatternProgressions
  where positionPatternProgressions = getPositionPatternProgressions chord tuning maxHeight

renderGuitarChord' controlAnnotation firstTuningFirst orientationVertical tuning maxHeight from positionPatternsProgressions =
  drop from $
    map (renderGuitarChord'' controlAnnotation firstTuningFirst orientationVertical tuning maxHeight) positionPatternsProgressions

renderGuitarChord'' controlAnnotation firstTuningFirst orientationVertical tuning maxHeight positionPatterns =
  heading $ concat $ intersperse "\n" $ 
      renderPositionPatternsRange firstTuningFirst orientationVertical controlAnnotation tuning maxHeight positionPatterns
  where
  minPosition = getPositionMultiPatternMin positionPatterns
  heading | minPosition /= 0 = (++) ("Fret: " ++ show minPosition ++ "\n")
          | otherwise = id

renderPositionPatternsRange firstTuningFirst orientationVertical controlAnnotation tuning count positionPatterns' = 
  map (renderPositionPattern firstTuningFirst orientationVertical controlAnnotation tuning minPosition (count-1)) positionPatterns'
  where minPosition = getPositionMultiPatternMin positionPatterns'

renderPositionPattern firstTuningFirst orientationVertical controlAnnotation tuning from maximumPosition positionPattern = 
  combiner $
    map (\(pos,stringIndex) -> 
       renderGuitarString stringIndex orientationVertical controlAnnotation from maximumPosition pos (arranger tuning!!stringIndex))
         (zip positionPattern [0..])
  where combiner | orientationVertical = foldl1 horizontalConcat
                 | otherwise = unlines
        arranger | firstTuningFirst = id
                 | otherwise = reverse

renderGuitarString stringIndex orientationVertical controlAnnotation from max positionIndices stringTuning = 
  lineBreaker $ map (char stringIndex orientationVertical stringTuning positionIndices controlAnnotation) [from..(from + max)]
  where lineBreaker | orientationVertical = intersperse '\n' 
                    | otherwise = id

char stringIndex orientationVertical stringTuning positionIndices controlAnnotation index 
  | index `elem` positionIndices = fingeringChar stringIndex stringTuning index controlAnnotation
  | otherwise = fretChar orientationVertical index

fingeringChar stringIndex stringTuning positionIndex controlAnnotation = 
  case controlAnnotation of 
    AnnotateNote -> abbreviateNote $ tuningAndPosToNote stringTuning positionIndex
    AnnotateMarking -> fingeringCharUnannotated positionIndex
    AnnotatePositionVertical -> head (show positionIndex)
    AnnotatePositionHorizontal -> head (show stringIndex)

fretChar orientationVertical 0 | orientationVertical = '='
                               | otherwise = '|'
                               
fretChar orientationVertical _ | orientationVertical = '-'
                               | otherwise = '-'

fingeringCharUnannotated 0 = 'o'
fingeringCharUnannotated _ = '*'

rotateText =  unlines . Data.List.transpose . lines
