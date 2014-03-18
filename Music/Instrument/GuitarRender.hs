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


import Music.Instrument.Guitar (findPositionPatterns,getPositionMultiPatternMin,getPositionPatternProgressions,PositionPatternProgression)
import Music.Instrument.Piano
import Music.Instrument.Common (ControlAnnotation (..),tuningAndPosToNote,abbreviateNote,horizontalConcat)


renderGuitarChord :: PositionPatternProgression a => ControlAnnotation -> Bool -> Bool -> Bool -> [Note] -> a -> Int -> Int -> [Char]
renderGuitarChord controlAnnotation annotateFrets firstTuningFirst orientationVertical tuning chord maxHeight from =
  head $
    renderGuitarChord' controlAnnotation annotateFrets firstTuningFirst orientationVertical tuning maxHeight from positionPatternProgressions
  where positionPatternProgressions = getPositionPatternProgressions chord tuning maxHeight

renderGuitarChord' controlAnnotation annotateFrets firstTuningFirst orientationVertical tuning maxHeight from positionPatternsProgressions =
  drop from $
    map (renderGuitarChord'' controlAnnotation annotateFrets firstTuningFirst orientationVertical tuning maxHeight) positionPatternsProgressions

renderGuitarChord'' controlAnnotation annotateFrets firstTuningFirst orientationVertical tuning maxHeight positionPatterns =
  heading $ concat $ intersperse "\n" $ 
      renderPositionPatternsRange annotateFrets firstTuningFirst orientationVertical controlAnnotation tuning maxHeight positionPatterns
  where
  minPosition = getPositionMultiPatternMin positionPatterns
  heading | minPosition /= 0 = (++) ("Fret: " ++ show minPosition ++ "\n")
          | otherwise = id

renderPositionPatternsRange annotateFrets firstTuningFirst orientationVertical controlAnnotation tuning maxHeight positionPatterns' = 
  map (renderPositionPattern annotateFrets firstTuningFirst orientationVertical controlAnnotation tuning minPosition (maxHeight-1)) positionPatterns'
  where minPosition = getPositionMultiPatternMin positionPatterns'

renderPositionPattern annotateFrets firstTuningFirst orientationVertical controlAnnotation tuning from maxHeight positionPattern = 
  unlines $ guitarStringTexts
  where guitarStringTexts | orientationVertical = Data.List.transpose guitarStringTexts'
                          | otherwise = guitarStringTexts'
        guitarStringTexts' | annotateFrets =  (Data.List.transpose fretAnnotations) ++ guitarStringTexts''
                           | otherwise = guitarStringTexts''
        fretAnnotations = map (overlayStringRight fretAnnotationPadding) fretAnnotations'
        overlayStringRight x y = map last $ Data.List.transpose [x,y]
        fretAnnotationPadding = take maximumFretAnnotationLength (repeat ' ')
        maximumFretAnnotationLength = maximum . map length $ fretAnnotations'
        fretAnnotations' = map show $ take (maxHeight+1) [0..]
        guitarStringTexts'' =
          map (\(pos,stringIndex) -> renderGuitarString' stringIndex orientationVertical controlAnnotation from maxHeight pos (tuning!!stringIndex))
            (zip positionPattern stringIndicies)
        stringIndicies | firstTuningFirst = [0..]
                       | otherwise = [guitarStringCount-1,guitarStringCount-2..]
        guitarStringCount = length positionPattern

renderGuitarString stringIndex orientationVertical controlAnnotation from maxHeight positionIndices stringTuning =  
   lineBreaker $ renderGuitarString' stringIndex orientationVertical controlAnnotation from maxHeight positionIndices stringTuning 
  where lineBreaker | orientationVertical = intersperse '\n'
                    | otherwise = id

renderGuitarString' stringIndex orientationVertical controlAnnotation from max positionIndices stringTuning = 
  map (char stringIndex orientationVertical stringTuning positionIndices controlAnnotation) [from..(from + max)]

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
