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

import Music.Instrument.Guitar (
   findPositionPatterns
  ,getPositionMultiPatternMin
  ,getPositionPatternProgressions
  ,PositionPatternProgression
  ,getPositionMultiPatternMinAdjusted
  ,getPositionPatternMinAdjusted
  ,getPositionPatternHeight
  ,getPositionPatternSpannedFrets)
import Music.Instrument.Piano
import Music.Instrument.Common (ControlAnnotation (..),tuningAndPosToNote,abbreviateNote,horizontalConcat,applyIf,insertAt)

import Debug.Trace

renderGuitarChord :: PositionPatternProgression a => Bool -> ControlAnnotation -> Bool -> Bool -> Bool -> [Note] -> a -> Int -> Int -> [Char]
renderGuitarChord allowOpens controlAnnotation annotateFrets firstTuningFirst orientationVertical tuning chord maxHeight from =
  head $
    renderGuitarChord' controlAnnotation annotateFrets firstTuningFirst orientationVertical tuning maxHeight from positionPatternProgressions
  where positionPatternProgressions = getPositionPatternProgressions allowOpens chord tuning maxHeight

renderGuitarChord' controlAnnotation annotateFrets firstTuningFirst orientationVertical tuning maxHeight from positionPatternsProgressions =
  drop from $
    map (renderGuitarChord'' controlAnnotation annotateFrets firstTuningFirst orientationVertical tuning maxHeight) positionPatternsProgressions

renderGuitarChord'' controlAnnotation annotateFrets firstTuningFirst orientationVertical tuning maxHeight positionPatterns =
   concat $ intersperse "\n" $ 
      renderPositionPatternsRange annotateFrets firstTuningFirst orientationVertical controlAnnotation tuning maxHeight positionPatterns

renderPositionPatternsRange annotateFrets firstTuningFirst orientationVertical controlAnnotation tuning maxHeight positionPatterns' = 
  map (renderPositionPattern annotateFrets firstTuningFirst orientationVertical controlAnnotation tuning minPosition (maxHeight-1)) positionPatterns'
  where minPosition = getPositionMultiPatternMin positionPatterns'


renderPositionPattern annotateFrets firstTuningFirst orientationVertical controlAnnotation tuning from maxHeight positionPattern = 
   heading $ unlines $ renderPositionPattern' annotateFrets firstTuningFirst orientationVertical controlAnnotation tuning from maxHeight positionPattern
  where minPositionAdjusted = getPositionPatternMinAdjusted maxHeight positionPattern
        heading | minPositionAdjusted /= 0 = (++) ("Fret: " ++ show minPositionAdjusted ++ "\n")
                | otherwise = id

renderPositionPattern' annotateFrets firstTuningFirst orientationVertical controlAnnotation tuning from maxHeight positionPattern = 
  guitarStringTexts
  where guitarStringTexts = applyIf orientationVertical (map reverse . Data.List.transpose) guitarStringTexts'
        guitarStringTexts' = applyIf annotateFrets (++ (Data.List.transpose fretAnnotations)) guitarStringTexts''
        fretAnnotations = map (overlayStringRight fretAnnotationPadding) fretAnnotations'
        overlayStringRight x y = map last $ Data.List.transpose [x,y]
        fretAnnotationPadding = take maximumFretAnnotationLength (repeat ' ')
        maximumFretAnnotationLength = maximum . map length $ fretAnnotations'
        fretAnnotations' = map show $ take (maxHeight'+1) [0..]
        guitarStringTexts'' =
          map (\(pos,stringIndex) 
            -> renderGuitarString' stringIndex orientationVertical controlAnnotation from maxHeight' pos (tuning!!stringIndex) positionPatternSpannedFrets)
              (zip (reverse positionPattern) stringIndicies)
        stringIndicies | firstTuningFirst = [0..]
                       | otherwise = [guitarStringCount-1,guitarStringCount-2..]
        guitarStringCount = length positionPattern
        maxHeight' = getPositionPatternHeight positionPattern
        minHeight' = getPositionPatternHeight positionPattern
        positionPatternSpannedFrets = getPositionPatternSpannedFrets positionPattern maxHeight
        minPositionAdjusted = getPositionPatternMinAdjusted maxHeight positionPattern
        tuning' = reverse tuning

firstGap [] = Nothing
firstGap xs = listToMaybe (take 1 $ map fst $ dropWhile (uncurry (==)) $ zip [head xs..] xs)

renderGuitarString' stringIndex orientationVertical controlAnnotation from max positionIndices stringTuning positionPatternSpannedFrets   = 
  applyIf (not (positionPatternsGap == Nothing)) (addGap positionPatternsGap)
    $ map (char stringIndex orientationVertical stringTuning positionIndices controlAnnotation)  positionPatternSpannedFrets
  where positionPatternsGap = firstGap positionPatternSpannedFrets 
        addGap (Just n) str = insertAt n (gapChar orientationVertical) str

gapChar orientationVertical | orientationVertical = '~'
                            | otherwise = 'S'

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
