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
import Debug.Trace

import Music.Instrument.Guitar (
   findPositionPatterns
  ,getPositionMultiPatternMin
  ,PositionPatternProgression
  ,getPositionPatternMinAdjusted
  ,getPositionPatternHeight
  ,getPositionPatternSpannedFrets
  )
  
import Music.Instrument.Piano
import Music.Instrument.Common (
 ControlAnnotation (..),tuningAndPosToNote,abbreviateNote,horizontalConcat,applyIf,insertAt
 ,NewNotes
 )
 
maxFretHeight = 30

renderGuitarConcept 
 :: (PositionPatternProgression a,NewNotes a)
 => Bool 
 -> ControlAnnotation 
 -> Bool
 -> Bool
 -> Bool
 -> [Note]
 -> a 
 -> Int 
 -> Int
 -> Bool 
 -> Bool
 -> [[Bool]]
 -> Bool
 -> [Char] 
renderGuitarConcept 
 allowOpens 
 controlAnnotation 
 annotateFrets
 firstTuningLast
 orientationVertical
 tuning 
 chord
 maxHeight
 from
 utilizeAllStrings 
 rootNoteLowest
 selectionMask
 renderAllFrets 
 =
   head
 $ renderGuitarChord' renderAllFrets controlAnnotation annotateFrets firstTuningLast orientationVertical tuning maxHeight from positionPatternProgressions 
 where 
 positionPatternProgressions = 
     take maxFretHeight 
   $ findPositionPatterns allowOpens chord tuning maxHeight utilizeAllStrings rootNoteLowest selectionMask
       
renderGuitarChord' renderAllFrets controlAnnotation annotateFrets firstTuningLast orientationVertical tuning maxHeight from positionPatternsProgressions =
    drop from
  $ map (renderGuitarChord'' renderAllFrets controlAnnotation annotateFrets firstTuningLast orientationVertical tuning maxHeight) positionPatternsProgressions

renderGuitarChord'' renderAllFrets controlAnnotation annotateFrets firstTuningLast orientationVertical tuning maxHeight positionPatterns =
    concat 
  $ intersperse "\n" 
  $ renderPositionPatternsRange renderAllFrets annotateFrets firstTuningLast orientationVertical controlAnnotation tuning maxHeight positionPatterns

renderPositionPatternsRange renderAllFrets annotateFrets firstTuningLast orientationVertical controlAnnotation tuning maxHeight positionPatterns' = 
  map (renderPositionPattern renderAllFrets annotateFrets firstTuningLast orientationVertical controlAnnotation tuning minPosition (maxHeight-1)) positionPatterns'
  where 
  minPosition = getPositionMultiPatternMin positionPatterns'

renderPositionPattern renderAllFrets annotateFrets firstTuningLast orientationVertical controlAnnotation tuning from maxHeight positionPattern = 
    applyIf (minPositionAdjusted /= 0 && not renderAllFrets) ((++) ("Fret: " ++ show minPositionAdjusted ++ "\n"))
  $ unlines
  $ renderPositionPattern' renderAllFrets annotateFrets firstTuningLast orientationVertical controlAnnotation tuning from maxHeight positionPattern
  where
  minPositionAdjusted = getPositionPatternMinAdjusted maxHeight positionPattern

renderPositionPattern' 
  renderAllFrets
  annotateFrets 
  firstTuningLast
  orientationVertical
  controlAnnotation
  tuning
  from
  maxHeight
  positionPattern
  = 
  applyIf orientationVertical (map reverse . Data.List.transpose) guitarStringTexts'
  where
  guitarStringTexts' = applyIf annotateFrets (++ (Data.List.transpose fretAnnotations)) guitarStringTexts''
  fretAnnotations = map (overlayStringRight fretAnnotationPadding) fretAnnotations'
  guitarStringTexts'' =
    map (\(pos,stringIndex) -> 
	       renderGuitarString
	       renderAllFrets 
	       (if firstTuningLast then stringIndex else compliment (length tuning) stringIndex)
		   orientationVertical
		   controlAnnotation 
		   from 
		   pos
		   (tuning'!!stringIndex) 
	       positionPatternSpannedFrets
	    )
    (zip (reverse positionPattern) stringIndicies)
  overlayStringRight x y = map last $ Data.List.transpose [x,y]
  fretAnnotationPadding = take maximumFretAnnotationLength (repeat ' ')
  maximumFretAnnotationLength = maximum . map length $ fretAnnotations'
  fretAnnotations' = map show positionPatternSpannedFrets
  stringIndicies = [0..]
  guitarStringCount = length positionPattern
  positionPatternSpannedFrets = if renderAllFrets then [0..maximum positionPatternSpannedFrets']
                                                  else positionPatternSpannedFrets'
  positionPatternSpannedFrets' = getPositionPatternSpannedFrets positionPattern maxHeight
  tuning' = reverse tuning

firstGap [] = Nothing
firstGap xs = listToMaybe (take 1 $ map fst $ dropWhile (uncurry (==)) $ zip [head xs..] xs)

compliment m n = m - n

renderGuitarString 
 renderAllFrets
 stringIndex 
 orientationVertical
 controlAnnotation
 from
 positionIndices
 stringTuning
 positionPatternSpannedFrets 
 = 
   applyIf (not (positionPatternsGap == Nothing)) (addGap positionPatternsGap)
 $ map (char stringIndex orientationVertical stringTuning positionIndices controlAnnotation) positionPatternSpannedFrets
 where 
 positionPatternsGap = firstGap positionPatternSpannedFrets 
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