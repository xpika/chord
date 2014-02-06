module Music.Instrument.GuitarRender where

import Music.Diatonic
import Music.Diatonic.Note
import Music.Diatonic.Degree
import Music.Diatonic.Chord
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import qualified Data.Set

import Music.Instrument.Guitar (findPositionPatterns,getPositionPatternMin)
import Music.Instrument.Piano
import Music.Instrument.Common (ControlAnnotation (..),tuningAndPosToNote,abbreviateNote,horizontalConcat)


renderGuitarChords :: ControlAnnotation -> Bool -> Bool -> [Note] -> Chord -> Int -> [Char]
renderGuitarChords controlAnnotation firstTuningFirst orientationVertical tuning chord maxHeight =
    (if minPosition /= 0 then "Fret: " ++ show minPosition ++ "\n" else "") ++ (concat $ intersperse "\n" $ 
        (renderPositionPatternsRange firstTuningFirst orientationVertical controlAnnotation tuning maxHeight positionPatterns) )
    where
    positionPatterns = head $ take 1 $ filter (not . null) $ findPositionPatterns chord tuning maxHeight
    minPosition = getPositionPatternMin positionPatterns
    
renderPositionPatternsRange firstTuningFirst orientationVertical controlAnnotation tuning count positionPatterns' = 
  map ( renderPositionPattern firstTuningFirst orientationVertical controlAnnotation tuning minPosition (count-1)) positionPatterns'
  where minPosition = getPositionPatternMin positionPatterns'

renderPositionPattern firstTuningFirst orientationVertical controlAnnotation tuning from maximumPosition positionPattern = 
  combiner $
    map (\(pos,stringIndex) -> 
       renderGuitarString stringIndex orientationVertical controlAnnotation from maximumPosition pos (arranger tuning!!stringIndex))
         (zip positionPattern [0..])
  where combiner | orientationVertical = foldl1 horizontalConcat
                 | otherwise = unlines
        arranger | firstTuningFirst = id
                 | otherwise = reverse


renderGuitarString stringIndex orientationVertical controlAnnotation from max positionIndex stringTuning = 
  (if orientationVertical then intersperse '\n' else id )
    (map (char stringIndex orientationVertical stringTuning positionIndex controlAnnotation) [from..(from + max)])

char stringIndex orientationVertical stringTuning positionIndex controlAnnotation index 
  | index == positionIndex = fingeringChar stringIndex stringTuning positionIndex controlAnnotation
  | otherwise = fretChar orientationVertical index

fingeringChar stringIndex stringTuning positionIndex controlAnnotation = case controlAnnotation of 
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
