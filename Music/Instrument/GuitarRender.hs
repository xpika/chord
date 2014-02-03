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
import Music.Instrument.Common (ControlAnnotation (..),tuningAndPosToNote,maxPatternHeight,abbreviateNote,horizontalConcat)


renderGuitarChords :: ControlAnnotation -> Bool -> [Note] -> Chord -> Int -> [Char]
renderGuitarChords controlAnnotation orientationVertical tuning chord maxHeight =
    (if minPosition /= 0 then "Fret: " ++ show minPosition ++ "\n" else "") ++ (concat $ intersperse "\n" $ 
        (renderPositionPatternsRange orientationVertical controlAnnotation tuning maxHeight positionPatterns) )
    where 
    positionPatterns = head $ take 1 $ filter (not . null) $ findPositionPatterns chord tuning maxHeight
    minPosition = getPositionPatternMin positionPatterns
    
renderPositionPatternsRange orientationVertical controlAnnotation tuning count positionPatterns' = 
  map ( renderPositionPattern orientationVertical controlAnnotation tuning minPosition (count-1)) positionPatterns'
  where minPosition = getPositionPatternMin positionPatterns'

renderPositionPattern orientationVertical controlAnnotation tuning from maximumPosition positionPattern = (if orientationVertical then foldl1 horizontalConcat else unlines . reverse ) $
  map (\(pos,stringIndex) -> renderGuitarString stringIndex orientationVertical controlAnnotation from maximumPosition pos (tuning!!stringIndex)) (zip positionPattern [0..])

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
