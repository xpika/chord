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

import Music.Instrument.Guitar (findPositionPatterns,getPositionPatternRange)
import Music.Instrument.Piano
import Music.Instrument.Common (ControlAnnotation (..),tuningAndPosToNote,maxPatternHeight,abbreviateNote)

renderGuitarChords :: ControlAnnotation -> [Note] -> Chord -> [Char]
renderGuitarChords controlAnnotation tuning chord =
    (if minPosition /= 0 then "Fret: " ++ show minPosition ++ "\n" else "") ++ (concat $ intersperse "\n" $ 
        renderPositionPatternsRange controlAnnotation tuning maxPatternHeight positionPatterns )
    where 
    positionPatterns = head $ take 1 $ filter (not . null) $ findPositionPatterns chord tuning maxPatternHeight
    (minPosition,_) =  getPositionPatternRange positionPatterns
    
renderPositionPatternsRange controlAnnotation tuning count positionPatterns' = 
  map (\positionPattern -> renderPositionPattern controlAnnotation tuning positionPattern minPosition (count-1)) positionPatterns'
  where (minPosition,_) = getPositionPatternRange positionPatterns'

renderPositionPattern controlAnnotation tuning positionPattern from maximumPosition = unlines $ Data.List.transpose $
  map (\(pos,stringIndex) -> renderGuitarString controlAnnotation from maximumPosition pos (tuning!!stringIndex)) (zip positionPattern [0..])

renderGuitarString controlAnnotation from max positionIndex stringTuning = map (\i->char i) [from..(from + max)]
  where char index | index == positionIndex = fingeringChar stringTuning positionIndex controlAnnotation
                   | otherwise = fretChar index

fingeringChar stringTuning positionIndex controlAnnotation = case controlAnnotation of 
    AnnotateNote -> abbreviateNote $ tuningAndPosToNote stringTuning positionIndex
    AnnotateMarking -> fingeringCharUnannotated positionIndex
    AnnotatePosition -> head (show positionIndex)

fretChar 0 = '='
fretChar _ = '-'

fingeringCharUnannotated 0 = 'o'
fingeringCharUnannotated _ = '*'
