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

import Music.Instrument.Guitar
import Music.Instrument.Internals
import Music.Instrument.Piano
import Music.Instrument.Common


renderGuitarChords :: ControlAnnotation -> [Note] -> Chord -> [Char]
renderGuitarChords controlAnnotation tuning chord =
    concat $ intersperse "\n" $ 
           renderVerticalyConstrainedPositionPatterns controlAnnotation tuning 0 4 p1 
        ++ renderVerticalyConstrainedPositionPatterns controlAnnotation tuning 1 4 p2 
    where 
    [p1,p2] = take 2 $ positionPatterns chord tuning 4
        

renderVerticalyConstrainedPositionPatterns controlAnnotation tuning from count positionPatterns' = 
  map (\positionPattern -> renderPositionPattern controlAnnotation tuning positionPattern from (count-1)) positionPatterns'

renderPositionPattern controlAnnotation tuning positionPattern from maximumPosition = unlines $ Data.List.transpose $
  map (\(pos,stringIndex) -> renderGuitarString controlAnnotation from maximumPosition pos (tuning!!stringIndex)) (zip positionPattern [0..])

renderGuitarString controlAnnotation from max positionIndex stringTuning = map (\i->char i) [from..(from + max)]
  where char index | index == positionIndex = fingeringChar
                   | otherwise = fretChar index
        fingeringChar = case controlAnnotation of {
                  AnnotateNote -> abbreviateNote $ tuningAndPosToNote stringTuning positionIndex
                ; AnnotateMarking -> fingeringCharUnannotated positionIndex
                ; AnnotatePosition -> head (show positionIndex)
        }
        fretChar 0 = '='
        fretChar _ = '-'
        fingeringCharUnannotated 0 = 'o'
        fingeringCharUnannotated _ = '*'
