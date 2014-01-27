module Music.Instrument.Internals where 

import Music.Diatonic
import Music.Diatonic.Note
import Music.Diatonic.Degree
import Music.Diatonic.Chord
import Data.List
import Data.Maybe
import Data.Char

import Music.Instrument.Guitar
import Music.Instrument.Piano
import Music.Instrument.Common

renderGuitarChords :: ControlAnnotation -> [Note] -> (Note -> Chord) -> Note -> [Char]
renderGuitarChords controlAnnotation tuning form root = 
    concat $ intersperse "\n" $ union 
        (renderPositionPatterns controlAnnotation tuning form root 0 4)
            (renderPositionPatterns controlAnnotation tuning form root 1 4)

renderPositionPatterns controlAnnotation tuning chordForm chordRoot from count = 
  map (\positionPattern -> renderPositionPattern controlAnnotation tuning positionPattern from (count-1))
    (positionPatterns (chordForm chordRoot) tuning from count)

renderPositionPattern controlAnnotation tuning positionPattern from maximumPosition = unlines $ Data.List.transpose $
  map (\(pos,stringIndex) -> renderString controlAnnotation from maximumPosition pos (tuning!!stringIndex)) (zip positionPattern [0..])

renderString controlAnnotation from max positionIndex stringTuning = map (\i->char i) [from..(from + max)]
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
        
positionPatterns chord tuning from count =  map ( map ( (+from) . fromJust) . map (uncurry (flip elemIndex))) $ map (zipWith (,) (strings tuning from count)) (notePatterns chord tuning from count)
notePatterns chord tuning from count = sequence $ map (filter (flip elem (extractChord chord))) (strings tuning from count)

strings tuning from count = Data.List.transpose ( (take count . drop from)  $ frets tuning)

frets tuning = map (\n -> (map (canonize . applyNTimes sharp n) tuning)) [0..]

extractChord chord = map snd $ degrees chord