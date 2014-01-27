module Music.Instrument.Internals where 

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
import Music.Instrument.Piano
import Music.Instrument.Common

renderGuitarChords :: ControlAnnotation -> [Note] -> Chord -> [Char]
renderGuitarChords controlAnnotation tuning chord =
    concat $ intersperse "\n" $ union 
        (renderVerticalyConstrainedPositionPatterns controlAnnotation tuning 0 4 p1)
            (renderVerticalyConstrainedPositionPatterns controlAnnotation tuning 1 4 p2')
    where 
    p2' = p2 \\ p1
    p1 = positionPatterns chord tuning 0 4
    p2 = positionPatterns chord tuning 1 4
        

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
        
positionPatterns chord tuning from count = 
  map ( map ( (+from) . fromJust) . map (uncurry (flip elemIndex))) $ map (zipWith (,) (frettedGuitarStringsLengths from count tuning)) (notePatterns chord tuning from count)

notePatterns chord tuning from count = 
  sequence $ map (filter (flip elem (chordNotes chord))) (frettedGuitarStringsLengths from count tuning)

frettedGuitarStringsLengths from count = map (take count . drop from) . frettedGuitarStrings
frettedGuitarStrings tuning = map fret tuning
fret tune = map (\n -> canonize . applyNTimes sharp n $ tune) [0..]

positionsAndTuningToNotes tuning positions = zipWith tuneAndPositionToNote tuning positions
tuneAndPositionToNote tune position =  fret tune !! position

chordNotes chord = map snd $ degrees chord

findChord inputNotes = do 
  chordType <- chordTypes
  root <- chromaticScale
  let notes = chordNotes (chordType root)
  guard (Data.Set.isSubsetOf (Data.Set.fromList (uns inputNotes )) (Data.Set.fromList notes))
  return (chordType root) 
  where uns = map canonize

chordTypes = [majorChord, minorChord, diminishedChord, augmentedChord,
              major7thChord, dominant7thChord, minor7thChord, minorMajor7thChord, minor7thFlat5thChord, diminished7thChord, augmentedMajor7thChord]

