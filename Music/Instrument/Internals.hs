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

type Tuning = [Note]

renderGuitarChords = renderChordsFirstFiveFretsWithMaximumHeightOfFour AnnotateMarking standardTuning

inversions = map  sequenceDegrees  . rotations 

rotations =  reverse . (\list -> map (\n -> (take (length list) . drop (length list -n)) (cycle list)) [1..length list] )

sequenceDegrees ds = scanl1 (\x y-> x + mod (y-x) (12::Int)) ds
    
renderMajorChordsWithTuning tuning = renderChordsWithTuning tuning majorChord 

renderChordsWithTuning tuning = renderChordsFirstFiveFretsWithMaximumHeightOfFour  AnnotateMarking tuning

renderChordsAnnotating annotation = renderChordsFirstFiveFretsWithMaximumHeightOfFour annotation standardTuning

renderChordsFirstFiveFretsWithMaximumHeightOfFour :: ControlAnnotation -> [Note] -> (Note -> Chord) -> Note -> [Char]
renderChordsFirstFiveFretsWithMaximumHeightOfFour a t f r = concat $ union (renderGuitarChords' a t f r) (renderGuitarChords' a (map sharp t) f r)

renderGuitarChords' annotate_notes tuning chordForm chordRoot = map unlines $ intersperse ["       "] $ map Data.List.transpose $ 
  map (\(v,b) -> renderFretBoard chordRoot chordForm annotate_notes tuning v b maximumPosition) (zip chordPositionsVertical [0..])
    where chordPositionsVertical = positions (chordForm chordRoot) tuning
          maximumPosition = maximum $ (map maximum) chordPositionsVertical

renderFretBoard chordRoot chordForm annotate_notes tuning strings iteration maximumPosition = 
  map (\(pos,stringIndex) -> renderString annotate_notes maximumPosition pos iteration (tuning!!stringIndex)) (zip strings [0..])

renderString annotate_notes max p iteration stringTuning = map (\i->char i p) [0..max]
  where char index pos | index == pos = fingeringChar pos
                       | otherwise = fretChar index 
        fingeringChar pos = case annotate_notes of {
                  AnnotateNote -> abbreviateNote $ tuningAndPosToNote stringTuning pos
                ; AnnotateMarking -> fingeringCharUnannotated pos
                ; AnnotatePosition -> head (show pos)
        }
        fretChar 0 = '='
        fretChar _ = '-'
        fingeringCharUnannotated 0 = 'o'
        fingeringCharUnannotated _ = '*'
        
positions chord tuning = map (map fromJust) $ map (map (uncurry (flip elemIndex))) $ map (zipWith (,) (strings tuning)) (guitarNotes chord tuning)

guitarNotes chord tuning = sequence $ map (filter (flip elem (extractChord chord))) (strings tuning)

strings :: [Note] -> [[Note]]
strings tuning = Data.List.transpose (take 4 (frets tuning))

extractChord chord = map snd $ degrees chord 

frets tuning = map (\n -> (map (canonize . applyNTimes sharp n) tuning)) [0..] 
