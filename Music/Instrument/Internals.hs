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


data ControlAnnotation = AnnotateNote | AnnotatePosition | AnnotateMarking


type Tuning = [Note]



data Instrument = Guitar | Piano


renderChords = renderChordsFirstFiveFretsWithMaximumHeightOfFour AnnotateMarking standardTuning



levelChord = map (flip mod (12::Int))

inversions = map  sequenceDegrees  . rotations 

rotations =  reverse . (\list -> map (\n -> (take (length list) . drop (length list -n)) (cycle list)) [1..length list] )

sequenceDegrees ds = scanl1 (\x y-> x + mod (y-x) (12::Int)) ds



renderPianoChord chordForm chordRoot = renderPiano (levelChord degrees)
    where degrees = extractDegrees (chordRoot,chordForm)
    
renderMajorChordsWithTuning tuning = renderChordsWithTuning tuning majorChord 

renderChordsWithTuning tuning = renderChordsFirstFiveFretsWithMaximumHeightOfFour  AnnotateMarking tuning

renderChordsAnnotating annotation = renderChordsFirstFiveFretsWithMaximumHeightOfFour annotation standardTuning



renderChordsFirstFiveFretsWithMaximumHeightOfFour :: ControlAnnotation -> [Note] -> (Note -> Chord) -> Note -> [Char]
renderChordsFirstFiveFretsWithMaximumHeightOfFour a t f r = concat $ union (renderChords' a t f r) (renderChords' a (map sharp t) f r)

noteToChromaticIndex note = fromJust (findIndex (flip equiv note) chromaticScale)

renderChords' annotate_notes tuning chordForm chordRoot = map unlines $ intersperse ["       "] $ map Data.List.transpose $ 
  map (\(v,b) -> renderFretBoardHorizontal chordRoot chordForm annotate_notes tuning v b) (zip (chordPositionsVertical) [0..])
    where chordPositionsVertical = positionsVertical (chordRoot,chordForm) tuning

renderFretBoardHorizontal chordRoot chordForm annotate_notes tuning strings iteration = map (\(pos,stringIndex) -> renderString annotate_notes maximumPosition pos iteration (tuning!!stringIndex)) (zip strings [0..])
  where
  maximumPosition = maximum $ (map maximum) chordPositionsVertical
  chordPositionsVertical = positionsVertical (chordRoot,chordForm) tuning

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
        
positionsVertical chord tuning = map (map fromJust) $ map (map (uncurry (flip elemIndex))) $ map (zipWith (,) (firstFourFretsVertical tuning)) (notesVertical chord tuning)

notesVertical chord tuning = sequence $ map (filter (flip elem (extractChord chord))) (firstFourFretsVertical tuning)

firstFourFretsVertical :: [Note] -> [[Note]]
firstFourFretsVertical tuning = Data.List.transpose (firstFourFrets tuning)

extractDegrees noteChordTuple = map (+ (noteToChromaticIndex (fst noteChordTuple))) $ map degreeToChromaticIndex  $ map fst $ degrees $ (snd noteChordTuple) (fst noteChordTuple)
extractChord noteChordTuple = map snd $ (\(x,y)-> degrees (y x)) noteChordTuple

degreeToChromaticIndex degree = fromJust (findIndex (flip equiv degree) degreeScale)

firstFourFrets tuning = take 4 (frets tuning)

frets tuning = map (\n -> (map (canonize . applyNTimes sharp n) tuning)) [0..] 

tuningAndPosToNote tuning pos = canonize $ applyNTimes sharp pos tuning

applyNTimes f n x = iterate f x !! n

abbreviateNote x = "cCdDefFgGaAb" !! fromJust (elemIndex x chromaticScale)
    
chromaticScale = [C,sharp C,D,sharp D,E,F,sharp F,G,sharp G,A,sharp A,B]
degreeScale = iterate (noteMap sharp) First