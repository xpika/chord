module Music.Instrument.Common where 

import Music.Diatonic
import Music.Diatonic.Note
import Music.Diatonic.Degree
import Music.Diatonic.Chord
import Data.List
import Data.Maybe
import Data.Char

data ControlAnnotation = AnnotateNote | AnnotatePosition | AnnotateMarking

abbreviateNote x = "CdDeEFgGaAbB" !! fromJust (elemIndex x chromaticScale)
    
chromaticScale = [C,sharp C,D,sharp D,E,F,sharp F,G,sharp G,A,sharp A,B]

tuningAndPosToNote tuning pos = canonize $ applyNTimes sharp pos tuning

applyNTimes f n x = iterate f x !! n

noteToChromaticIndex note = fromJust (findIndex (flip equiv note) chromaticScale)

extractDegrees chord = map (+ (noteToChromaticIndex (root cord))) $ map degreeToChromaticIndex  $ map fst $ degrees $ chord

degreeToChromaticIndex degree = fromJust (findIndex (flip equiv degree) degreeScale)

degreeScale = iterate (noteMap sharp) First

levelChord = map (flip mod (12::Int))

inversions = map sequenceDegrees . rotations

rotations = reverse . (\list -> map (\n -> (take (length list) . drop (length list -n)) (cycle list)) [1..length list])

sequenceDegrees ds = scanl1 (\x y-> x + mod (y-x) (12::Int)) ds
