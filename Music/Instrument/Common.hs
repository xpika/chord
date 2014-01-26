module Music.Instrument.Common where 

import Music.Diatonic
import Music.Diatonic.Note
import Music.Diatonic.Degree
import Music.Diatonic.Chord
import Data.List
import Data.Maybe
import Data.Char

data ControlAnnotation = AnnotateNote | AnnotatePosition | AnnotateMarking

abbreviateNote x = "cCdDefFgGaAb" !! fromJust (elemIndex x chromaticScale)
    
chromaticScale = [C,sharp C,D,sharp D,E,F,sharp F,G,sharp G,A,sharp A,B]

tuningAndPosToNote tuning pos = canonize $ applyNTimes sharp pos tuning

applyNTimes f n x = iterate f x !! n

noteToChromaticIndex note = fromJust (findIndex (flip equiv note) chromaticScale)

extractDegrees noteChordTuple = map (+ (noteToChromaticIndex (fst noteChordTuple))) $ map degreeToChromaticIndex  $ map fst $ degrees $ uncurry (flip ($)) $ noteChordTuple

degreeToChromaticIndex degree = fromJust (findIndex (flip equiv degree) degreeScale)

degreeScale = iterate (noteMap sharp) First

levelChord = map (flip mod (12::Int))