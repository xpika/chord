module Music.Instrument.Common where 

import Music.Diatonic
import Music.Diatonic.Note
import Music.Diatonic.Degree
import Music.Diatonic.Chord
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Set

data ControlAnnotation = AnnotateNote | AnnotatePositionVertical | AnnotatePositionHorizontal | AnnotateMarking

abbreviateNote x = "CdDeEFgGaAbB" !! fromJust (elemIndex x chromaticScale)
    
chromaticScale = [C,sharp C,D,sharp D,E,F,sharp F,G,sharp G,A,sharp A,B]

chromaticScaleLength = length chromaticScale

tuningAndPosToNote tuning pos = canonize $ applyNTimes sharp pos tuning

applyNTimes f n x = iterate f x !! n

noteToChromaticIndex note = fromJust (findIndex (flip equiv note) chromaticScale)

extractDegrees chord = map (+ (noteToChromaticIndex (root chord))) $ map degreeToChromaticIndex  $ map fst $ degrees $ chord

degreeToChromaticIndex degree = fromJust (findIndex (flip equiv degree) degreeScale)

degreeScale = iterate (noteMap sharp) First

levelChord = map (flip mod chromaticScaleLength)

inversions = map sequenceDegrees . rotations

rotations = reverse . (\list -> map (\n -> (take (length list) . drop (length list -n)) (cycle list)) [1..length list])

sequenceDegrees ds = scanl1 (\x y-> x + mod (y-x) chromaticScaleLength) ds

findChord inputNotes = do 
  chordType <- chordTypes
  root <- chromaticScale
  let notes = chordToNotes (chordType root)
  guard (Data.Set.isSubsetOf (Data.Set.fromList (uns inputNotes )) (Data.Set.fromList notes))
  return (chordType root) 
  where uns = map canonize

chordTypes = [majorChord, minorChord, diminishedChord, augmentedChord,
              major7thChord, dominant7thChord, minor7thChord, minorMajor7thChord, minor7thFlat5thChord, diminished7thChord, augmentedMajor7thChord]

chordToNotes chord = map snd $ degrees chord

horizontalConcat str1 str2 = (unlines) $ zipWith (++) (lines str1) (lines str2)

deepenListOfLists = map deepenList
deepenList = map (:[])

