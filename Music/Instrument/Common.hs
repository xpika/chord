{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

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
import Music.Diatonic.Scale

class NewNotes a where
  newNotes :: a -> [Note]

instance NewNotes Chord where
  newNotes = notes

instance NewNotes Scale where
  newNotes = notes

instance NewNotes NewScale where
  newNotes (NewScale chord) = notes chord

instance NewNotes Note where
  newNotes n = [n]

instance NewNotes [Note] where
  newNotes n = n

instance NewNotes [Int] where
  newNotes = map intervalToNote 

class NewIntervals a where
  newIntervals :: NewNotes a => a -> [Int] 

instance NewIntervals [Int] where
  newIntervals = id

instance NewIntervals a where
  newIntervals =  notesToIntervals . newNotes

data NewScale = NewScale Chord

chordToScale = NewScale

intervalToNote interval = chromaticScale !! (interval `rem` chromaticScaleLength)
notesToIntervals notes = concatMap (\(o,y) -> map (\i -> i + (o*chromaticScaleLength)  ) y ) $ zip [0..] (properGroup (\x y -> x > y) (map noteToChromaticIndex notes))

data ControlAnnotation = AnnotateNote | AnnotatePositionVertical | AnnotatePositionHorizontal | AnnotateMarking

abbreviateNote x = "CdDeEFgGaAbB" !! fromJust (elemIndex x chromaticScale)
    
chromaticScale = [C,sharp C,D,sharp D,E,F,sharp F,G,sharp G,A,sharp A,B]

chromaticScaleLength = length chromaticScale

tuningAndPosToNote stringTuning pos = canonize $ applyNTimes sharp pos stringTuning

applyNTimes f n x = iterate f x !! n

noteToChromaticIndex note = fromJust (findIndex (flip equiv note) chromaticScale)

extractDegrees' concept = map (+ (noteToChromaticIndex root')) $ map (semitones . distance root' ) notes'
 where 
 root' = head notes'
 notes' = newNotes concept

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

horizontalConcat str1 str2 = unlines $ horizontalConcat' (lines str1) (lines str2)
horizontalConcat' str1 str2 = zipWith (++) str1 str2

deepenListOfListsAndAddEmpties =  addEmpties . deepenListOfLists
deepenListOfLists = map deepenList

deepenList = map (:[])
addEmpties = map ([]:)
addEmptiesToEmpties xs = map (\x -> applyIf (null x) ([]:) x) xs

findIndicess p xs ys = map (\x -> findIndices (p x) xs) ys

demoEquiv string number = length string == (length (show number))

uniqueBy eq l = uniqueBy' l []
  where
    uniqueBy' [] _ = []
    uniqueBy' (y:ys) xs
      | elem_by eq y xs = uniqueBy' ys xs
      | otherwise = y : uniqueBy' ys (y:xs)

elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
elem_by _ _ [] = False
elem_by eq y (x:xs) = x `eq` y || elem_by eq y xs

applyIf :: Bool -> (a -> a) -> a -> a
applyIf p f v = if p then f v else v

insertAt = (\n x xs -> case splitAt n xs of { (a, b) -> a ++ [x] ++ b })

properGroup f (x:xs) = properGroup' f [x] xs
properGroup f [] = [] 
properGroup' f buf@(_:_) (x:xs) = if f x (last buf) then properGroup' f (buf++[x]) xs
                                                    else buf : properGroup' f [x] (xs)
properGroup' f buf [] = [buf]
