{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Music.Instrument.Common where 

import Music.Diatonic hiding (transpose)
import Music.Diatonic.Note hiding (transpose)
import Music.Diatonic.Degree
import Music.Diatonic.Chord
import Music.Diatonic.Scale
import Music.Diatonic.Harmony
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Set

class NewNotes a where
  newNotes :: a -> [Note]
  lastIntervals :: a -> NewSteps
  lastIntervals a = NewSteps se . notesToSteps . newNotes $ a
    where se = requiresSequence' a
  requiresSequence' :: a -> Bool
  getChords :: a -> Maybe [Chord]
  isSlash :: a -> Bool
  isSlash = const False 

instance NewNotes Chord where
  newNotes = notes
  requiresSequence' _ = True
  getChords = const Nothing

instance NewNotes Scale where
  newNotes = notes
  requiresSequence' _ = False
  getChords = const Nothing

instance NewNotes NewScale where
  newNotes (NewScale chord) = notes chord
  requiresSequence' _ = False
  getChords = const Nothing

instance NewNotes Note where
  newNotes n = [n]
  requiresSequence' _ = False
  getChords = const Nothing

instance NewNotes [Note] where
  newNotes n = n
  requiresSequence' _ = True
  getChords = const Nothing

instance NewNotes NewSteps where
  newNotes ns = (map stepToNote) . deStep $ ns
  lastIntervals = id
  requiresSequence' (NewSteps b _) = b
  getChords = const Nothing

instance NewNotes Harmony where
  newNotes = undefined
  requiresSequence' _ = undefined
  getChords = Just . chords 

instance NewNotes [Chord] where
  newNotes = undefined
  requiresSequence' _ = undefined
  getChords = Just

instance Nts NewChord where
  notes (NewChord inversion b c) = applyMaybe (\x y-> y:x) (x:canonize(tf y):xs) inversion
    where 
    (x:y:xs) = notes c 
    tf = case b of
        (Just (Sus 2)) -> (\x -> flat (flat x))
        (Just (Sus 4)) -> (\x -> (sharp x))
        _ -> id

instance NewNotes NewChord where
  newNotes = notes
  requiresSequence' _ = True
  getChords = const Nothing
  isSlash (NewChord i _ _) = isJust i

sus n c = NewChord Nothing (Just $ Sus n) c

slash note chord = NewChord (Just note) Nothing chord

getNoteIndexFromChord note chord = elemIndex note (notes chord)
getNoteFromChordIndex chordIndex chord = (notes chord) !! chordIndex

{-
sus2 chord = (x:canonize(flat(flat y)):xs)
  where (x:y:xs) = newNotes chord
sus4 chord = (x:canonize(sharp y):xs)
  where (x:y:xs) = newNotes chord
-}

data NewScale = NewScale Chord
data NewSteps = NewSteps Bool [Int]

data NewChord = NewChord (Maybe Note) (Maybe ChordModifier) Chord

data ChordModifier = Sus Int

applyMaybe f a m = case m of
  Just b -> f a b
  _ -> a

instance Show ChordModifier where
 show (Sus x) = "sus"++show x

instance Show NewChord where
 show (NewChord inversion modifier c) = applyMaybe (\x y -> x ++ "/" ++show y) (applyMaybe (\x y -> x++show y) (show c) modifier) inversion

stepMap f (NewSteps b d) = NewSteps b (f d)
deStep (NewSteps b xs) = xs
getSteps (NewSteps b xs) = xs

convertToSteps = lastIntervals 
shiftOctave n = shiftStep (n*12)
shiftStep n = stepMap (map (+n))

chordToScale = NewScale

stepToNote interval = chromaticScale !! (interval `rem` chromaticScaleLength)
notesToSteps notes = concatMap (\(o,y) -> map (\i -> i + (o*chromaticScaleLength)  ) y ) $ zip [0..] (properGroup (\x y -> x > y) (map noteToChromaticIndex notes))

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


overlay xs ys = map head $ transpose [xs,ys]

hAppend w w2 =   unlines 
               $ map concat
               $ transpose 
               $  map (\lines ->  
                    map (\line ->  
                        overlay line (replicate (maximum (map length lines)+1) ' ')
                    ) lines 
                  )
               $ (\documents -> map (\document -> overlay document (replicate (maximum (map length documents)) " ")) documents) [lines w,lines w2]
               
                
hConcat = foldl1 hAppend
