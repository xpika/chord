{-# LANGUAGE FlexibleContexts #-}
module Music.Instrument.Chord 
(
 renderChords
 ,
 renderChordsAnnotatingNotes
 ,
 module Music.Diatonic
 ,
 module Music.Diatonic.Chord
 ,
 standardTuning
 ,
 dropD
 , 
 renderMajorChordsWithTuning
)
where

import Music.Diatonic 
import Music.Diatonic.Chord
import Data.List
import Data.Maybe


dropD = [D,A,D,G,B,E]


renderMajorChordsWithTuning tuning = renderChordsWithTuning tuning majorChord 

renderMajorChords note = renderChords majorChord note

renderChordsWithTuning tuning = renderChordsFirstFiveFretsWithMaximumHeightOfFour  False tuning

renderChordsAnnotatingNotes :: Deg s Note => (a -> s) -> a -> [Char]
renderChordsAnnotatingNotes = renderChordsFirstFiveFretsWithMaximumHeightOfFour  True standardTuning

renderChords :: Deg s Note => (a -> s) -> a -> [Char]
renderChords = renderChordsFirstFiveFretsWithMaximumHeightOfFour False standardTuning

renderChordsFirstFiveFretsWithMaximumHeightOfFour a t f r = concat $  union (renderChords' a t f r) (renderChords' a (map sharp t) f r)

renderChords' :: Deg s Note => Bool -> [Note] -> (a -> s) -> a -> [[Char]]
renderChords' annotate_notes tuning chordForm chordRoot  = map unlines $ intersperse ["       "] $ map Data.List.transpose $ 
  map (renderFretBoardHorizontal chordRoot chordForm annotate_notes tuning) (zipWith zip chordPositionsVertical chordNotesVertical)
    where chordPositionsVertical = positionsVertical (chordRoot,chordForm) tuning
          chordNotesVertical = notesVertical (chordRoot,chordForm) tuning

renderFretBoardHorizontal chordRoot chordForm annotate_notes tuning = map (\(pos,n) -> renderString maximumPosition (char n) pos)
  where
  char n = case annotate_notes of True -> (Just (head $ show n))
                                  False -> Nothing
  maximumPosition = maximum $ (map maximum) chordPositionsVertical
  chordPositionsVertical = positionsVertical (chordRoot,chordForm) tuning

renderString max char n =  modifyHead ((\x -> if x=='-' then '-' else openChar))  (map (\x->if x==n then (closedChar) else ('-')) [0..max])
  where (openChar,closedChar) = case char of { Just c -> (c,c);_ -> ('o','*')}

positionsVertical :: Deg s Note => (a, a -> s) -> [Note] -> [[Int]]
positionsVertical chord tuning = map (map fromJust) $ map (map (uncurry (flip elemIndex))) $ map (zipWith (,) (firstFourFretsVertical tuning)) (notesVertical chord tuning)

notesVertical chord tuning = sequence $ map (filter (flip elem (extractChord chord))) (firstFourFretsVertical tuning)

firstFourFretsVertical :: [Note] -> [[Note]]
firstFourFretsVertical tuning = Data.List.transpose (firstFourFrets tuning)

modifyHead f [] = []
modifyHead f xs = f (head xs):tail xs

extractChord noteChordTuple = map snd $ degrees $ (snd noteChordTuple) (fst noteChordTuple)
firstFourFrets tuning = take 4 (frets tuning)

frets :: [Note] -> [[Note]]
frets tuning = map (\n -> (map (canonize . applyNTimes sharp n) tuning)) [0..] 

applyNTimes f n x = iterate f x !! n

standardTuning = [E,A,D,G,B,E]
