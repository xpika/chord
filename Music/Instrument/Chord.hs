{-# LANGUAGE FlexibleContexts #-}
module Music.Instrument.Chord 
(
  renderChords
 ,renderChordsAnnotatingNotes
 ,module Music.Diatonic
 ,module Music.Diatonic.Chord
)
where

import Music.Diatonic 
import Music.Diatonic.Chord
import Data.List
import Data.Maybe

renderChords :: Deg s Note => a -> (a -> s) -> [Char]
renderChords = renderChords' False

renderChordsAnnotatingNotes :: Deg s Note => a -> (a -> s) -> [Char]
renderChordsAnnotatingNotes = renderChords' True

renderChords' :: Deg s Note => Bool -> a -> (a -> s) -> [Char]
renderChords' b chordRoot chordForm = concat $ map unlines $ intersperse ["       "] $ map Data.List.transpose $ 
  map (renderFretBoardHorizontal chordRoot chordForm b) (zipWith zip chordPositionsVertical chordNotesVertical)
    where chordPositionsVertical = positionsVertical (chordRoot,chordForm)
          chordNotesVertical = notesVertical (chordRoot,chordForm)

renderFretBoardHorizontal chordRoot chordForm b  = map (\(pos,n) -> renderString maximumPosition (char n) pos)
  where
  char n = case b of True -> (Just (head $ show n))
                     False -> Nothing
  maximumPosition = maximum $ (map maximum) chordPositionsVertical
  chordPositionsVertical = positionsVertical (chordRoot,chordForm)

renderString max char n =  modifyHead ((\x -> if x=='-' then '-' else openChar))  (map (\x->if x==n then (closedChar) else ('-')) [0..max])
  where (openChar,closedChar) = case char of { Just c -> (c,c);_ -> ('o','*')}

positionsVertical :: Deg s Note => (a, a -> s) -> [[Int]]
positionsVertical chord =  map (map fromJust) $ map (map (uncurry (flip elemIndex))) $ map (zipWith (,) standardTuningFirstFourFretsVertical) (notesVertical chord)

notesVertical chord = sequence $ map (filter (flip elem (extractChord chord) )) standardTuningFirstFourFretsVertical

standardTuningFirstFourFretsVertical :: [[Note]]
standardTuningFirstFourFretsVertical = Data.List.transpose standardTuningFirstFourFrets 

modifyHead f [] = []
modifyHead f xs = f (head xs):tail xs

extractChord noteChordTuple = map snd $ degrees $ (snd noteChordTuple) (fst noteChordTuple)
standardTuningFirstFourFrets = take 4 standardTuningFrets

standardTuningFrets :: [[Note]]
standardTuningFrets = map (\n -> (map (canonize . applyNTimes sharp n) standardTuningFirstFret)) [0..] 

applyNTimes f n x = iterate f x !! n

standardTuningFirstFret = [E,A,D,G,B,E]
