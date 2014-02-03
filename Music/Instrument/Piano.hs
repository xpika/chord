module Music.Instrument.Piano where 

import Data.List
import Data.Maybe
import Music.Instrument.Common

import Music.Diatonic hiding (transpose)
import Music.Diatonic.Note hiding (transpose)
import Music.Diatonic.Degree
import Music.Diatonic.Chord

renderPianoChord annotation chord = renderPianoPositions annotation (levelChord degrees)
    where degrees = extractDegrees chord

renderPianoPositions annotation positions = foldl (markPiano annotation) cleanPiano positions

markPiano marking piano position = replaceAt (getPianoPositionCharacterIndex position) marking' piano
    where marking' = case marking of 
                       AnnotateMarking -> '*'
                       AnnotatePositionHorizontal -> head (show position)
                       AnnotatePositionVertical -> '1'
                       AnnotateNote -> abbreviateNote $ tuningAndPosToNote C position
    

cleanPiano = map (\x -> if elem x pianoMarkings then ' ' else x ) markedPiano

getPianoPositionCharacterIndex pos = fromJust (elemIndex (pianoMarkings !! pos) markedPiano)

pianoMarkings = ['a'..'l']

twoOctavePiano = unlines $ zipWith (++) (lines cleanPiano) ( map (drop 1) $ lines cleanPiano)

interleave = concat . transpose

markedPiano = unlines
        [
         " ____________________ "
        ,"| |b||d| | |g||i||k| |"
        ,"| |_||_| | |_||_||_| |"
        ,"|a |c |e |f |h |j |l |"
        ,"|__|__|__|__|__|__|__|"
        ]
        
replaceAt i v xs = map (\(x,i') -> if i==i' then v else x) $  zip xs [0..]

