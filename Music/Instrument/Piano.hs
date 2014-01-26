module Music.Instrument.Piano where 

import Data.List
import Data.Maybe
import Music.Instrument.Common


renderPiano positions = foldl (markPiano '*') cleanPiano positions

markPiano marking piano position = replaceAt (getPianoPositionCharacterIndex position) marking piano

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

