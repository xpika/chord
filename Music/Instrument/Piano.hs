module Music.Instrument.Piano where 

import Data.List
import Data.Maybe
import Music.Instrument.Common

import Music.Diatonic hiding (transpose)
import Music.Diatonic.Note hiding (transpose)
import Music.Diatonic.Degree
import Music.Diatonic.Chord
import Music.Instrument.Coordinate

renderPianoConcept octaves annotation chord  = renderPianoPositions octaves annotation degrees
  where degrees = extractDegrees' chord

renderPianoPositions octaves annotation positions = foldl (markPiano annotation) (nOctavePianoTextDiagram octaves') positions
  where octaves' = if octaves == -1 then maximum positions `div` 12 else octaves

markPiano marking piano position = replaceAtText (getMultiOctavePianoPositionCharacterIndex position) marking' piano
  where marking' = case marking of {
     AnnotateMarking -> '*'
    ;AnnotatePositionHorizontal -> head (show position)
    ;AnnotatePositionVertical -> '1'
    ;AnnotateNote -> abbreviateNote $ tuningAndPosToNote C position
   }

cleanPiano = map (\x -> if elem x pianoMarkings then ' ' else x ) markedPiano

getMultiOctavePianoPositionCharacterIndex pos = ((octave * pianoOctaveTextDiagramSegmentWidth) + xcoordinate,ycoordinate)
  where (xcoordinate,ycoordinate) = unaryTextToCoordinate markedPiano (getPianoPositionCharacterIndex subPosition)
        (octave,subPosition) = positionToOctaveAndSubposition pos

positionToOctaveAndSubposition pos = divMod pos chromaticScaleLength

getPianoPositionCharacterIndex pos = fromJust (elemIndex (pianoMarkings !! pos) markedPiano)

pianoMarkings = ['a'..'l']

nOctavePianoTextDiagram n = nOctavePianoTextDiagrams n !! n
nOctavePianoTextDiagrams n = iterate addOctaveToPianoTextDiagram cleanPiano

addOctaveToPianoTextDiagram pianoTextDiagram 
  = unlines $ zipWith (++) (lines cleanPiano) ( map (drop 1) $ lines pianoTextDiagram)

interleave = concat . transpose

pianoOctaveTextDiagramSegmentLength = length (concat pianoOctaveTextDiagramSegment)
pianoOctaveTextDiagramSegmentWidth = length (head pianoOctaveTextDiagramSegment)
pianoOctaveTextDiagramSegment = transpose $ init $ transpose $ lines $ markedPiano

markedPiano = unlines [
  " ____________________ "
 ,"| |b||d| | |g||i||k| |"
 ,"| |_||_| | |_||_||_| |"
 ,"|a |c |e |f |h |j |l |"
 ,"|__|__|__|__|__|__|__|"
 ]
        

