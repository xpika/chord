module GetExpression
{-
(getExpression)
-}
where

import Language.Haskell.TH
import Language.Haskell.Meta.Parse.Careful
import Data.Either

getExpression string = head $ rights [parseExp string]
getExpressionAndLine string = (makeGhciLine string , head $ rights [parseExp string])

makeGhciLine x = "Prelude Music.Instrument.Chord> putStrLn ("++x++")"

expressions = [
  "renderGuitarChord AnnotateMarking True False standardTuning (minorChord C) 4 1"
 ,"renderGuitarChord AnnotateMarking False True dropD (majorChord F) 4 0"
 ,"renderGuitarChord AnnotateNote False True standardTuning (majorChord G) 4 0"
 ,"renderGuitarChord AnnotatePositionVertical False True ukelele (majorChord C) 4 0"
 ,"renderGuitarChord AnnotateNote False True standardTuning (majorScale B) 4 0"
 ,"renderPianoChord 0 AnnotateMarking (majorChord C)"
 ,"renderPianoChord 1 AnnotateNote (majorChord A)"
 ,"head $ findPositionPatterns (majorChord C) standardTuning 4"]

 
