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
  "renderGuitarChords AnnotateMarking False False standardTuning (minorChord B) 4"
 ,"renderGuitarChords AnnotateMarking False True dropD (majorChord F) 4"
 ,"renderGuitarChords AnnotateNote False True standardTuning (majorChord G) 4"
 ,"renderGuitarChords AnnotatePositionVertical False True ukelele (majorChord C) 4"
 ,"renderPianoChord 0 AnnotateMarking (majorChord C)"
 ,"renderPianoChord 1 AnnotateNote (majorChord A)"
 ]

 
