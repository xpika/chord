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
  "renderGuitarChords AnnotateMarking False standardTuning (minorChord B) 4"
 ,"renderGuitarChords AnnotateMarking True dropD (majorChord F) 4"
 ,"renderGuitarChords AnnotateNote True standardTuning (majorChord G) 4"
 ,"renderGuitarChords AnnotatePosition True ukelele (majorChord C) 4"
 ,"renderPianoChord AnnotateNote (majorChord C)"
 ,"renderPianoChord AnnotatePosition (majorChord A)"
 ]

 
