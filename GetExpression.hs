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
  "renderGuitarChord True AnnotatePositionVertical False True False standardTuning (minorChord C) 4 1"
 ,"renderGuitarChord False AnnotateMarking False True True dropD (majorChord F) 4 0"
 ,"renderGuitarChord False AnnotatePositionHorizontal False False True (reverse standardTuning) (majorChord G) 4 0"
 ,"renderGuitarChord False AnnotatePositionVertical False True True ukelele (majorChord C) 4 0"
 ,"renderGuitarChord False AnnotateNote False True True standardTuning (majorScale B) 4 0"
 ,"renderGuitarChord False AnnotateNote True True True standardTuning E 13 0"
 ,"renderPianoChord 0 AnnotateMarking (majorChord C)"
 ,"renderPianoChord 1 AnnotateNote (majorChord A)"
 ,"head $ findPositionPatterns True (majorChord C) standardTuning 4"]
