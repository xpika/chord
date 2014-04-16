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
  "renderGuitarConcept True AnnotatePositionVertical False True False standardTuning (minorChord C) 4 1"
 ,"renderGuitarConcept False AnnotateMarking False True True dropD (majorChord F) 4 0"
 ,"renderGuitarConcept False AnnotatePositionHorizontal False False True (reverse standardTuning) (majorChord G) 4 0"
 ,"renderGuitarConcept False AnnotatePositionVertical False True True ukelele (majorChord C) 4 0"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (majorScale B) 4 0"
 ,"renderGuitarConcept False AnnotateNote True True True standardTuning E 13 0"
 ,"renderPianoConcept 0 AnnotateMarking (majorChord C)"
 ,"renderPianoConcept 1 AnnotateNote (majorScale A)"
 ,"head $ findPositionPatterns True (majorChord C) standardTuning 4"
 ]
