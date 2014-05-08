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
  "renderGuitarConcept True AnnotatePositionVertical False True False standardTuning (minorChord C) 4 1 True False [] False False"
 ,"renderGuitarConcept True AnnotatePositionVertical True True True standardTuning (minorChord C) 4 1 True False [] False False"
 ,"renderGuitarConcept False AnnotateMarking False True True dropD (majorChord F) 4 0 True False [] False False"
 ,"renderGuitarConcept False AnnotatePositionHorizontal False False True (reverse standardTuning) (majorChord G) 4 0 True False [] False False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (sus4 $ dominant7thChord G) 4 3 False True [] False True"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (minorChord G) 4 1 False False lightChord False False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (minor7thChord A) 4 2 False False lightChord False True"
 ,"renderGuitarConcept False AnnotateMarking False True True standardTuning (majorChord D) 4 0 False True [] False False"
 ,"renderGuitarConcept False AnnotateMarking True True True standardTuning (fifthChord B) 4 0 False True powerChord True False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (majorChord (flat A)) 4 0 False False [] False False"
 ,"renderGuitarConcept False AnnotatePositionVertical False True True ukelele (majorChord C) 4 0 True False [] False False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (majorScale B) 4 0 True False [] False False"
 ,"renderGuitarConcept False AnnotateNote True True True standardTuning E 13 0 True False [] False False"
 ,"renderPianoConcept 0 AnnotateMarking (majorChord C)"
 ,"renderPianoConcept 1 AnnotateNote (majorScale A)"
 ,"head $ findPositionPatterns True (majorChord C) standardTuning 4 True False [] False"
 ]
