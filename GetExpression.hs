module GetExpression
{-
(getExpression)
-}
where

import Language.Haskell.TH
import Language.Haskell.Meta.Parse.Careful
import Data.Either

getExpression string = head $ rights [parseExp string]
getExpressionAndLine string = (makeGhciLine string,head $ rights [parseExp string])

makeGhciLine x = ghciLinePrompt ++ "putStr $ concat $ map hConcat ("++x++")"
makeGhciLine'' x = ghciLinePrompt ++ "putStr (concat . concat . map intersperse \"\\n\") ("++x++")"
makeGhciLine' x = ghciLinePrompt ++ "putStrLn ("++x++")"
makeGhciLine''' x = ghciLinePrompt ++ "putStr $ " ++ x
ghciLinePrompt = "Prelude Music.Instrument.Chord> "

{-
 -
renderGuitarConcept 
 allowOpens 
 controlAnnotation 
 annotateFrets
 firstTuningLast
 orientationVertical
 tuning 
 chord
 maxHeight
 from
 utilizeAllStrings 
 rootNoteLowest
 selectionMask
 renderAllFrets 
 renderPressedFrets
 utilizeAllNotes
 strictIntervals
 - -}

expressions = [
  "renderGuitarConcept True AnnotatePositionVertical False True False standardTuning (minorChord C) 4 1 True False [] False False False False False"
 ,"renderGuitarConcept True AnnotatePositionVertical True True True standardTuning (minorChord C) 4 1 True False [] False False False False False"
 ,"renderGuitarConcept False AnnotateMarking False True True dropD (majorChord F) 4 0 True False [] False False False False False"
 ,"renderGuitarConcept False AnnotatePositionVertical False True True standardTuning (harmony (majorScale A)) 4 0 True False [] True True False False False"
 ,"concat . map hConcat $ renderGuitarConcept False AnnotatePositionVertical False True True standardTuning [majorChord D,majorChord A,minorChord B,majorChord G] 4 0 True False [] False False False False False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (majorScale F) 4 0 False False [] True False True True False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (shiftOctave 1 $ convertToSteps $ majorScale F) 4 0 False False [] True False True True False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (chordToScale (majorChord F)) 4 0 False False [] False False True True False"
 ,"renderGuitarConcept False AnnotatePositionHorizontal False False True (reverse standardTuning) (majorChord G) 4 0 True False [] False False False False False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (sus 4 $ dominant7thChord G) 4 3 False True [] False False True False False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (minorChord G) 4 1 False False lightChord False False False False False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (minor7thChord A) 4 2 False False lightChord False False True False False"
 ,"renderGuitarConcept False AnnotateMarking False True True standardTuning (slash C (majorChord D)) 4 0 False True [] False False False False False"
 ,"renderGuitarConcept False AnnotateMarking True True True standardTuning (fifthChord B) 4 0 False True powerChord True False False False False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (majorChord (flat A)) 4 0 False False [] False False False False False"
 ,"renderGuitarConcept False AnnotatePositionVertical False True True ukelele (majorChord C) 4 0 True False [] False False False False False"
 ,"renderGuitarConcept False AnnotateNote False True True standardTuning (minorPentatonicScale A) 4 5 True False [] False False False False False"
 ,"renderGuitarConcept False AnnotateNote True True True standardTuning E 13 0 True False [] False False False False False"
 ,"renderPianoConcept 0 AnnotateMarking (majorChord C)"
 ,"renderPianoConcept 1 AnnotateNote (majorScale A)"
 ,"map head $ findPositionPatterns True (majorChord C) standardTuning 4 True False [] False False"
 ]
