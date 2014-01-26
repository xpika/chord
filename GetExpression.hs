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
  "renderChords majorChord B"
 ,"renderMajorChordsWithTuning dropD F"
 ,"renderChordsAnnotating AnnotateNote majorChord G"
 ,"renderChordsAnnotating AnnotatePosition majorChord D"
 ,"renderPianoChord majorChord C"
 ,"renderPianoChord majorChord A"
 ]

 