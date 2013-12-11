{-# LANGUAGE TemplateHaskell #-} 
import System.IO
import Music.Instrument.Chord 
import Language.Haskell.TH 
import Language.Haskell.Meta.Parse.Careful 
import Data.Either
import GetExpression


main = do file <- readFile "README.md.template"
          let fileLines = lines file
          let modifiedFile = unlines $ insertAt (length fileLines -1) (
                                  (makeGhciLine (expressions!!0)) ++ "\n" ++ $(return (getExpression (expressions!!0) ))  ++ "\n"
                                ++(makeGhciLine (expressions!!1)) ++ "\n" ++ $(return (getExpression (expressions!!1) )) 
                                ++(makeGhciLine (expressions!!2)) ++ "\n" ++ $(return (getExpression (expressions!!2) )) 
                              )
                             fileLines
          writeFile "README.md" modifiedFile

insertAt = (\n x xs -> case splitAt n xs of { (a, b) -> a ++ [x] ++ b })
