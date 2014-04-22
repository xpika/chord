{-# LANGUAGE TemplateHaskell #-} 
import System.IO
import Music.Instrument.Chord 
import Music.Instrument.Common
import Language.Haskell.TH 
import Language.Haskell.Meta.Parse.Careful 
import Data.Either
import GetExpression


main = do file <- readFile "README.md.template"
          let fileLines = lines file
          let modifiedFile = unlines $ insertAt (length fileLines -1) (
                                  (makeGhciLine (expressions!!0)) ++ "\n" ++ $(return (getExpression (expressions!!0))) 
                                 ++(makeGhciLine (expressions!!1)) ++ "\n" ++ $(return (getExpression (expressions!!1))) 
                                 ++(makeGhciLine (expressions!!2)) ++ "\n" ++ $(return (getExpression (expressions!!2))) 
                                 ++(makeGhciLine (expressions!!3)) ++ "\n" ++ $(return (getExpression (expressions!!3))) 
                                 ++(makeGhciLine (expressions!!4)) ++ "\n" ++ $(return (getExpression (expressions!!4)))
                                ++(makeGhciLine (expressions!!5)) ++ "\n" ++ $(return (getExpression (expressions!!5)))
                                ++(makeGhciLine (expressions!!6)) ++ "\n" ++ $(return (getExpression (expressions!!6)))
                                ++(makeGhciLine (expressions!!7)) ++ "\n" ++ $(return (getExpression (expressions!!7)))
                                ++(makeGhciLine (expressions!!8)) ++ "\n" ++ show ($(return (getExpression (expressions!!8))))
                                ++(makeGhciLine (expressions!!9)) ++ "\n" ++ show ($(return (getExpression (expressions!!9))))
                              )
                             fileLines
          writeFile "README.md" modifiedFile

