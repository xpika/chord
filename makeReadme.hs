{-# LANGUAGE TemplateHaskell #-} 
import System.IO
import Music.Instrument.Chord 
import Music.Instrument.Common
import Language.Haskell.TH 
import Language.Haskell.Meta.Parse
import Data.Either
import GetExpression
import Data.List

concat' =  concat . map hConcat
  
main = do file <- readFile "README.md.template"
          let fileLines = lines file
          let modifiedFile = unlines $ insertAt (length fileLines -1) (
                             (makeGhciLine (expressions!!0)) ++ "\n" ++ concat' $(return (getExpression (expressions!!0)))
                             ++(makeGhciLine (expressions!!1)) ++ "\n" ++ concat' $(return (getExpression (expressions!!1))) 
                             ++(makeGhciLine (expressions!!2)) ++ "\n" ++ concat' $(return (getExpression (expressions!!2))) 
                             ++(makeGhciLine (expressions!!3)) ++ "\n" ++ concat' $(return (getExpression (expressions!!3))) 
                             ++(makeGhciLine''' (expressions!!4)) ++ "\n" ++ $(return (getExpression (expressions!!4)))
                             ++(makeGhciLine (expressions!!5)) ++ "\n" ++ concat' $(return (getExpression (expressions!!5)))
                             ++(makeGhciLine (expressions!!6)) ++ "\n" ++ concat' $(return (getExpression (expressions!!6)))
                             ++(makeGhciLine (expressions!!7)) ++ "\n" ++ concat' $(return (getExpression (expressions!!7)))
                             ++(makeGhciLine (expressions!!8)) ++ "\n" ++ concat' $(return (getExpression (expressions!!8)))
                             ++(makeGhciLine (expressions!!9)) ++ "\n" ++ concat' $(return (getExpression (expressions!!9)))
                             ++(makeGhciLine (expressions!!10)) ++ "\n" ++ concat' $(return (getExpression (expressions!!10)))
                             ++(makeGhciLine (expressions!!11)) ++ "\n" ++ concat' $(return (getExpression (expressions!!11)))
                             ++(makeGhciLine (expressions!!12)) ++ "\n" ++ concat' $(return (getExpression (expressions!!12)))
                             ++(makeGhciLine (expressions!!13)) ++ "\n" ++ concat' $(return (getExpression (expressions!!13)))
                             ++(makeGhciLine (expressions!!14)) ++ "\n" ++ concat' $(return (getExpression (expressions!!14)))
                             ++(makeGhciLine (expressions!!15)) ++ "\n" ++ concat' $(return (getExpression (expressions!!15)))
                             ++(makeGhciLine (expressions!!16)) ++ "\n" ++ concat' $(return (getExpression (expressions!!16)))
                             ++(makeGhciLine (expressions!!17)) ++ "\n" ++ concat' $(return (getExpression (expressions!!17)))
                             ++(makeGhciLine' (expressions!!18)) ++ "\n" ++ $(return (getExpression (expressions!!18)))
                             ++(makeGhciLine' (expressions!!19)) ++ "\n" ++ $(return (getExpression (expressions!!19)))
                             ++(ghciLinePrompt ++ (expressions!!20)) ++ "\n" ++ show ($(return (getExpression (expressions!!20))))
                             ) fileLines
          writeFile "README.md" modifiedFile


