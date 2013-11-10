{-# LANGUAGE TemplateHaskell #-} 
import System.IO
import Music.Instrument.Chord 
import Language.Haskell.TH 
import Language.Haskell.Meta.Parse.Careful 
import Data.Either
import GetExpression

makeGhciLine x = "Prelude Music.Instrument.Chord> putStrLn ("++x++")"


main = do file <- readFile "README.md.template"
          let chordsOutPut = $(return (getExpression (expressions!!0) ))
          let modifiedFile = unlines $ insertAt 14 chordsOutPut (lines file)
          writeFile "README.md" modifiedFile

insertAt = (\n x xs -> case splitAt n xs of { (a, b) -> a ++ [x] ++ b })
