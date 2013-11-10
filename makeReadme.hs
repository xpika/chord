import System.IO
import Music.Instrument.Chord 

main = do file <- readFile "README.md.template"
          let chordsOutPut =  (renderChords C majorChord)
          let modifiedFile = unlines $ insertAt 14 chordsOutPut (lines file)
          writeFile "README.md" modifiedFile

insertAt = (\n x xs -> case splitAt n xs of { (a, b) -> a ++ [x] ++ b })
