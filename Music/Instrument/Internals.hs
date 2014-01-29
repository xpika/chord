module Music.Instrument.Internals where 

import Music.Diatonic
import Music.Diatonic.Note
import Music.Diatonic.Degree
import Music.Diatonic.Chord
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import qualified Data.Set

import Music.Instrument.Guitar
import Music.Instrument.Piano
import Music.Instrument.Common


        
positionPatterns chord tuning count = positionPatterns' chord tuning [0..] count
        
positionPatterns' chord tuning froms count = 
  scanl1 (flip (\\)) ( map (\x-> positionPatterns'' chord tuning x count) froms)
        
positionPatterns'' chord tuning from count = 
  map ( map ( (+from) . fromJust) . map (uncurry (flip elemIndex))) $ map (zipWith (,) (frettedGuitarStringsLengths from count tuning)) (notePatterns chord tuning from count)

notePatterns chord tuning from count = 
  sequence $ map (filter (flip elem (chordToNotes chord))) (frettedGuitarStringsLengths from count tuning)

frettedGuitarStringsLengths from count = map (take count . drop from) . frettedGuitarStrings
frettedGuitarStrings tuning = map fret tuning
fret tune = map (\n -> canonize . applyNTimes sharp n $ tune) [0..]

positionsAndTuningToNotes tuning positions = zipWith tuneAndPositionToNote tuning positions
tuneAndPositionToNote tune position =  fret tune !! position



