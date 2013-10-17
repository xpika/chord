import Music.Diatonic 
import Music.Diatonic.Chord
import Data.List
import Data.Maybe

standardTuningFirstFret = [E,A,D,G,B,E]

applyNTimes f 0 x = x
applyNTimes f n x = applyNTimes f (n-1) (f x)

standardTuningFrets = map (\n -> (map (canonize . applyNTimes sharp n) standardTuningFirstFret)) [0..] 

standardTuningFirstFourFrets = take 4 standardTuningFrets
standardTuningFirstFourFretsStrings  = Data.List.transpose standardTuningFirstFourFrets 

majorFingerings chord = sequence $ map (filter (flip elem (extractChord chord) )) ( standardTuningFirstFourFretsStrings )

chordPositions chord =  map (map fromJust ) $ map (map (uncurry (flip elemIndex) )) $ map (zipWith (,) standardTuningFirstFourFretsStrings   )  (majorFingerings chord)

renderString :: Int -> String
renderString  =   (\n -> map (\x->if x==n then 'x' else '-') [1,2,3,4]) 

renderChords chord = concat $ map  ( unlines) $ intersperse ["       "] $  map Data.List.transpose $ map ( map renderString) (chordPositions chord)

extractChord (note,chord) = map snd $ degrees $ chord note



