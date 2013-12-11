Instrument Chord
=====

Render music chords on a guitar and piano

Installation:

```
cabal install instrument-chord
```

Example:

```
$ghci

Prelude>:m + Music.Instrument.Chord 
Prelude Music.Instrument.Chord> putStrLn (renderChords D majorChord)
-oo---
------
*--*-*
----*-

Prelude Music.Instrument.Chord> putStrLn (renderChords A minorChord)
oo---o
----*-
--**--
------
       
o----o
----*-
--**--
-*----
Prelude Music.Instrument.Chord> putStrLn (renderChordsAnnotatingNotes G majorChord)
--DGB-
------
-B----
G----G
       
--DG--
------
-B----
G---DG

```
