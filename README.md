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
Prelude Music.Instrument.Chord> putStrLn (renderChords majorChord B)
--o---
**---*
------
---**-
       
------
**---*
------
--***-

Prelude Music.Instrument.Chord> putStrLn (renderMajorChordsWithTuning dropD F)
-o----
----**
---*--
*-*---
       
------
----**
---*--
***---
----oo
---*--
***---
Prelude Music.Instrument.Chord> putStrLn (renderChordsAnnotatingNotes majorChord G)
--DGB-
------
-B----
G----G
       
--DG--
------
-B----
G---DG

```
