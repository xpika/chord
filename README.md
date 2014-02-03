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
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChords AnnotateMarking False standardTuning (minorChord B) 4)
Fret: 2
*---
-*--
--*-
--*-
*---
*---
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChords AnnotateMarking True dropD (majorChord F) 4)
=o====
----**
---*--
*-*---

======
----**
---*--
***---
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChords AnnotateNote True standardTuning (majorChord G) 4)
==DGB=
------
-B----
G----G

==DG==
------
-B----
G---DG
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChords AnnotatePositionVertical True ukelele (majorChord C) 4)
000=
----
----
---3

0=0=
----
----
-3-3
Prelude Music.Instrument.Chord> putStrLn (renderPianoChord AnnotateNote (majorChord C))
 ____________________ 
| | || | | | || || | |
| |_||_| | |_||_||_| |
|C |  |E |  |G |  |  |
|__|__|__|__|__|__|__|
Prelude Music.Instrument.Chord> putStrLn (renderPianoChord AnnotatePositionHorizontal (majorChord A))
 ____________________ 
| |1|| | | | || || | |
| |_||_| | |_||_||_| |
|  |  |4 |  |  |9 |  |
|__|__|__|__|__|__|__|

```
