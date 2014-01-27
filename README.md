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
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChords AnnotateMarking standardTuning (majorChord B))
--*---
**---*
------
---**-

------
**---*
------
--***-
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChords AnnotateMarking dropD (majorChord F))
=o====
----**
---*--
*-*---

======
----**
---*--
***---
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChords AnnotateNote standardTuning (majorChord G))
==DGB=
------
-B----
G----G

==DG==
------
-B----
G---DG
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChords AnnotatePosition standardTuning (majorChord D))
=00===
------
2--2-2
----3-
Prelude Music.Instrument.Chord> putStrLn (renderPianoChord AnnotateNote (majorChord C))
 ____________________ 
| | || | | | || || | |
| |_||_| | |_||_||_| |
|C |  |E |  |G |  |  |
|__|__|__|__|__|__|__|
Prelude Music.Instrument.Chord> putStrLn (renderPianoChord AnnotatePosition (majorChord A))
 ____________________ 
| |1|| | | | || || | |
| |_||_| | |_||_||_| |
|  |  |4 |  |  |9 |  |
|__|__|__|__|__|__|__|

```
