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
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChords majorChord B)
==o===
**---*
------
---**-
       
======
**---*
------
--***-

Prelude Music.Instrument.Chord> putStrLn (renderMajorChordsWithTuning dropD F)
=o====
----**
---*--
*-*---
       
======
----**
---*--
***---
====oo
---*--
***---
Prelude Music.Instrument.Chord> putStrLn (renderChordsAnnotating AnnotateNote majorChord G)
==dgb=
------
-b----
g----g
       
==dg==
------
-b----
g---dg
Prelude Music.Instrument.Chord> putStrLn (renderChordsAnnotating AnnotatePosition majorChord D)
=00===
------
2--2-2
----3-
Prelude Music.Instrument.Chord> putStrLn (renderPianoChord AnnotateNote majorChord C)
 ____________________ 
| | || | | | || || | |
| |_||_| | |_||_||_| |
|c |  |e |  |g |  |  |
|__|__|__|__|__|__|__|
Prelude Music.Instrument.Chord> putStrLn (renderPianoChord AnnotatePosition majorChord A)
 ____________________ 
| |1|| | | | || || | |
| |_||_| | |_||_||_| |
|  |  |4 |  |  |9 |  |
|__|__|__|__|__|__|__|

```
