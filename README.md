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
Prelude Music.Instrument.Chord> putStrLn (renderPianoChord majorChord C)
 ____________________ 
| | || | | | || || | |
| |_||_| | |_||_||_| |
|* |  |* |  |* |  |  |
|__|__|__|__|__|__|__|
Prelude Music.Instrument.Chord> putStrLn (renderPianoChord majorChord A)
 ____________________ 
| |*|| | | | || || | |
| |_||_| | |_||_||_| |
|  |  |* |  |  |* |  |
|__|__|__|__|__|__|__|

```
