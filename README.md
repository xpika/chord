Instrument Chord
=====

Render music chords on instruments (Just guitar at this stage).

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
==DGB=
------
-B----
G----G
       
==DG==
------
-B----
G---DG
Prelude Music.Instrument.Chord> putStrLn (renderChordsAnnotating AnnotatePosition majorChord D)
=00===
------
2--2-2
----3-

```
