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
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord AnnotateMarking True False standardTuning (minorChord C) 4 1)
Fret: 3
*---
*---
--*-
--*-
-*--
*---
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord AnnotateMarking True True dropD (majorChord F) 4 0)
=o====
----**
---*--
*-*---

======
----**
---*--
***---
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord AnnotatePositionHorizontal False True (reverse standardTuning) (majorChord G) 4 0)
=432==
------
----1-
5----0

==32==
------
----1-
54---0
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord AnnotatePositionVertical True True ukelele (majorChord C) 4 0)
000=
----
----
---3

0=0=
----
----
-3-3
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord AnnotateNote True True standardTuning (majorScale B) 4 0)
E===BE
-bea--
gBE-dg
---b--
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord AnnotateNote True True standardTuning E 13 0)
E====E
------
--E---
------
------
----E-
------
-E----
------
---E--
------
------
E----E
Prelude Music.Instrument.Chord> putStrLn (renderPianoChord 0 AnnotateMarking (majorChord C))
 ____________________ 
| | || | | | || || | |
| |_||_| | |_||_||_| |
|* |  |* |  |* |  |  |
|__|__|__|__|__|__|__|
Prelude Music.Instrument.Chord> putStrLn (renderPianoChord 1 AnnotateNote (majorChord A))
 ____________________ ____________________ 
| | || | | | || || | | |d|| | | | || || | |
| |_||_| | |_||_||_| | |_||_| | |_||_||_| |
|  |  |  |  |  |A |  |  |  |E |  |  |  |  |
|__|__|__|__|__|__|__|__|__|__|__|__|__|__|
Prelude Music.Instrument.Chord> putStrLn (head $ findPositionPatterns (majorChord C) standardTuning 4)
[[[0],[3],[2],[0],[1],[0]],[[0],[3],[2],[0],[1],[3]],[[3],[3],[2],[0],[1],[0]],[[3],[3],[2],[0],[1],[3]]]
```
