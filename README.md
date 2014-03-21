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
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord True AnnotatePositionVertical False True False standardTuning (minorChord C) 4 1)
Fret: 3
|S3--
|S3--
|S--5
0S---
|S-4-
|S3--

Fret: 3
3--
3--
--5
--5
-4-
3--
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord False AnnotateMarking False True True dropD (majorChord F) 4 0)
=o====
----**
---*--
*-*---

Fret: 1
----**
---*--
***---
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord False AnnotatePositionHorizontal False False True (reverse standardTuning) (majorChord G) 4 0)
=432==
------
----1-
5----0

==32==
------
----1-
54---0
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord False AnnotatePositionVertical False True True ukelele (majorChord C) 4 0)
000=
----
----
---3

0=0=
----
----
-3-3
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord False AnnotateNote False True True standardTuning (majorScale B) 4 0)
E===BE
-bea--
gBE-dg
---b--
Prelude Music.Instrument.Chord> putStrLn (renderGuitarChord False AnnotateNote True True True standardTuning E 13 0)
0 E====E
1 ------
2 --E---
3 ------
4 ------
5 ----E-
6 ------
7 -E----
8 ------
9 ---E--
10------
11------
12E----E
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
Prelude Music.Instrument.Chord> putStrLn (head $ findPositionPatterns True (majorChord C) standardTuning 4)
[[[0],[3],[2],[0],[1],[0]],[[0],[3],[2],[0],[1],[3]],[[3],[3],[2],[0],[1],[0]],[[3],[3],[2],[0],[1],[3]]]
```
