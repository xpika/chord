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
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept True AnnotatePositionVertical False True False standardTuning (minorChord C) 4 1 True False [] False False False)
Fret: 3
|S3---
|S-4--
0S----
|S--5-
|S3---
|S3---

Fret: 3
3---
-4--
--5-
--5-
3---
3---
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept True AnnotatePositionVertical True True True standardTuning (minorChord C) 4 1 True False [] False False False)
Fret: 3
0===0==
 ^^^^^^
333---3
4----4-
5--5---
6------

Fret: 3
333---3
4----4-
5--55--
6------
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateMarking False True True dropD (majorChord F) 4 0 True False [] False False False)
=o====
----**
---*--
*-*---

Fret: 1
----**
---*--
***---
------
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateNote False True True standardTuning (majorScale F) 4 0 False False [] True True True)
=AD===
Fb----
--E---
GC----
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateNote False True True standardTuning (shiftOctave 1 $ convertToSteps $ majorScale F) 4 0 False False [] True True True)
===G=E
----C-
---A--
--FbD-
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateNote False True True standardTuning (chordToScale (majorChord F)) 4 0 False False [] False True True)
=A====
F-----
------
-C----
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotatePositionHorizontal False False True (reverse standardTuning) (majorChord G) 4 0 True False [] False False False)
=234==
------
----5-
1----6

==34==
------
----5-
12---6
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateNote False True True standardTuning (sus4 $ dominant7thChord G) 4 3 False True [] False True False)
Fret: 3
GCF-DG
------
---C--
------

Fret: 3
G-F-DG
------
-D-C--
------

Fret: 3
G-F--G
------
-D-C--
----F-

Fret: 3
G----G
------
-DGC--
----F-
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateNote False True True standardTuning (minorChord G) 4 1 False False lightChord False False False)
Fret: 3
---bDG
------
------
------
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateNote False True True standardTuning (minor7thChord A) 4 2 False False lightChord False True False)
Fret: 5
--GCEA
------
------
------
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateMarking False True True standardTuning (majorChord D) 4 0 False True [] False False False)
==o===
------
---*-*
----*-
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateMarking True True True standardTuning (fifthChord B) 4 0 False True powerChord True False False)
0======
1------
2-*----
3------
4--**--
5------
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateNote False True True standardTuning (majorChord (flat A)) 4 0 False False [] False False False)
Fret: 1
--eaC-
------
-C----
------
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotatePositionVertical False True True ukelele (majorChord C) 4 0 True False [] False False False)
000=
----
----
---3

0=0=
----
----
-3-3
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateNote False True True standardTuning (minorPentatonicScale A) 4 5 True False [] False False False)
Fret: 5
ADGCEA
------
-EAD--
C---GC
Prelude Music.Instrument.Chord> putStrLn (renderGuitarConcept False AnnotateNote True True True standardTuning E 13 0 True False [] False False False)
 0E====E
 1------
 2--E---
 3------
 4------
 5----E-
 6------
 7-E----
 8------
 9---E--
01------
11------
21E----E
Prelude Music.Instrument.Chord> putStrLn (renderPianoConcept 0 AnnotateMarking (majorChord C))
 ____________________ 
| | || | | | || || | |
| |_||_| | |_||_||_| |
|* |  |* |  |* |  |  |
|__|__|__|__|__|__|__|
Prelude Music.Instrument.Chord> putStrLn (renderPianoConcept 1 AnnotateNote (majorScale A))
 ____________________ ____________________ 
| | || | | | || || | | |d|| | | |g||a|| | |
| |_||_| | |_||_||_| | |_||_| | |_||_||_| |
|  |  |  |  |  |A |B |  |D |E |  |  |  |  |
|__|__|__|__|__|__|__|__|__|__|__|__|__|__|
Prelude Music.Instrument.Chord> putStrLn (head $ findPositionPatterns True (majorChord C) standardTuning 4 True False [] False False)
[[[0],[3],[2],[0],[1],[0]],[[0],[3],[2],[0],[1],[3]],[[3],[3],[2],[0],[1],[0]],[[3],[3],[2],[0],[1],[3]]]
```
