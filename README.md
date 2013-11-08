Instrument Chord
=====

Render music chords on a guitar and piano

cabal install instrument-chord

Example:

```
$ghci

Prelude>:m + Music.Instrument.Chord Music.Diatonic.Chord
Prelude Music.Instrument.Chord Music.Diatonic.Chord>  putStrLn (renderChords  (C , majorChord  ))

o--o-o
----*-
--*---
-*----
------
       
o--o--
----*-
--*---
-*---*
------
       
---o-o
----*-
--*---
**----
------
       
---o--
----*-
--*---
**---*
------
```
