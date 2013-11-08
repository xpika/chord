Instrument Chord
=====

Render music chords on a guitar and piano

* Requires https://github.com/xpika/music-diatonic

Example:

```
Prelude Music.Diatonic Music.Instrument.Chord Music.Diatonic.Chord>  putStrLn (renderChords  (C , majorChord  ))

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
