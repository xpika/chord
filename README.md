Instrument Chord
=====

Render music chords on a guitar

```
$cabal install instrument-chord
```

Example:

```
$ghci

Prelude>:m + Music.Instrument.Chord 
Prelude Music.Instrument.Chord> putStrLn (renderChords  (C , majorChord  ))
-oo---
------
*--*-*
----*-

```
