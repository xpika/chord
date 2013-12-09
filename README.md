Instrument Chord
=====

Render music chords on a guitar

Installation:
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
