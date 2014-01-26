{-# LANGUAGE FlexibleContexts #-}
module Music.Instrument.Chord 
(
 renderChords
 ,
 renderChordsAnnotating
 ,
 module Music.Diatonic
 ,
 module Music.Diatonic.Chord
 ,
 standardTuning
 ,
 dropD
 , 
 renderMajorChordsWithTuning
 ,
 ControlAnnotation (..)
 ,
 renderPianoChord
 ,
 module Music.Diatonic.Note
 ,
 module Music.Diatonic.Degree
)
where

import Music.Diatonic
import Music.Diatonic.Note
import Music.Diatonic.Degree
import Music.Diatonic.Chord


import Music.Instrument.Internals
import Music.Instrument.Guitar
import Music.Instrument.Piano
