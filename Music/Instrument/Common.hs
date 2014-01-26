module Music.Instrument.Common where 

import Music.Diatonic
import Music.Diatonic.Note
import Music.Diatonic.Degree
import Music.Diatonic.Chord
import Data.List
import Data.Maybe
import Data.Char



data ControlAnnotation = AnnotateNote | AnnotatePosition | AnnotateMarking
