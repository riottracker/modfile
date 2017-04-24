module Codec.Tracker.Common where

import Text.Printf
 
data Tone = C | Csharp | D | Dsharp | E | F | Fsharp | G | Gsharp | A | Asharp | B
  deriving (Eq, Enum)

instance Show Tone where
  show C      = "C"
  show Csharp = "C#"
  show D      = "D"
  show Dsharp = "D#"
  show E      = "E"
  show F      = "F"
  show Fsharp = "F#"
  show G      = "G"
  show Gsharp = "G#"
  show A      = "A"
  show Asharp = "A#"
  show B      = "B"

data Pitch = Pitch Tone Int
  deriving (Show, Eq)

instance Enum Pitch where
  toEnum             n = Pitch (toEnum $ n `mod` 12) (n `div` 12)
  fromEnum (Pitch t o) = 12 * o + fromEnum t

data Note = Note Pitch | NoteCut | NoteOff | NoteFade
  deriving (Eq)

instance Show Note where
  show (Note (Pitch t o)) = printf "%2s%d" (show t) o
  show NoteCut            = "xxx"
  show NoteOff            = "###"
  show NoteFade           = "///"
 
