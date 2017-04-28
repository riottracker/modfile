-- | common types
module Codec.Tracker.Common (Tone (..), Pitch (..), Note (..)) where

import Text.Printf

-- | Tones.
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

-- | Pitch representation.
data Pitch = Pitch Tone Int
  deriving (Show, Eq)

instance Enum Pitch where
  toEnum             n = Pitch (toEnum $ n `mod` 12) (n `div` 12)
  fromEnum (Pitch t o) = 12 * o + fromEnum t

-- | Represents the different note events supported by trackers. Each module loader implements its own `Enum` instance.
data Note = Note Pitch | NoteCut | NoteOff | NoteFade
  deriving (Eq)

instance Show Note where
  show (Note (Pitch t o)) = printf "%2s%d" (show t) o
  show NoteCut            = "xxx"
  show NoteOff            = "###"
  show NoteFade           = "///"
 
