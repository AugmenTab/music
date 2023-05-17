module Data.Music.Note
  ( Note(..)

  , Octave(..)

  , Duration
  , dot, dotted
  , whole, half, quarter, eighth, sixteenth
  ) where

import           Flipstone.Prelude
import           Data.Music.Pitch (Pitch)

import           Data.Ratio (denominator, numerator, (%))
import           Numeric.Natural (Natural)
import           Text.Show (Show(..))

data Note
  = Note Duration Pitch Octave
  | Rest Duration
  deriving stock (Eq)

instance Show Note where
  show (Note d p o) = "Note " <> show d <> " " <> show p <> show o
  show (Rest d)     = "Rest " <> show d

newtype Octave = Octave Natural
  deriving newtype (Enum, Eq, Show)

instance Bounded Octave where
  minBound = Octave 0
  maxBound = Octave 8

newtype Duration = Duration (Ratio Natural)
  deriving newtype (Eq)

instance Show Duration where
  show (Duration d) =
    "Duration " <> show (numerator d) <> "/" <> show (denominator d)

dot :: Note -> Note
dot (Note d p c) = Note (dotted d) p c
dot (Rest d)     = Rest (dotted d)

dotted :: Duration -> Duration
dotted (Duration d) = Duration $ d * (3 % 2)

whole :: Duration
whole = Duration $ 1 % 1

half :: Duration
half = Duration $ 1 % 2

quarter :: Duration
quarter = Duration $ 1 % 4

eighth :: Duration
eighth = Duration $ 1 % 8

sixteenth :: Duration
sixteenth = Duration $ 1 % 16
