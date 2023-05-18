module Data.Music.Note
  ( Note(..)
  , dot

  , Octave(..)
  ) where

import           Flipstone.Prelude
import           Data.Music.Pitch (Pitch)
import           Data.Music.Time (Duration, dotted)

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

dot :: Note -> Note
dot (Note d p c) = Note (dotted d) p c
dot (Rest d)     = Rest (dotted d)
