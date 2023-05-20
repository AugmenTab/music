module Data.Music.Note
  ( Note(..)
  , noteToText
  , dot

  , Octave
  , mkOctave
  ) where

import           Flipstone.Prelude
import           Data.Music.Pitch (Pitch)
import           Data.Music.Time (Duration, dotted)

import qualified Data.Text as T
import           Numeric.Natural (Natural)
import           Text.Show (Show(..))

-- | The representation of a musical note - either a note that sounds, with a
-- pitch and an octave, or a rest. Both have a duration.
data Note
  = Note Duration Pitch Octave
  | Rest Duration
  deriving stock (Eq)

instance Show Note where
  show (Note d p o) = "Note " <> show d <> " " <> show p <> show o
  show (Rest d)     = "Rest " <> show d

noteToText :: Note -> T.Text
noteToText = T.pack . show

-- | Takes a Note and "dots" its duration - that is, increases its duration by
-- 50%.
dot :: Note -> Note
dot (Note d p c) = Note (dotted d) p c
dot (Rest d)     = Rest (dotted d)

-- | Represents what octave a Note's Pitch will sound in. Currently, this only
-- supports Octaves 0 through 8. Octaves can be built using 'mkOctave'.
newtype Octave = Octave Natural
  deriving newtype (Enum, Eq, Show)

instance Bounded Octave where
  minBound = Octave 0
  maxBound = Octave 8

-- | Attempts to build
-- supports Octaves 0 through 8. Octaves can be built using 'mkOctave'.
mkOctave :: Integral a => a -> Either T.Text Octave
mkOctave n
  | n < 0     = Left "Octaves below 0 are not supported."
  | n > 8     = Left "Octaves above 8 are not supported."
  | otherwise = Right . Octave $ fromIntegral n
