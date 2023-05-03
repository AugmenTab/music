module Data.Music.Invertible
  ( Invertible(..)
  ) where

-- Invertible defines a music type that can be inverted - that is, it can be
-- reflected or "flipped" over some value.
--
-- For intervals, this is the note the interval is "comparing" itself against.
-- A major 2nd "up" from a note is the same as a minor 6th "down" from the same
-- note an octave higher.
--
-- For scales, it's the tonic note from which all the other notes are derived.
-- Each note is inverted around that note to determine the inverted scale.
--
-- For chords, it's inverted around the second-lowest-sounding pitch in the
-- chord. In root position, the bass (root) note is inverted when in second
-- inversion, where the root note is now the highest-sounding note.
class Invertible a where
  invert :: a -> a
