module Data.Music.Scale
  ( Scale(..)
  , Mode
  , scaleFromIntervals
  , scaleToIntervals

  -- Scales
  , major
  , minor
  , chromatic
  ) where

import           Flipstone.Prelude
import qualified Data.Music.Interval as I

import qualified Data.Foldable as F
import qualified Data.FixedList as FL
import qualified Data.List as L
import qualified Data.Text as T
import           Text.Show (Show(..))

-- A Scale is a list of fixed length, holding the interval values to determine
-- the 2nd through last note in the scale from the tonic. They all have a fixed
-- length of 1 less than the scale length - we don't need to determine distance
-- from the tonic for the tonic or the octave.
data Scale
  = Ditonic     (FL.FixedList1  I.Interval)
  | Tritonic    (FL.FixedList2  I.Interval)
  | Tetratonic  (FL.FixedList3  I.Interval)
  | Pentatonic  (FL.FixedList4  I.Interval)
  | Hexatonic   (FL.FixedList5  I.Interval)
  | Heptatonic  (FL.FixedList6  I.Interval)
  | Octatonic   (FL.FixedList7  I.Interval)
  | Nonatonic   (FL.FixedList8  I.Interval)
  | Decatonic   (FL.FixedList9  I.Interval)
  | Undecatonic (FL.FixedList10 I.Interval)
  | Chromatic   (FL.FixedList11 I.Interval)
  deriving stock (Eq)

instance Show Scale where
  show = T.unpack . scaleToText

-- This type synonym is a convenience for using a mode.
type Mode = Scale

scaleFromIntervals :: [I.Interval] -> Either T.Text Scale
scaleFromIntervals intervals =
  case L.length intervals of
    2  -> Right $ Ditonic     $ FL.fromFoldable' intervals
    3  -> Right $ Tritonic    $ FL.fromFoldable' intervals
    4  -> Right $ Tetratonic  $ FL.fromFoldable' intervals
    5  -> Right $ Pentatonic  $ FL.fromFoldable' intervals
    6  -> Right $ Hexatonic   $ FL.fromFoldable' intervals
    7  -> Right $ Heptatonic  $ FL.fromFoldable' intervals
    8  -> Right $ Octatonic   $ FL.fromFoldable' intervals
    9  -> Right $ Nonatonic   $ FL.fromFoldable' intervals
    10 -> Right $ Decatonic   $ FL.fromFoldable' intervals
    11 -> Right $ Undecatonic $ FL.fromFoldable' intervals
    12 -> Right $ Chromatic   $ FL.fromFoldable' intervals
    l  -> Left $ T.pack $ "Cannot build a scale of length " <> show l

scaleToIntervals :: Scale -> [I.Interval]
scaleToIntervals (Ditonic     intervals) = F.toList intervals
scaleToIntervals (Tritonic    intervals) = F.toList intervals
scaleToIntervals (Tetratonic  intervals) = F.toList intervals
scaleToIntervals (Pentatonic  intervals) = F.toList intervals
scaleToIntervals (Hexatonic   intervals) = F.toList intervals
scaleToIntervals (Heptatonic  intervals) = F.toList intervals
scaleToIntervals (Octatonic   intervals) = F.toList intervals
scaleToIntervals (Nonatonic   intervals) = F.toList intervals
scaleToIntervals (Decatonic   intervals) = F.toList intervals
scaleToIntervals (Undecatonic intervals) = F.toList intervals
scaleToIntervals (Chromatic   intervals) = F.toList intervals

scaleToText :: Scale -> T.Text
scaleToText scale =
  let intervals =
        T.unwords
          [ "<"
          , T.unwords $ I.intervalToText <$> scaleToIntervals scale
          , ">"
          ]

   in case scale of
        Ditonic     _ -> "Ditonic "     <> intervals
        Tritonic    _ -> "Tritonic "    <> intervals
        Tetratonic  _ -> "Tetratonic "  <> intervals
        Pentatonic  _ -> "Pentatonic "  <> intervals
        Hexatonic   _ -> "Hexatonic "   <> intervals
        Heptatonic  _ -> "Heptatonic "  <> intervals
        Octatonic   _ -> "Octatonic "   <> intervals
        Nonatonic   _ -> "Nonatonic "   <> intervals
        Decatonic   _ -> "Decatonic "   <> intervals
        Undecatonic _ -> "Undecatonic " <> intervals
        Chromatic   _ -> "Chromatic "   <> intervals

--
-- Common Scales as Constants
--
major :: Scale
major =
  Heptatonic
    $ FL.fromFoldable'
    $ [ I.maj_2, I.maj_3, I.per_4, I.per_5, I.maj_6, I.maj_7 ]

minor :: Scale
minor =
  Heptatonic
    $ FL.fromFoldable'
    $ [ I.maj_2, I.min_3, I.per_4, I.per_5, I.min_6, I.min_7 ]

chromatic :: Scale
chromatic =
  Chromatic
    $ FL.fromFoldable'
        [ I.min_2
        , I.maj_2
        , I.min_3
        , I.maj_3
        , I.per_4
        , I.aug_4
        , I.per_5
        , I.min_6
        , I.maj_6
        , I.min_7
        , I.maj_7
        ]