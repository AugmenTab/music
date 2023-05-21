{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Data.Music.Scale
  ( Scale
     ( Ditonic
     , Tritonic
     , Tetratonic
     , Pentatonic
     , Hexatonic
     , Heptatonic
     , Octatonic
     , Nonatonic
     , Decatonic
     , Undecatonic
     , Chromatic
     )
  , Mode
  , scaleFromIntervals
  , scaleToIntervals
  , scaleToText
  , addInterval
  , mergeScales, scaleDifference, scaleIntersection, scaleUnion
  ) where

import           Flipstone.Prelude
import qualified Data.Music.Interval as I
import           Data.Music.Invertible (Invertible(..))

import qualified Data.Foldable as F
import qualified Data.FixedList as FL
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import           Text.Show (Show(..))

-- | A Scale is a list of fixed length, holding the Interval values to determine
-- the 2nd through last note in the scale from the tonic. They all have a fixed
-- length of 1 less than the scale length - we don't need to determine distance
-- from the tonic for the tonic or the octave. They are always sorted into
-- ascending order, and are effectively a 'Set'.
--
-- Scales have a notion of enharmonic equivalence; two scales of equal size
-- where each interval in the same position of each scale both have an equal
-- number of half steps from the tonic will be treated as equivalent.
--
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

instance Eq Scale where
  scale1 == scale2 =
    let toHalfSteps = fmap I.intervalToHalfSteps . scaleToIntervals
     in toHalfSteps scale1 == toHalfSteps scale2

instance Invertible Scale where
  invert scale =
    let invertIntervals is = invert <$> FL.reverse is
     in case scale of
          Ditonic     intervals -> Ditonic     $ invertIntervals intervals
          Tritonic    intervals -> Tritonic    $ invertIntervals intervals
          Tetratonic  intervals -> Tetratonic  $ invertIntervals intervals
          Pentatonic  intervals -> Pentatonic  $ invertIntervals intervals
          Hexatonic   intervals -> Hexatonic   $ invertIntervals intervals
          Heptatonic  intervals -> Heptatonic  $ invertIntervals intervals
          Octatonic   intervals -> Octatonic   $ invertIntervals intervals
          Nonatonic   intervals -> Nonatonic   $ invertIntervals intervals
          Decatonic   intervals -> Decatonic   $ invertIntervals intervals
          Undecatonic intervals -> Undecatonic $ invertIntervals intervals
          Chromatic   intervals -> Chromatic   $ invertIntervals intervals

instance Show Scale where
  show = T.unpack . scaleToText

-- | Type synonym for using a mode.
type Mode = Scale

-- | Attempts to build a Scale from a provided nonempty list of Intervals.
-- Because Scales are represented with the tonic as implicit, the list of
-- Intervals provided should be of a size 1 less than the desired Scale. So, to
-- get the Major scale, one would provide a list of six Intervals containing the
-- Intervals that build the 2nd through the 7th scale degrees.
scaleFromIntervals :: NonEmpty I.Interval -> Scale
scaleFromIntervals intervals =
  -- This is an very loose pattern match, but we know there should be no way
  -- (thanks to the  NonEmpty requirement for the Interval list and the deduping
  -- of Intervals prior to building the Scale) for this to have more than 11
  -- Intervals in it once it reaches the case expression. This makes it
  -- technically unsafe if it were to ever try get more than 11 Intervals, but
  -- that should never happen in practice so it should never fail.
  let scale = dedupeAndSort $ toList intervals
   in case length scale of
        1  -> Ditonic     $ FL.fromFoldable' scale
        2  -> Tritonic    $ FL.fromFoldable' scale
        3  -> Tetratonic  $ FL.fromFoldable' scale
        4  -> Pentatonic  $ FL.fromFoldable' scale
        5  -> Hexatonic   $ FL.fromFoldable' scale
        6  -> Heptatonic  $ FL.fromFoldable' scale
        7  -> Octatonic   $ FL.fromFoldable' scale
        8  -> Nonatonic   $ FL.fromFoldable' scale
        9  -> Decatonic   $ FL.fromFoldable' scale
        10 -> Undecatonic $ FL.fromFoldable' scale
        _  -> Chromatic   $ FL.fromFoldable' scale

-- | Unpacks a Scale into a list of its Intervals.
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

-- | Adds an Interval to a provided Scale. This will de-dupe the Scale to remove
-- equivalent Intervals.
addInterval :: I.Interval -> Scale -> Scale
addInterval interval =
  scaleFromIntervals
    . (I.unison :|)
    . dedupeAndSort
    . (:) interval
    . scaleToIntervals

-- | Merges two Scales together, effectively taking the union of the two.
mergeScales :: Scale -> Scale -> Scale
mergeScales scale1 scale2 =
  -- It's safe to use :| to build a NonEmpty here because dedupeAndSort will
  -- strip out any tonic-equivalent notes.
  scaleFromIntervals $ I.unison :| scaleUnion scale1 scale2

-- | Finds the Intervals in the first provided Scale that is not in common with
-- the second provided Scale.
scaleDifference :: Scale -> Scale -> [I.Interval]
scaleDifference scale1 scale2 =
  Set.toAscList $ Set.difference (scaleSet scale1) (scaleSet scale2)

-- | Finds the Intervals in common between two provided Scales.
scaleIntersection :: Scale -> Scale -> [I.Interval]
scaleIntersection scale1 scale2 =
  Set.toAscList $ Set.intersection (scaleSet scale1) (scaleSet scale2)

-- | Finds the unique Intervals across two provided Scales.
scaleUnion :: Scale -> Scale -> [I.Interval]
scaleUnion scale1 scale2 =
  Set.toAscList $ Set.union (scaleSet scale1) (scaleSet scale2)

--
-- Helpers
--
dedupeAndSort :: [I.Interval] -> [I.Interval]
dedupeAndSort = Set.toAscList . Set.fromList . L.filter (not . I.isTonic)

scaleSet :: Scale -> Set.Set I.Interval
scaleSet = Set.fromList . scaleToIntervals
