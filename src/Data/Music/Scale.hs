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
  , scaleFromIntervals, unsafeMakeScale
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

-- | Attempts to build a Scale from a provided list of Intervals. Because
-- Scales are represented with the tonic as implicit, the list of Intervals
-- provided should be of a size 1 less than the desired Scale. So, to get the
-- Major scale, one would provide a list of six Intervals containing the
-- Intervals that build the 2nd through the 7th scale degrees.
--
-- Will fail if the provided list has 11 or more unique non-tonic Intervals (the
-- Chromatic scale would have 11 Intervals, with the 12th being the implicit
-- tonic), or if the provided list contains only tonic Intervals - that is, the
-- unison or the octave.
--
scaleFromIntervals :: [I.Interval] -> Either T.Text Scale
scaleFromIntervals intervals =
  case dedupeAndSort intervals of
    []    -> Left  $ T.pack "Cannot build a scale from 0 non-tonic intervals."
    scale -> Right $ unsafeMakeScale scale
--
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

-- | Allows a user to build a `Scale` from a list of `Interval`s without
-- verifying that it will build successfully based on size or content. This is
-- intended for users that wish to build scales in an unsafe but educated way
-- where they know it will succeed and are willing to deal with the possibility
-- of failure on their own terms.
unsafeMakeScale :: [I.Interval] -> Scale
unsafeMakeScale intervals =
  case length intervals of
    1  -> Ditonic     $ FL.fromFoldable' intervals
    2  -> Tritonic    $ FL.fromFoldable' intervals
    3  -> Tetratonic  $ FL.fromFoldable' intervals
    4  -> Pentatonic  $ FL.fromFoldable' intervals
    5  -> Hexatonic   $ FL.fromFoldable' intervals
    6  -> Heptatonic  $ FL.fromFoldable' intervals
    7  -> Octatonic   $ FL.fromFoldable' intervals
    8  -> Nonatonic   $ FL.fromFoldable' intervals
    9  -> Decatonic   $ FL.fromFoldable' intervals
    10 -> Undecatonic $ FL.fromFoldable' intervals
    _  -> Chromatic   $ FL.fromFoldable' intervals

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
  unsafeMakeScale . dedupeAndSort . (:) interval . scaleToIntervals

-- | Merges two Scales together, effectively taking the union of the two.
mergeScales :: Scale -> Scale -> Scale
mergeScales scale1 scale2 = unsafeMakeScale $ scaleUnion scale1 scale2

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
