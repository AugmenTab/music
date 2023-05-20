{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Data.Music.Interval
  ( Interval
  , mkInterval
  , intervalFromHalfSteps
  , intervalToHalfSteps
  , intervalToText

  , Quality(..)
  , qualityToText

  , Size
  , mkIntervalSize

  -- Unison
  , unison
  , per_1
  , aug_1

  -- Seconds
  , dim_2
  , min_2
  , maj_2
  , aug_2

  -- Thirds
  , dim_3
  , min_3
  , maj_3
  , aug_3

  -- Fourths
  , dim_4
  , per_4
  , aug_4

  -- Fifths
  , dim_5
  , per_5
  , aug_5

  -- Sixths
  , dim_6
  , min_6
  , maj_6
  , aug_6

  -- Sevenths
  , dim_7
  , min_7
  , maj_7
  , aug_7

  -- Octave
  , octave
  , dim_8
  , per_8
  ) where

import           Flipstone.Prelude
import           Data.Music.Invertible (Invertible(..))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Numeric.Natural (Natural)
import           Text.Show (Show(..))

-- | An Interval is a representation of the space between two notes. It has two
-- elements: a Quality, which determines whether the interval is Major, Minor,
-- Augmented, Diminished, or Perfect; and a Size, which is the number of note
-- letters between the two notes (A to B is a second, D to G is a fourth, etc).
data Interval = Interval Quality Size

instance Eq Interval where
  interval1 == interval2 =
    intervalToHalfSteps interval1 == intervalToHalfSteps interval2

instance Invertible Interval where
  invert (Interval quality size) = Interval (invert quality) $ 9 - size

instance Ord Interval where
  compare interval1@(Interval q1 s1) interval2@(Interval q2 s2) =
    mconcat
      [ compare (intervalToHalfSteps interval1) (intervalToHalfSteps interval2)
      , compare s1 s2
      , compare q1 q2
      ]

instance Show Interval where
  show (Interval quality size) = show quality <> show size

-- | Constructs an Interval from a given quality and interval. This can fail if
-- an impossible interval combination is provided.
--
-- > mkInterval Major 2 == Right Interval M2
-- > mkInterval Perfect 3 == Left "Invalid interval P3."
--
mkInterval :: Integral a => Quality -> a -> Either T.Text Interval
mkInterval quality num = do
  size <- mkIntervalSize num

  let isPerfect = Set.member size perfectIntervals
      toInterval = Right $ Interval quality size
      invalid =
        Left
          $ T.pack
          $ "Invalid interval " <> show quality <> show size <> "."

  case quality of
    Diminished ->
      toInterval

    Minor
      | isPerfect -> invalid
      | otherwise -> toInterval

    Perfect
      | isPerfect -> toInterval
      | otherwise -> invalid

    Major
      | isPerfect -> invalid
      | otherwise -> toInterval

    Augmented ->
      toInterval

intervalToText :: Interval -> T.Text
intervalToText = T.pack . show

perfectIntervals :: Set.Set Size
perfectIntervals =
  Set.fromList [ Size 1, Size 4, Size 5, Size 8 ]

-- | A number of half steps between two notes. Any operations that would result
-- in a negative number of half steps will fail.
newtype HalfSteps = HalfSteps Natural
  deriving newtype (Eq, Num, Ord, Show)

instance Bounded HalfSteps where
  minBound =  0
  maxBound = 12

validIntervalMap :: Map.Map (Size, HalfSteps) Interval
validIntervalMap =
  Map.fromList
    [ ( (Size 1, HalfSteps  0), per_1 )
    , ( (Size 1, HalfSteps  1), aug_1 )
    , ( (Size 2, HalfSteps  0), dim_2 )
    , ( (Size 2, HalfSteps  1), min_2 )
    , ( (Size 2, HalfSteps  2), maj_2 )
    , ( (Size 2, HalfSteps  3), aug_2 )
    , ( (Size 3, HalfSteps  2), dim_3 )
    , ( (Size 3, HalfSteps  3), min_3 )
    , ( (Size 3, HalfSteps  4), maj_3 )
    , ( (Size 3, HalfSteps  5), aug_3 )
    , ( (Size 4, HalfSteps  4), dim_4 )
    , ( (Size 4, HalfSteps  5), per_4 )
    , ( (Size 4, HalfSteps  6), aug_4 )
    , ( (Size 5, HalfSteps  6), dim_5 )
    , ( (Size 5, HalfSteps  7), per_5 )
    , ( (Size 5, HalfSteps  8), aug_5 )
    , ( (Size 6, HalfSteps  7), dim_6 )
    , ( (Size 6, HalfSteps  8), min_6 )
    , ( (Size 6, HalfSteps  9), maj_6 )
    , ( (Size 6, HalfSteps 10), aug_6 )
    , ( (Size 7, HalfSteps  9), dim_7 )
    , ( (Size 7, HalfSteps 10), min_7 )
    , ( (Size 7, HalfSteps 11), maj_7 )
    , ( (Size 7, HalfSteps 12), aug_7 )
    , ( (Size 8, HalfSteps 11), dim_8 )
    , ( (Size 8, HalfSteps 12), per_8 )
    ]

-- | This function attempts to build an interval using the following inputs:
-- - a Size, indicating letter name distance between the tonic and the chosen
-- interval to build
-- - the desired HalfSteps up from the tonic
--
-- With these two values, we attempt to create an interval that describes the
-- desired gap. Because this has the possibility of failure, we return Either.
intervalFromHalfSteps :: Size -> HalfSteps -> Either T.Text Interval
intervalFromHalfSteps size steps =
  let errorMessage =
        T.unwords
          [ "Cannot create Interval of Size"
          , T.pack $ show size
          , "with"
          , T.pack $ show steps
          , "half-steps."
          ]

   in maybe (Left errorMessage) Right
        $ Map.lookup (size, steps) validIntervalMap

-- `intervalToHalfSteps` represents all possible valid intervals, but because
-- the Interval type *can* represent impossible intervals (even though this
-- isn't possible in practice because the smart constructor is the only exposed
-- way to build one), this pattern match is non-exhaustive. Perhaps modify
-- Interval to be less flexible?

-- | The number of half steps up from the tonic the interval represents.
intervalToHalfSteps :: Interval -> HalfSteps
intervalToHalfSteps interval =
  case interval of
    Interval Perfect    1 -> HalfSteps  0
    Interval Augmented  1 -> HalfSteps  1
    Interval Diminished 2 -> HalfSteps  0
    Interval Minor      2 -> HalfSteps  1
    Interval Major      2 -> HalfSteps  2
    Interval Augmented  2 -> HalfSteps  3
    Interval Diminished 3 -> HalfSteps  2
    Interval Minor      3 -> HalfSteps  3
    Interval Major      3 -> HalfSteps  4
    Interval Augmented  3 -> HalfSteps  5
    Interval Diminished 4 -> HalfSteps  4
    Interval Perfect    4 -> HalfSteps  5
    Interval Augmented  4 -> HalfSteps  6
    Interval Diminished 5 -> HalfSteps  6
    Interval Perfect    5 -> HalfSteps  7
    Interval Augmented  5 -> HalfSteps  8
    Interval Diminished 6 -> HalfSteps  7
    Interval Minor      6 -> HalfSteps  8
    Interval Major      6 -> HalfSteps  9
    Interval Augmented  6 -> HalfSteps 10
    Interval Diminished 7 -> HalfSteps  9
    Interval Minor      7 -> HalfSteps 10
    Interval Major      7 -> HalfSteps 11
    Interval Augmented  7 -> HalfSteps 12
    Interval Diminished 8 -> HalfSteps 11
    Interval Perfect    8 -> HalfSteps 12

-- | An interval Quality.
data Quality
  = Diminished
  | Minor
  | Perfect
  | Major
  | Augmented
  deriving stock (Bounded, Enum, Eq, Ord)

instance Invertible Quality where
  invert = toEnum . (fromEnum (maxBound :: Quality) -) . fromEnum

instance Show Quality where
  show Diminished = "d"
  show Minor      = "m"
  show Perfect    = "P"
  show Major      = "M"
  show Augmented  = "A"

qualityToText :: Quality -> T.Text
qualityToText quality =
  case quality of
    Diminished -> "Diminished"
    Minor      -> "Minor"
    Perfect    -> "Perfect"
    Major      -> "Major"
    Augmented  -> "Augmented"

newtype Size = Size Natural
  deriving newtype (Eq, Num, Ord, Show)

instance Bounded Size where
  minBound = 1
  maxBound = 8

-- | Attempts to create an interval Size from a given Integral value. Intervals
-- greater than one octave are reduced to their basic sizes (9 becomes 2, 13
-- becomes 4, etc). Fails if given a number less than 1.
mkIntervalSize :: Integral a => a -> Either T.Text Size
mkIntervalSize num
  | num <= 0  = Left "Intervals must be at least Size 1."
  | num >= 9  = Right . fromIntegral $ mod num 7
  | otherwise = Right $ fromIntegral num

-- Unison
unison :: Interval
unison = per_1

per_1 :: Interval
per_1 = Interval Perfect 1

aug_1 :: Interval
aug_1 = Interval Augmented 1

-- Seconds
dim_2 :: Interval
dim_2 = Interval Diminished 2

min_2 :: Interval
min_2 = Interval Minor 2

maj_2 :: Interval
maj_2 = Interval Major 2

aug_2 :: Interval
aug_2 = Interval Augmented 2

-- Thirds
dim_3 :: Interval
dim_3 = Interval Diminished 3

min_3 :: Interval
min_3 = Interval Minor 3

maj_3 :: Interval
maj_3 = Interval Major 3

aug_3 :: Interval
aug_3 = Interval Augmented 3

-- Fourths
dim_4 :: Interval
dim_4 = Interval Diminished 4

per_4 :: Interval
per_4 = Interval Perfect 4

aug_4 :: Interval
aug_4 = Interval Augmented 4

-- Fifths
dim_5 :: Interval
dim_5 = Interval Diminished 5

per_5 :: Interval
per_5 = Interval Perfect 5

aug_5 :: Interval
aug_5 = Interval Augmented 5

-- Sixths
dim_6 :: Interval
dim_6 = Interval Diminished 6

min_6 :: Interval
min_6 = Interval Minor 6

maj_6 :: Interval
maj_6 = Interval Major 6

aug_6 :: Interval
aug_6 = Interval Augmented 6

-- Sevenths
dim_7 :: Interval
dim_7 = Interval Diminished 7

min_7 :: Interval
min_7 = Interval Minor 7

maj_7 :: Interval
maj_7 = Interval Major 7

aug_7 :: Interval
aug_7 = Interval Augmented 7

-- Octave
octave :: Interval
octave = per_8

dim_8 :: Interval
dim_8 = Interval Diminished 8

per_8 :: Interval
per_8 = Interval Perfect 8
