module Data.Music.Time
  ( Duration
  , mkDuration, fromDenominator
  , scaleDuration
  , dotted, triplet, toTriplet
  , whole, half, quarter, eighth, sixteenth

  , Tempo
  , (.=)
  , tempoBeat
  , tempoBPM

  , changeTempoBeat
  , changeTempoBPM
  , speedUpBy
  , slowDownBy
  , scaleTempo
  , doubleTime
  , halfTime

  , grave
  , largo
  , funeralMarch
  , adagio
  , andante
  , moderato
  , march
  , allegretto
  , allegro
  , vivace
  , presto
  , prestissimo

  , BPM

  , Meter
  , TimeSignature
  , (./)
  , meterBeatCount
  , meterBeatDuration
  , cut, common

  , BeatCount
  , BeatNote
  ) where

import           Flipstone.Prelude

import qualified Data.Text as T
import           Data.Ratio (denominator, numerator, (%))
import           Numeric.Natural (Natural)
import           Text.Show (Show(..))

-- | Represents a Note or Rest duration. This is represented as a ratio of the
-- note or rest's duration to the whole note or rest. So, a quarter note is
-- represented as @1 % 4@, while a whole note is represented as @1 % 1@.
--
-- Basic arithmetic can be done on Durations - addition, subtraction,
-- multiplication, and division. Since Duration is based on a Ration of Natural,
-- any operation that would result in a negative Duration will in the same
-- manner that any operation that would result in a negative Natural would.
newtype Duration = Duration (Ratio Natural)
  deriving newtype (Eq, Fractional, Num, Ord, Real)

instance Show Duration where
  show (Duration d) =
    "Duration " <> show (numerator d) <> "/" <> show (denominator d)

-- | Creates a Duration based on the provided positive ratio of the note or
-- rest's duration to the whole note or rest.
mkDuration :: Ratio Natural -> Duration
mkDuration = Duration

-- | Create a Duration using the provided positive whole number as the
-- "denominator" of the duration.
--
-- > fromDenominator 4 == Duration (1 % 4) == quarter
fromDenominator :: Natural -> Duration
fromDenominator = Duration . (%) 1

-- | Scales a Duration by the provided ratio.
--
-- > scaleDuration 2 quarter == (1 / 2) * whole == half
-- > scaleDuration (1 % 4) quarter == 0.5 * eighth == sixteenth
--
scaleDuration :: Ratio Natural -> Duration -> Duration
scaleDuration f (Duration d) = Duration $ d * f

-- | "Dots" a Duration by increasing its value by 50%.
--
-- > dotted quarter == scaleDuration (3 % 2) quarter
--
dotted :: Duration -> Duration
dotted = scaleDuration $ 3 % 2

-- | Adjusts a Duration to one triplet Duration where the complete triplet has
-- the same Duration as the provided Duration - that is, 1/3 its value.
--
-- > 3 * triplet quarter == quarter
--
triplet :: Duration -> Duration
triplet = scaleDuration $ 1 % 3

-- | Adjusts a Duration to its triplet value - that is, the `Duration` where it
-- represents 1/3 of the Duration worth double its original size.
--
-- > 3 * toTriplet quarter == half
--
toTriplet :: Duration -> Duration
toTriplet = scaleDuration $ 2 % 3

-- | One whole note (𝅝) or whole rest (𝄻) Duration.
whole :: Duration
whole = Duration $ 1 % 1

-- | One half note (𝅗𝅥) or half rest (𝄼) Duration.
half :: Duration
half = Duration $ 1 % 2

-- | One quarter note (♩) or quarter rest (𝄽) Duration.
quarter :: Duration
quarter = Duration $ 1 % 4

-- | One eighth note (𝅘𝅥𝅮) or eighth rest (𝄾) Duration.
eighth :: Duration
eighth = Duration $ 1 % 8

-- | One sixteenth note (𝅘𝅥𝅯) or sixteenth rest (𝄿) Duration.
sixteenth :: Duration
sixteenth = Duration $ 1 % 16

-- | Represents a Tempo as a note duration that gets the beat, and the number of
-- beats per minute. Use '.=' to construct Tempos.
--
-- 𝅘𝅥 = 85
-- > quarter .= 85
--
-- 𝅘𝅥𝅭 = 120
-- > Duration (3 % 8) .= 120
-- > dotted quarter .= 120
--
data Tempo = Tempo Duration BPM

instance Eq Tempo where
  (Tempo (Duration d1) (BPM bpm1)) == (Tempo (Duration d2) (BPM bpm2)) =
    d1 * (bpm1 % 1) == d2 * (bpm2 % 1)

instance Show Tempo where
  show (Tempo (Duration d) n) =
    mconcat
      [ "Tempo "
      , show $ numerator d
      , "/"
      , show $ denominator d
      , " = "
      , show n
      ]

infixl 8 .=

-- | Constructs a Tempo from a provided Duration and BPM.
(.=) :: Duration -> BPM -> Tempo
(.=) = Tempo

-- | The note duration getting the beat in a Tempo.
tempoBeat :: Tempo -> Duration
tempoBeat (Tempo duration _) = duration

-- | The number of beats per minute in a Tempo.
tempoBPM :: Tempo -> BPM
tempoBPM (Tempo _ bpm) = bpm

-- | Modifies a given Tempo by changing the beat note to the provided Duration.
changeTempoBeat :: Duration -> Tempo -> Tempo
changeTempoBeat duration (Tempo _ bpm) = Tempo duration bpm

-- | Modifies a given Tempo by changing the beats per minute to the provided
-- BPM.
changeTempoBPM :: BPM -> Tempo -> Tempo
changeTempoBPM bpm (Tempo duration _) = Tempo duration bpm

-- | Increases the speed of a given Tempo.
speedUpBy :: Tempo -> BPM -> Tempo
speedUpBy (Tempo duration bpm) = Tempo duration . (+) bpm

-- | Decreases the speed of a given Tempo. This can fail if the provided BPM
-- would reduce the Tempo below 1 bpm.
slowDownBy :: Tempo -> BPM -> Either T.Text Tempo
slowDownBy (Tempo duration bpm) decrease
  | decrease >= bpm = Left "Cannot decrease tempo to below 1 bpm."
  | otherwise       = Right . Tempo duration $ bpm - decrease

-- | Scales a given Tempo by a provided non-negative Ratio. This can fail if
-- the provided Ratio is equal to 0.
scaleTempo :: Ratio Natural -> Tempo -> Either T.Text Tempo
scaleTempo f (Tempo duration (BPM bpm)) =
  if f == 0
     then Left "Cannot scale tempo to 0 bpm."
     else
       let toNat :: Rational -> Natural
           toNat = round
        in Right . Tempo duration . BPM . toNat $ realToFrac bpm * realToFrac f

-- | Doubles a given Tempo.
doubleTime :: Tempo -> Tempo
doubleTime (Tempo duration bpm) = Tempo duration $ bpm * 2

-- | Halves a given Tempo, rounding down for odd BPMs.
halfTime :: Tempo -> Tempo
halfTime (Tempo duration bpm) = Tempo duration $ div bpm 2

-- | ♩ = 35
grave :: Tempo
grave = quarter .= 35

-- | ♩ = 50
largo :: Tempo
largo = quarter .= 50

-- | ♩ = 60
funeralMarch :: Tempo
funeralMarch = quarter .= 60

-- | ♩ = 70
adagio :: Tempo
adagio = quarter .= 70

-- | ♩ = 95
andante :: Tempo
andante = quarter .= 95

-- | ♩ = 112
moderato :: Tempo
moderato = quarter .= 112

-- | ♩ = 120
march :: Tempo
march = quarter .= 120

-- | ♩ = 130
allegretto :: Tempo
allegretto = quarter .= 130

-- | ♩ = 144
allegro :: Tempo
allegro = quarter .= 144

-- | ♩ = 163
vivace :: Tempo
vivace = quarter .= 163

-- | ♩ = 180
presto :: Tempo
presto = quarter .= 180

-- | ♩ = 208
prestissimo :: Tempo
prestissimo = quarter .= 208

-- | Represents the number of beats per minute for a Tempo. Use '.=' to
-- construct Tempos.
newtype BPM = BPM Natural
 deriving newtype (Eq, Enum, Integral, Num, Ord, Real, Show)

-- | Represents a Meter or Time Signature, with the number of beats and the note
-- that gets the beat. Use './' to construct Meters.
--
-- >>> 3 ./ 4
-- Meter 3/4
--
data Meter = Meter BeatCount BeatNote
  deriving stock (Eq)

instance Ord Meter where
  Meter (BeatCount bc1) (BeatNote bn1) `compare` Meter (BeatCount bc2) (BeatNote bn2) =
    compare (bc1 % bn1) (bc2 % bn2)

instance Show Meter where
  show (Meter bc bn) = "Meter " <> show bc <> "/" <> show bn

-- | An alias for 'Meter' making use of another common term for Meter.
type TimeSignature = Meter

infixl 8 ./

-- | Constructs a Meter from two provided Naturals representing the number of
-- beats and the beat note, respectively. Intended to look like a time
-- signature when written.
--
-- >>> 3 ./ 4
-- Meter 3/4
--
(./) :: Natural -> Natural -> Meter
bc ./ bn = Meter (BeatCount bc) (BeatNote bn)

-- | The number of beats in the Meter - the "numerator" of the provided time
-- signature.
meterBeatCount :: Meter -> BeatCount
meterBeatCount (Meter bc _) = bc

-- | The beat note of the Meter - the "denominator" of the provided time
-- signature.
meterBeatDuration :: Meter -> Duration
meterBeatDuration (Meter _ (BeatNote bn)) = Duration $ 1 % bn

-- | 2/2 (𝄵) time.
cut :: TimeSignature
cut = 2 ./ 2

-- | 4/4 (𝄴) time.
common :: TimeSignature
common = 4 ./ 4

-- | Represents a number of beats for a given Meter, Measure, or Phrase. Use
-- './' to construct Meters.
newtype BeatCount = BeatCount Natural
  deriving newtype (Eq, Ord, Show)

-- | Represents the note that gets the beat for a given Meter, Measure, or
-- Phrase. Use './' to construct Meters.
newtype BeatNote = BeatNote Natural
  deriving newtype (Eq, Ord, Show)
