module Data.Music.Time
  ( Duration
  , dotted
  , whole, half, quarter, eighth, sixteenth

  , Tempo
  , BPM
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

newtype Duration = Duration (Ratio Natural)
  deriving newtype (Eq, Num, Ord)

instance Show Duration where
  show (Duration d) =
    "Duration " <> show (numerator d) <> "/" <> show (denominator d)

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

data Tempo = Tempo Duration BPM

instance Eq Tempo where
  (Tempo (Duration d1) n1) == (Tempo (Duration d2) n2) =
    d1 * (n1 % 1) == d2 * (n2 % 1)

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
(.=) :: Duration -> BPM -> Tempo
(.=) = Tempo

tempoBeat :: Tempo -> Duration
tempoBeat (Tempo duration _) = duration

tempoBPM :: Tempo -> BPM
tempoBPM (Tempo _ bpm) = bpm

type BPM = Natural

changeTempoBeat :: Duration -> Tempo -> Tempo
changeTempoBeat duration (Tempo _ bpm) = Tempo duration bpm

changeTempoBPM :: BPM -> Tempo -> Tempo
changeTempoBPM bpm (Tempo duration _) = Tempo duration bpm

speedUpBy :: Tempo -> BPM -> Tempo
speedUpBy (Tempo duration bpm) = Tempo duration . (+) bpm

slowDownBy :: Tempo -> BPM -> Either T.Text Tempo
slowDownBy (Tempo duration bpm) decrease
  | decrease >= bpm = Left "Cannot decrease tempo to below 1 bpm."
  | otherwise       = Right . Tempo duration $ bpm - decrease

scaleTempo :: Ratio Natural -> Tempo -> Tempo
scaleTempo f (Tempo duration bpm) =
  let toNat :: Rational -> Natural
      toNat = round
   in Tempo duration . toNat $ realToFrac bpm * realToFrac f

doubleTime :: Tempo -> Tempo
doubleTime = scaleTempo 2

halfTime :: Tempo -> Tempo
halfTime = scaleTempo 0.5

grave :: Tempo
grave = quarter .= 35

largo :: Tempo
largo = quarter .= 50

funeralMarch :: Tempo
funeralMarch = quarter .= 60

adagio :: Tempo
adagio = quarter .= 70

andante :: Tempo
andante = quarter .= 95

moderato :: Tempo
moderato = quarter .= 112

march :: Tempo
march = quarter .= 120

allegretto :: Tempo
allegretto = quarter .= 130

allegro :: Tempo
allegro = quarter .= 144

vivace :: Tempo
vivace = quarter .= 163

presto :: Tempo
presto = quarter .= 180

prestissimo :: Tempo
prestissimo = quarter .= 208

data Meter = Meter BeatCount BeatNote
  deriving stock (Eq)

instance Ord Meter where
  Meter (BeatCount bc1) (BeatNote bn1) `compare` Meter (BeatCount bc2) (BeatNote bn2) =
    compare (bc1 % bn1) (bc2 % bn2)

instance Show Meter where
  show (Meter bc bn) = "Meter " <> show bc <> "/" <> show bn

type TimeSignature = Meter

infixl 8 ./
(./) :: Natural -> Natural -> Meter
bc ./ bn = Meter (BeatCount bc) (BeatNote bn)

meterBeatCount :: Meter -> BeatCount
meterBeatCount (Meter bc _) = bc

meterBeatDuration :: Meter -> Duration
meterBeatDuration (Meter _ (BeatNote bn)) = Duration $ 1 % bn

cut :: TimeSignature
cut = 2 ./ 2

common :: TimeSignature
common = 4 ./ 4

newtype BeatCount = BeatCount Natural
  deriving newtype (Eq, Ord, Show)

newtype BeatNote = BeatNote Natural
  deriving newtype (Eq, Ord, Show)
