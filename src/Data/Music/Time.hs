module Data.Music.Time
  ( Duration
  , dotted
  , whole, half, quarter, eighth, sixteenth

  , Tempo(..)
  , (.=)
  , grave
  , largo
  , adagio
  , andante
  , moderato
  , allegretto
  , allegro
  , vivace
  , presto
  , prestissimo

  , Meter(..)
  , TimeSignature
  , (./)
  , cut, common
  ) where

import           Flipstone.Prelude

import           Data.Ratio (denominator, numerator, (%))
import           Numeric.Natural (Natural)
import           Text.Show (Show(..))

newtype Duration = Duration (Ratio Natural)
  deriving newtype (Eq)

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

data Tempo = Tempo Duration Natural

instance Eq Tempo where
  (Tempo (Duration d1) n1) == (Tempo (Duration d2) n2) = d1 / d2 == n1 % n2

instance Show Tempo where
  show (Tempo (Duration d) n) =
    mconcat
      [ "Tempo "
      , show (numerator d)
      , "/"
      , show (denominator d)
      , " = "
      , show n
      ]

infixl 8 .=
(.=) :: Duration -> Natural -> Tempo
(.=) = Tempo

grave :: Tempo
grave = quarter .= 35

largo :: Tempo
largo = quarter .= 50

adagio :: Tempo
adagio = quarter .= 70

andante :: Tempo
andante = quarter .= 95

moderato :: Tempo
moderato = quarter .= 112

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

newtype Meter = Meter (Natural, Natural)
  deriving newtype (Eq)

instance Ord Meter where
  compare (Meter (b1, d1)) (Meter (b2, d2)) = compare (b1 % d1) (b2 % d2)

instance Show Meter where
  show (Meter (b, d)) = "Meter " <> show b <> "/" <> show d

type TimeSignature = Meter

infixl 8 ./
(./) :: Natural -> Natural -> Meter
bs ./ bn = Meter (bs, bn)

cut :: TimeSignature
cut = 2 ./ 2

common :: TimeSignature
common = 4 ./ 4
