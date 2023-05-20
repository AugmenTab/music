module Data.Music.Pitch
  ( Pitch(..)
  , pitchToText
  , Tonic
  , Supertonic
  , Mediant
  , Subdominant
  , Dominant
  , Submediant
  , Subtonic
  , LeadingTone

  , PitchClass(..)
  , pitchClassToText

  , Accidental(..)
  , accidentalSymbol, accidentalToText
  , flatten, sharpen
  , setAccidental, doubleFlat, flat, natural, sharp, doubleSharp
  ) where

import           Flipstone.Prelude

import qualified Data.List as L
import qualified Data.Text as T
import           Text.Show (Show(..))

-- | A Pitch represents a musical note's letter value and accidental, without
-- regard for what octave the note is in. It supports enharmonic equivalence.
--
-- > Pitch B Natural == Pitch C Flat
-- > Pitch D NoAccidental == Pitch D Natural
-- > Pitch G Natural == Pitch A DoubleFlat
-- > Pitch F Sharp == Pitch E DoubleSharp
--
data Pitch = Pitch PitchClass Accidental

instance Eq Pitch where
  pitch1@(Pitch pc1 a1) == pitch2@(Pitch pc2 a2) =
    L.or [ pc1 == pc2 && a1 == a2
         , pitchToInt pitch1 == pitchToInt pitch2
         ]

instance Ord Pitch where
  compare pitch1@(Pitch pc1 _) pitch2@(Pitch pc2 _) =
    pitchToInt pitch1 `compare` pitchToInt pitch2 <> compare pc1 pc2

instance Show Pitch where
  show (Pitch pitchClass accidental) =
    "Pitch " <> show pitchClass <> show accidental

pitchToInt :: Pitch -> Int
pitchToInt (Pitch pc a) =
  case pitchClassToInt pc + accidentalToInt a of
    n | n < 0     -> 12 + n
      | otherwise -> n

pitchToText :: Pitch -> T.Text
pitchToText = T.pack . show

-- These type synonyms are conveniences for using scale degree names.

-- | Type synonym for referencing the first scale degree in a 7-note scale.
type Tonic = Pitch

-- | Type synonym for referencing the second scale degree in a 7-note scale.
type Supertonic = Pitch

-- | Type synonym for referencing the third scale degree in a 7-note scale.
type Mediant = Pitch

-- | Type synonym for referencing the fourth scale degree in a 7-note scale.
type Subdominant = Pitch

-- | Type synonym for referencing the fifth scale degree in a 7-note scale.
type Dominant = Pitch

-- | Type synonym for referencing the sixth scale degree in a 7-note scale.
type Submediant = Pitch

-- | Type synonym for referencing the seventh scale degree in a 7-note scale
-- with a minor 7th, such as the natural minor scale.
type Subtonic = Pitch

-- | Type synonym for referencing the seventh scale degree in a 7-note scale
-- with a major 7th.
type LeadingTone = Pitch

-- | Represents the letter value of a Pitch.
data PitchClass
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving stock (Eq, Ord, Show)

pitchClassToInt :: PitchClass -> Int
pitchClassToInt pc =
  case pc of
    A ->  0
    B ->  2
    C ->  3
    D ->  5
    E ->  7
    F ->  8
    G -> 10

pitchClassToText :: PitchClass -> T.Text
pitchClassToText = T.pack . show

-- | Represents the note's accidental - Sharp, Flat, Double Sharp, Double Flat,
-- or none. This system makes a distinction between:
--
-- - Natural, which here is used for notes that would normally have an
-- accidental in the given context (interval, scale, chord, etc.) but do not due
-- to modifications or "playing outside."
--
-- - NoAccidental, which is used for notes that would not normally have an
-- accidental in the given context, e.g. "C" in the C Major Scale.
--
data Accidental
  = DoubleFlat
  | Flat
  | NoAccidental
  | Natural
  | Sharp
  | DoubleSharp
  deriving stock (Bounded, Enum, Eq, Ord)

instance Show Accidental where
  -- This shows the natural pitch without an accidental mark, which is the
  -- normal way of writing an undecorated pitch.
  show NoAccidental = ""

  -- This is a true "natural" pitch, with the proper accidental shown. This is
  -- important to differentiate explicitly natural pitch in "playing outside"
  -- contexts.
  show Natural = "♮"

  -- These are common accidentals that are always represented as intended.
  show DoubleFlat  = "𝄫"
  show Flat        = "♭"
  show Sharp       = "♯"
  show DoubleSharp = "𝄪"

-- | Provides the accidental symbol as Text, or Nothing for @NoAccidental@.
accidentalSymbol :: Accidental -> Maybe T.Text
accidentalSymbol NoAccidental = Nothing
accidentalSymbol accidental   = Just . T.pack $ show accidental

accidentalToInt :: Accidental -> Int
accidentalToInt accidental =
  case accidental of
    DoubleFlat   -> negate 2
    Flat         -> negate 1
    NoAccidental -> 0
    Natural      -> 0
    Sharp        -> 1
    DoubleSharp  -> 2

-- | Provides a textual representation of the accidental as an English word -
-- e.g. "Sharp", "Double Flat."
accidentalToText :: Accidental -> T.Text
accidentalToText accidental =
  case accidental of
    DoubleFlat   -> "Double Flat"
    Flat         -> "Flat"
    NoAccidental -> "No Accidental"
    Natural      -> "Natural"
    Sharp        -> "Sharp"
    DoubleSharp  -> "Double Sharp"

-- | This flattens a provided pitch without changing its pitch class. It will
-- return the pitch unchanged if it is already double-flat.
flatten :: Pitch -> Pitch
flatten (Pitch pc acc) =
  case acc of
    DoubleFlat -> Pitch pc DoubleFlat
    Natural    -> Pitch pc Flat
    _          -> Pitch pc $ toEnum $ fromEnum acc - 1

-- | This sharpens a provided pitch without changing its pitch class. It will
-- return the pitch unchanged if it is already double-sharp.
sharpen :: Pitch -> Pitch
sharpen (Pitch pc acc) =
  case acc of
    DoubleSharp  -> Pitch pc DoubleSharp
    NoAccidental -> Pitch pc Sharp
    _            -> Pitch pc $ toEnum $ fromEnum acc + 1

-- | Sets the accidental of a given Pitch.
setAccidental :: Accidental -> Pitch -> Pitch
setAccidental acc (Pitch pc _) = Pitch pc acc

-- | Sets the accidental of a given Pitch to 'DoubleFlat'.
doubleFlat :: Pitch -> Pitch
doubleFlat = setAccidental DoubleFlat

-- | Sets the accidental of a given Pitch to 'Flat'.
flat :: Pitch -> Pitch
flat = setAccidental Flat

-- | Sets the accidental of a given Pitch to 'Natural'.
natural :: Pitch -> Pitch
natural = setAccidental Natural

-- | Sets the accidental of a given Pitch to 'Sharp'.
sharp :: Pitch -> Pitch
sharp = setAccidental Sharp

-- | Sets the accidental of a given Pitch to 'DoubleSharp'.
doubleSharp :: Pitch -> Pitch
doubleSharp = setAccidental DoubleSharp
