module Data.Music.Pitch
  ( Accidental(..)
  , flatten, sharpen
  , doubleFlat, flat, natural, sharp, doubleSharp

  , Pitch(..)

  , Tonic
  , Supertonic
  , Mediant
  , Subdominant
  , Dominant
  , Submediant
  , Subtonic
  , LeadingTone

  , PitchClass(..)
  ) where

import           Flipstone.Prelude

import qualified Data.List as L
import           Text.Show (Show(..))

data Pitch = Pitch PitchClass Accidental

instance Eq Pitch where
  pitch1@(Pitch pc1 a1) == pitch2@(Pitch pc2 a2) =
    L.or [ pc1 == pc2 && a1 == a2
         , pitchToInt pitch1 == pitchToInt pitch2
         ]

instance Show Pitch where
  show (Pitch pitchClass accidental) =
    "Pitch " <> show pitchClass <> show accidental

pitchToInt :: Pitch -> Int
pitchToInt (Pitch pc a) =
  case pitchClassToInt pc + accidentalToInt a of
    n | n < 0     -> 12 + n
      | otherwise -> n

-- These type synonyms are conveniences for using scale degree names.
type Tonic       = Pitch
type Supertonic  = Pitch
type Mediant     = Pitch
type Subdominant = Pitch
type Dominant    = Pitch
type Submediant  = Pitch

-- TODO: I'm not exactly sure which of these to use if this gets expanded into
-- an ADT...
type Subtonic    = Pitch
type LeadingTone = Pitch

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

accidentalToInt :: Accidental -> Int
accidentalToInt accidental =
  case accidental of
    DoubleFlat   -> negate 2
    Flat         -> negate 1
    NoAccidental -> 0
    Natural      -> 0
    Sharp        -> 1
    DoubleSharp  -> 2

{-| This flattens a provided pitch without changing its pitch class. It will
   return the pitch unchanged if it is already double-flat.
-}
flatten :: Pitch -> Pitch
flatten (Pitch pc acc) =
  case acc of
    DoubleFlat -> Pitch pc DoubleFlat
    Natural    -> Pitch pc Flat
    _          -> Pitch pc $ toEnum $ fromEnum acc - 1

{-| This sharpens a provided pitch without changing its pitch class. It will
   return the pitch unchanged if it is already double-sharp.
-}
sharpen :: Pitch -> Pitch
sharpen (Pitch pc acc) =
  case acc of
    DoubleSharp  -> Pitch pc DoubleSharp
    NoAccidental -> Pitch pc Sharp
    _            -> Pitch pc $ toEnum $ fromEnum acc + 1

doubleFlat :: Pitch -> Pitch
doubleFlat (Pitch pc _) = Pitch pc DoubleFlat

flat :: Pitch -> Pitch
flat (Pitch pc _) = Pitch pc Flat

natural :: Pitch -> Pitch
natural (Pitch pc _) = Pitch pc Natural

sharp :: Pitch -> Pitch
sharp (Pitch pc _) = Pitch pc Sharp

doubleSharp :: Pitch -> Pitch
doubleSharp (Pitch pc _) = Pitch pc DoubleSharp
