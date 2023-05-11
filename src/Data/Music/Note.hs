module Data.Music.Note
  ( Accidental(..)
  , flatten, sharpen
  , doubleFlat, flat, natural, sharp, doubleSharp

  , Note(..)
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

data Note = Note PitchClass Accidental

instance Eq Note where
  (Note pc1 a1) == (Note pc2 a2) =
    L.or [ pc1 == pc2 && a1 == a2
         -- TODO: Add enharmonic equivalence
         ]

instance Show Note where
  show (Note pitchClass accidental) =
    "Note " <> show pitchClass <> show accidental

-- These type synonyms are conveniences for using scale degree names.
type Tonic       = Note
type Supertonic  = Note
type Mediant     = Note
type Subdominant = Note
type Dominant    = Note
type Submediant  = Note

-- TODO: I'm not exactly sure which of these to use if this gets expanded into
-- an ADT...
type Subtonic    = Note
type LeadingTone = Note

data PitchClass
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data Accidental
  = DoubleFlat
  | Flat
  | NoAccidental
  | Natural
  | Sharp
  | DoubleSharp
  deriving stock (Bounded, Enum, Eq, Ord)

instance Show Accidental where
  -- This shows the natural note without an accidental mark, which is the normal
  -- way of writing an undecorated note.
  show NoAccidental = ""

  -- This is a true "natural" note, with the proper accidental shown. This is
  -- important to differentiate explicitly natural notes in "playing outside"
  -- contexts.
  show Natural = "♮"

  -- These are common accidentals that are always represented as intended.
  show DoubleFlat  = "𝄫"
  show Flat        = "♭"
  show Sharp       = "♯"
  show DoubleSharp = "𝄪"

{-| This flattens a provided note without changing its pitch class. It will
   return the note unchanged if it is already double-flat.
-}
flatten :: Note -> Note
flatten (Note pc acc) =
  case acc of
    DoubleFlat -> Note pc DoubleFlat
    Natural    -> Note pc Flat
    _          -> Note pc $ toEnum $ fromEnum acc - 1

{-| This sharpens a provided note without changing its pitch class. It will
   return the note unchanged if it is already double-sharp.
-}
sharpen :: Note -> Note
sharpen (Note pc acc) =
  case acc of
    DoubleSharp  -> Note pc DoubleSharp
    NoAccidental -> Note pc Sharp
    _            -> Note pc $ toEnum $ fromEnum acc + 1

doubleFlat :: Note -> Note
doubleFlat (Note pc _) = Note pc DoubleFlat

flat :: Note -> Note
flat (Note pc _) = Note pc Flat

natural :: Note -> Note
natural (Note pc _) = Note pc Natural

sharp :: Note -> Note
sharp (Note pc _) = Note pc Sharp

doubleSharp :: Note -> Note
doubleSharp (Note pc _) = Note pc DoubleSharp
