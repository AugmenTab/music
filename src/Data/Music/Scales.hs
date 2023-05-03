module Data.Music.Scales
  -- Common Scales
  ( major
  , minor
  , harmonicMinor
  , melodicMinor
  , blues
  , wholeTone
  , octatonicHW
  , octatonicWH
  , chromatic

  -- Pentatonic Scales
  , pentatonicMajor
  , pentatonicMinor
  , pentatonicNeutral

  -- Church Modes
  , ionian
  , dorian
  , phrygian
  , lydian
  , mixolydian
  , aeolian
  , locrian
  ) where

import           Flipstone.Prelude
import qualified Data.Music.Interval as I
import           Data.Music.Scale (Scale(..))

import           Data.FixedList (fromFoldable')

--
-- Common Scales
--
major :: Scale
major =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.maj_3, I.per_4, I.per_5, I.maj_6, I.maj_7 ]

minor :: Scale
minor =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.min_3, I.per_4, I.per_5, I.min_6, I.min_7 ]

harmonicMinor :: Scale
harmonicMinor =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.min_3, I.per_4, I.per_5, I.min_6, I.maj_7 ]

-- This is melodic minor ascending; descending is just natural minor.
melodicMinor :: Scale
melodicMinor =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.min_3, I.per_4, I.per_5, I.maj_6, I.maj_7 ]

blues :: Scale
blues =
  Hexatonic $ fromFoldable' [ I.min_3, I.per_4, I.dim_5, I.per_5, I.min_7 ]

wholeTone :: Scale
wholeTone =
  Hexatonic $ fromFoldable' [ I.maj_2, I.maj_3, I.aug_4, I.aug_5, I.min_7 ]

octatonicHW :: Scale
octatonicHW =
  Octatonic
    $ fromFoldable'
        [ I.min_2
        , I.min_3
        , I.maj_3
        , I.dim_5
        , I.per_5
        , I.maj_6
        , I.min_7
        ]

octatonicWH :: Scale
octatonicWH =
  Octatonic
    $ fromFoldable'
        [ I.maj_2
        , I.min_3
        , I.per_4
        , I.dim_5
        , I.min_6
        , I.maj_6
        , I.maj_7
        ]

chromatic :: Scale
chromatic =
  Chromatic
    $ fromFoldable'
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

--
-- Pentatonic Scales
--
pentatonicMajor :: Scale
pentatonicMajor =
  Pentatonic $ fromFoldable' [ I.maj_2, I.maj_3, I.per_5, I.maj_6 ]

pentatonicMinor :: Scale
pentatonicMinor =
  Pentatonic $ fromFoldable' [ I.min_3, I.per_4, I.per_5, I.min_7 ]

pentatonicNeutral :: Scale
pentatonicNeutral =
  Pentatonic $ fromFoldable' [ I.maj_2, I.per_4, I.per_5, I.min_7 ]

--
-- Church Modes
--
ionian :: Scale
ionian = major

dorian :: Scale
dorian =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.min_3, I.per_4, I.per_5, I.maj_6, I.min_7 ]

phrygian :: Scale
phrygian =
  Heptatonic
    $ fromFoldable' [ I.min_2, I.min_3, I.per_4, I.per_5, I.min_6, I.min_7 ]

lydian :: Scale
lydian =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.maj_3, I.aug_4, I.per_5, I.maj_6, I.maj_7 ]

mixolydian :: Scale
mixolydian =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.maj_3, I.per_4, I.per_5, I.maj_6, I.min_7 ]

aeolian :: Scale
aeolian = minor

locrian :: Scale
locrian =
  Heptatonic
    $ fromFoldable' [ I.min_2, I.min_3, I.per_4, I.dim_5, I.min_6, I.min_7 ]
