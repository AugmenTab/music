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

-- | The major scale.
major :: Scale
major =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.maj_3, I.per_4, I.per_5, I.maj_6, I.maj_7 ]

-- | The natural minor scale.
minor :: Scale
minor =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.min_3, I.per_4, I.per_5, I.min_6, I.min_7 ]

-- | The harmonic minor scale.
harmonicMinor :: Scale
harmonicMinor =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.min_3, I.per_4, I.per_5, I.min_6, I.maj_7 ]

-- | The melodic minor scale (ascending).
melodicMinor :: Scale
melodicMinor =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.min_3, I.per_4, I.per_5, I.maj_6, I.maj_7 ]

-- | The (hexatonic) blues scale, built from the minor pentatonic scale plus the
-- flat 5th degree.
blues :: Scale
blues =
  Hexatonic $ fromFoldable' [ I.min_3, I.per_4, I.dim_5, I.per_5, I.min_7 ]

-- | The whole-tone scale.
wholeTone :: Scale
wholeTone =
  Hexatonic $ fromFoldable' [ I.maj_2, I.maj_3, I.aug_4, I.aug_5, I.min_7 ]

-- | The octatonic scale built using alternating half- and whole-step intervals.
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

-- | The octatonic scale built using alternating whole- and half-step intervals.
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

-- | The chromatic scale.
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

-- | The major pentatonic scale.
pentatonicMajor :: Scale
pentatonicMajor =
  Pentatonic $ fromFoldable' [ I.maj_2, I.maj_3, I.per_5, I.maj_6 ]

-- | The minor pentatonic scale.
pentatonicMinor :: Scale
pentatonicMinor =
  Pentatonic $ fromFoldable' [ I.min_3, I.per_4, I.per_5, I.min_7 ]

-- | The neutral pentatonic scale.
pentatonicNeutral :: Scale
pentatonicNeutral =
  Pentatonic $ fromFoldable' [ I.maj_2, I.per_4, I.per_5, I.min_7 ]

--
-- Church Modes
--

-- | The Ionian mode, the first mode of the major scale. This is equivalent to
-- the major scale.
ionian :: Scale
ionian = major

-- | The Dorian mode, the second mode of the major scale.
dorian :: Scale
dorian =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.min_3, I.per_4, I.per_5, I.maj_6, I.min_7 ]

-- | The Phrygian mode, the third mode of the major scale.
phrygian :: Scale
phrygian =
  Heptatonic
    $ fromFoldable' [ I.min_2, I.min_3, I.per_4, I.per_5, I.min_6, I.min_7 ]

-- | The Lydian mode, the fourth mode of the major scale.
lydian :: Scale
lydian =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.maj_3, I.aug_4, I.per_5, I.maj_6, I.maj_7 ]

-- | The Mixolydian mode, the fifth mode of the major scale.
mixolydian :: Scale
mixolydian =
  Heptatonic
    $ fromFoldable' [ I.maj_2, I.maj_3, I.per_4, I.per_5, I.maj_6, I.min_7 ]

-- | The Aeolian mode, the sixth mode of the major scale. This is equivalent to
-- the natural minor scale.
aeolian :: Scale
aeolian = minor

-- | The Locrian mode, the seventh and final mode of the major scale.
locrian :: Scale
locrian =
  Heptatonic
    $ fromFoldable' [ I.min_2, I.min_3, I.per_4, I.dim_5, I.min_6, I.min_7 ]
