module Data.Music.Scales
  -- Common Scales
  ( major
  , minor
  , harmonicMinor
  , melodicMinor
  , blues
  , wholeTone
  , chromatic

  -- Pentatonic Scales
  , pentatonicMajor
  , pentatonicMinor
  , pentatonicNeutral

  -- Octatonic Scales
  , wholeHalf
  , diminished
  , halfWhole
  , diminishedDominant
  , majorBebop
  , minorBebop
  , melodicMinorBebop
  , dorianBebop
  , bebopDominant
  , seventhFlat5Diminished

  -- Church Modes
  , ionian
  , dorian
  , phrygian
  , lydian
  , mixolydian
  , aeolian
  , locrian

  -- Modes of Harmonic Minor
  , locrianNat6
  , ionianSharp5
  , dorianSharp4
  , phrygianDominant
  , lydianSharp2
  , superLocrianDblFlat7

  -- Modes of Melodic Minor
  ,  dorianFlat2
  , lydianAugmented
  , lydianDominant, overtone
  , mixolydianFlat6
  , halfDiminished
  , altered, superLocrian

  -- Harmonic Major and its Modes
  , harmonicMajor
  , dorianFlat5
  , phrygianFlat4
  , lydianFlat3
  , mixolydianFlat2
  , lydianAugmentedSharp2
  , locrianDblFlat7

  -- Double Harmonic Major and its Modes
  , dblHarmonicMajor
  , lydianSharp2Sharp6
  , ultraphrygian
  , dblHarmonicMinor, gypsyMinor, hungarianMinor
  , oriental
  , ionianSharp2Sharp5
  , locrianDblFlat3DblFlat7

  -- Neapolitan Scales and their Modes
  , neapolitanMajor
  , leadingWholeTone
  , lydianAugmentedDominant
  , lydianDominantFlat6
  , majorLocrian
  , halfDiminishedFlat4
  , alteredDominantDblFlat3

  , neapolitanMinor
  , lydianSharp6
  , mixolydianAugmented
  , aeolianSharp4
  , locrianDominant
  , ionianSharp2
  , ultralocrian
  ) where

import qualified Data.Music.Interval as I
import           Data.Music.Scale (Scale(..), unsafeMakeScale)

--
-- Common Scales
--

-- | The major scale.
major :: Scale
major =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.per_4, I.per_5, I.maj_6, I.maj_7 ]

-- | The natural minor scale.
minor :: Scale
minor =
  unsafeMakeScale [ I.maj_2, I.min_3, I.per_4, I.per_5, I.min_6, I.min_7 ]

-- | The harmonic minor scale.
harmonicMinor :: Scale
harmonicMinor =
  unsafeMakeScale [ I.maj_2, I.min_3, I.per_4, I.per_5, I.min_6, I.maj_7 ]

-- | The melodic minor scale (ascending).
melodicMinor :: Scale
melodicMinor =
  unsafeMakeScale [ I.maj_2, I.min_3, I.per_4, I.per_5, I.maj_6, I.maj_7 ]

-- | The (hexatonic) blues scale, built from the minor pentatonic scale plus the
-- flat 5th degree.
blues :: Scale
blues = unsafeMakeScale [ I.min_3, I.per_4, I.dim_5, I.per_5, I.min_7 ]

-- | The whole-tone scale.
wholeTone :: Scale
wholeTone = unsafeMakeScale [ I.maj_2, I.maj_3, I.aug_4, I.aug_5, I.min_7 ]

-- | The chromatic scale.
chromatic :: Scale
chromatic =
  unsafeMakeScale
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
pentatonicMajor = unsafeMakeScale [ I.maj_2, I.maj_3, I.per_5, I.maj_6 ]

-- | The minor pentatonic scale.
pentatonicMinor :: Scale
pentatonicMinor = unsafeMakeScale [ I.min_3, I.per_4, I.per_5, I.min_7 ]

-- | The neutral pentatonic scale.
pentatonicNeutral :: Scale
pentatonicNeutral = unsafeMakeScale [ I.maj_2, I.per_4, I.per_5, I.min_7 ]

--
-- Octatonic Scales
--

-- | The octatonic scale built using alternating whole- and half-step intervals.
wholeHalf :: Scale
wholeHalf =
  unsafeMakeScale
    [ I.maj_2
    , I.min_3
    , I.per_4
    , I.dim_5
    , I.min_6
    , I.maj_6
    , I.maj_7
    ]

-- | The diminished scale.
diminished :: Scale
diminished = wholeHalf

-- | The octatonic scale built using alternating half- and whole-step intervals.
halfWhole :: Scale
halfWhole =
  unsafeMakeScale
    [ I.min_2
    , I.min_3
    , I.maj_3
    , I.dim_5
    , I.per_5
    , I.maj_6
    , I.min_7
    ]

-- | The diminished dominant scale.
diminishedDominant :: Scale
diminishedDominant = halfWhole

-- | The major bebop scale - the major scale with an added ♯5.
majorBebop :: Scale
majorBebop =
  unsafeMakeScale
    [ I.maj_2
    , I.maj_3
    , I.per_4
    , I.per_5
    , I.aug_5
    , I.maj_6
    , I.maj_7
    ]

-- | The minor bebop scale - the minor scale with an added ♯7.
minorBebop :: Scale
minorBebop =
  unsafeMakeScale
    [ I.maj_2
    , I.min_3
    , I.per_4
    , I.per_5
    , I.min_6
    , I.min_7
    , I.maj_7
    ]

-- | The melodic minor bebop scale - the melodic minor scale with an added ♯5.
melodicMinorBebop :: Scale
melodicMinorBebop =
  unsafeMakeScale
    [ I.maj_2
    , I.min_3
    , I.per_4
    , I.per_5
    , I.aug_5
    , I.maj_6
    , I.maj_7
    ]

-- | The Dorian bebop scale - the Dorian mode with an added ♯3.
dorianBebop :: Scale
dorianBebop =
  unsafeMakeScale
    [ I.maj_2
    , I.min_3
    , I.maj_3
    , I.per_4
    , I.per_5
    , I.maj_6
    , I.min_7
    ]

-- | The bebop dominant scale - the Mixolydian mode with an added ♯7.
bebopDominant :: Scale
bebopDominant =
  unsafeMakeScale
    [ I.maj_2
    , I.maj_3
    , I.per_4
    , I.per_5
    , I.maj_6
    , I.min_7
    , I.maj_7
    ]

-- | The seventh ♭5 diminished scale - the result of building a scale using a
-- m7♭5 (half-diminished) chord on the first scale degree, and a fully
-- diminished chord on the second scale degree.
seventhFlat5Diminished :: Scale
seventhFlat5Diminished =
  unsafeMakeScale
    [ I.maj_2
    , I.maj_3
    , I.per_4
    , I.dim_5
    , I.min_6
    , I.min_7
    , I.maj_7
    ]

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
  unsafeMakeScale [ I.maj_2, I.min_3, I.per_4, I.per_5, I.maj_6, I.min_7 ]

-- | The Phrygian mode, the third mode of the major scale.
phrygian :: Scale
phrygian =
  unsafeMakeScale [ I.min_2, I.min_3, I.per_4, I.per_5, I.min_6, I.min_7 ]

-- | The Lydian mode, the fourth mode of the major scale.
lydian :: Scale
lydian =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.aug_4, I.per_5, I.maj_6, I.maj_7 ]

-- | The Mixolydian mode, the fifth mode of the major scale.
mixolydian :: Scale
mixolydian =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.per_4, I.per_5, I.maj_6, I.min_7 ]

-- | The Aeolian mode, the sixth mode of the major scale. This is equivalent to
-- the natural minor scale.
aeolian :: Scale
aeolian = minor

-- | The Locrian mode, the seventh and final mode of the major scale.
locrian :: Scale
locrian =
  unsafeMakeScale [ I.min_2, I.min_3, I.per_4, I.dim_5, I.min_6, I.min_7 ]

--
-- Modes of Harmonic Minor
--

-- | Locrian ♮6, the second mode of the harmonic minor scale.
locrianNat6 :: Scale
locrianNat6 =
  unsafeMakeScale [ I.min_2, I.min_3, I.per_4, I.dim_5, I.maj_6, I.min_7 ]

-- | Ionian ♯5, the third mode of the harmonic minor scale.
ionianSharp5 :: Scale
ionianSharp5 =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.per_4, I.aug_5, I.maj_6, I.maj_7 ]

-- | Dorian ♯4, the fourth mode of the harmonic minor scale.
dorianSharp4 :: Scale
dorianSharp4 =
  unsafeMakeScale [ I.maj_2, I.min_3, I.aug_4, I.per_5, I.maj_6, I.min_7 ]

-- | Phrygian Dominant, the fifth mode of the harmonic minor scale.
phrygianDominant :: Scale
phrygianDominant =
  unsafeMakeScale [ I.min_2, I.maj_3, I.per_4, I.per_5, I.min_6, I.min_7 ]

-- | Lydian ♯2, the sixth mode of the harmonic minor scale.
lydianSharp2 :: Scale
lydianSharp2 =
  unsafeMakeScale [ I.aug_2, I.maj_3, I.aug_4, I.per_5, I.maj_6, I.maj_7 ]

-- | Super Locrian 𝄫7, the seventh and final mode of the harmonic minor scale.
superLocrianDblFlat7 :: Scale
superLocrianDblFlat7 =
  unsafeMakeScale [ I.min_2, I.min_3, I.dim_4, I.dim_5, I.min_6, I.dim_7 ]

--
-- Modes of Melodic Minor
--

-- | Dorian ♭2, the second mode of the melodic minor scale.
dorianFlat2 :: Scale
dorianFlat2 =
  unsafeMakeScale [ I.min_2, I.min_3, I.per_4, I.per_5, I.maj_6, I.min_7 ]

-- | Lydian Augmented, the third mode of the melodic minor scale.
lydianAugmented :: Scale
lydianAugmented =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.aug_4, I.aug_5, I.maj_6, I.maj_7 ]

-- | Lydian Dominant, the fourth mode of the melodic minor scale.
lydianDominant :: Scale
lydianDominant =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.aug_4, I.per_5, I.maj_6, I.min_7 ]

-- | The Overtone Scale, the fourth mode of the melodic minor scale.
overtone :: Scale
overtone = lydianDominant

-- | Mixolydian ♭6, the fifth mode of the melodic minor scale.
mixolydianFlat6 :: Scale
mixolydianFlat6 =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.per_4, I.per_5, I.min_6, I.min_7 ]

-- | The Half-Diminished Scale, the sixth mode of the melodic minor scale.
halfDiminished :: Scale
halfDiminished =
  unsafeMakeScale [ I.maj_2, I.min_3, I.per_4, I.dim_5, I.min_6, I.min_7 ]

-- | The Altered Scale, the seventh mode of the melodic minor scale.
altered :: Scale
altered =
  unsafeMakeScale [ I.min_2, I.min_3, I.dim_4, I.dim_5, I.min_6, I.dim_7 ]

-- | Super Locrian, the seventh mode of the melodic minor scale.
superLocrian :: Scale
superLocrian = altered

--
-- Harmonic Major and its Modes
--

-- | The harmonic major scale - the major scale with a lowered 6th scale degree.
harmonicMajor :: Scale
harmonicMajor =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.per_4, I.per_5, I.min_6, I.maj_7 ]

-- | Dorian ♭5, the second mode of the harmonic major scale.
dorianFlat5 :: Scale
dorianFlat5 =
  unsafeMakeScale [ I.maj_2, I.min_3, I.per_4, I.dim_5, I.maj_6, I.min_7 ]

-- | Phrygian ♭4, the third mode of the harmonic major scale.
phrygianFlat4 :: Scale
phrygianFlat4 =
  unsafeMakeScale [ I.min_2, I.min_3, I.dim_4, I.per_5, I.min_6, I.min_7 ]

-- | Lydian ♭3, the fourth mode of the harmonic major scale.
lydianFlat3 :: Scale
lydianFlat3 =
  unsafeMakeScale [ I.maj_2, I.min_3, I.aug_4, I.per_5, I.maj_6, I.maj_7 ]

-- | Mixolydian ♭2, the fifth mode of the harmonic major scale.
mixolydianFlat2 :: Scale
mixolydianFlat2 =
  unsafeMakeScale [ I.min_2, I.maj_3, I.per_4, I.per_5, I.maj_6, I.min_7 ]

-- | Lydian Augmented ♯2, the sixth mode of the harmonic major scale.
lydianAugmentedSharp2 :: Scale
lydianAugmentedSharp2 =
  unsafeMakeScale [ I.aug_2, I.maj_3, I.aug_4, I.aug_5, I.maj_6, I.maj_7 ]

-- | Locrian 𝄫7, the seventh and final mode of the harmonic major scale.
locrianDblFlat7 :: Scale
locrianDblFlat7 =
  unsafeMakeScale [ I.min_2, I.min_3, I.per_4, I.dim_5, I.min_6, I.dim_7 ]

--
-- Double Harmonic Major and its Modes
--

-- | The double harmonic major scale - the major scale with lowered 2nd and 6th
-- scale degrees.
dblHarmonicMajor :: Scale
dblHarmonicMajor =
  unsafeMakeScale [ I.min_2, I.maj_3, I.per_4, I.per_5, I.min_6, I.maj_7 ]

-- | Lydian ♯2 ♯6, the second mode of the double harmonic major scale.
lydianSharp2Sharp6 :: Scale
lydianSharp2Sharp6 =
  unsafeMakeScale [ I.aug_2, I.maj_3, I.aug_4, I.per_5, I.aug_6, I.maj_7 ]

-- | Ultraphyrgian, the third mode of the double harmonic major scale.
ultraphrygian :: Scale
ultraphrygian =
  unsafeMakeScale [ I.min_2, I.min_3, I.dim_4, I.per_5, I.min_6, I.dim_7 ]

-- | Double harmonic minor, the fourth mode of the double harmonic major scale.
dblHarmonicMinor :: Scale
dblHarmonicMinor =
  unsafeMakeScale [ I.maj_2, I.min_3, I.aug_4, I.per_5, I.min_6, I.maj_7 ]

-- | Gypsy minor, the fourth mode of the double harmonic major scale.
gypsyMinor :: Scale
gypsyMinor = dblHarmonicMinor

-- | Hungarian minor, the fourth mode of the double harmonic major scale.
hungarianMinor :: Scale
hungarianMinor = dblHarmonicMinor

-- | The Oriental scale, the fifth mode of the double harmonic major scale.
oriental :: Scale
oriental =
  unsafeMakeScale [ I.min_2, I.maj_3, I.per_4, I.dim_5, I.maj_6, I.min_7 ]

-- | Ionian ♯2 ♯5, the sixth mode of the double harmonic major scale.
ionianSharp2Sharp5 :: Scale
ionianSharp2Sharp5 =
  unsafeMakeScale [ I.aug_2, I.maj_3, I.per_4, I.aug_5, I.maj_6, I.maj_7 ]

-- | Locrian 𝄫3 𝄫7, the seventh and final mode of the double harmonic major
-- scale.
locrianDblFlat3DblFlat7 :: Scale
locrianDblFlat3DblFlat7 =
  unsafeMakeScale [ I.min_2, I.dim_3, I.per_4, I.dim_5, I.min_6, I.dim_7 ]

--
-- Neapolitan Scales and their Modes
--

-- | The Neapolitan major scale.
neapolitanMajor :: Scale
neapolitanMajor =
  unsafeMakeScale [ I.min_2, I.min_3, I.per_4, I.per_5, I.maj_6, I.maj_7 ]

-- | The leading whole tone scale, the second mode of the Neapolitan major
-- scale.
leadingWholeTone :: Scale
leadingWholeTone =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.aug_4, I.aug_5, I.aug_6, I.maj_7 ]

-- | Lydian augmented dominant, the third mode of the Neapolitan major scale.
lydianAugmentedDominant :: Scale
lydianAugmentedDominant =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.aug_4, I.aug_5, I.maj_6, I.min_7 ]

-- | Lydian dominant ♭6, the fourth mode of the Neapolitan major scale.
lydianDominantFlat6 :: Scale
lydianDominantFlat6 =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.aug_4, I.per_5, I.min_6, I.min_7 ]

-- | Major Locrian, the fifth mode of the Neapolitan major scale.
majorLocrian :: Scale
majorLocrian =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.per_4, I.dim_5, I.min_6, I.min_7 ]

-- | The half-diminished ♭4 scale, the sixth mode of the Neapolitan major scale.
halfDiminishedFlat4 :: Scale
halfDiminishedFlat4 =
  unsafeMakeScale [ I.maj_2, I.min_3, I.dim_4, I.dim_5, I.min_6, I.min_7 ]

-- | Altered dominant 𝄫3, the seventh and final mode of the Neapolitan
-- major scale.
alteredDominantDblFlat3 :: Scale
alteredDominantDblFlat3 =
  unsafeMakeScale [ I.min_2, I.dim_3, I.dim_4, I.dim_5, I.min_6, I.min_7 ]

-- | The Neapolitan minor scale.
neapolitanMinor :: Scale
neapolitanMinor =
  unsafeMakeScale [ I.min_2, I.min_3, I.per_4, I.per_5, I.min_6, I.maj_7 ]

-- | Lydian ♯6, the second mode of the Neapolitan minor scale.
lydianSharp6 :: Scale
lydianSharp6 =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.aug_4, I.per_5, I.aug_6, I.maj_7 ]

-- | Mixolydian augmented, the third mode of the Neapolitan minor scale.
mixolydianAugmented :: Scale
mixolydianAugmented =
  unsafeMakeScale [ I.maj_2, I.maj_3, I.per_4, I.aug_5, I.maj_6, I.min_7 ]

-- | Aeolian ♯2, the fourth mode of the Neapolitan minor scale.
aeolianSharp4 :: Scale
aeolianSharp4 =
  unsafeMakeScale [ I.maj_2, I.min_3, I.aug_4, I.per_5, I.min_6, I.min_7 ]

-- | Locrian dominant, the fifth mode of the Neapolitan minor scale.
locrianDominant :: Scale
locrianDominant =
  unsafeMakeScale [ I.min_2, I.maj_3, I.per_4, I.dim_5, I.min_6, I.min_7 ]

-- | Ionian ♯2, the sixth mode of the Neapolitan minor scale.
ionianSharp2 :: Scale
ionianSharp2 =
  unsafeMakeScale [ I.aug_2, I.maj_3, I.per_4, I.per_5, I.maj_6, I.maj_7 ]

-- | Ultralocrian, the seventh and final mode of the Neapolitan minor scale.
ultralocrian :: Scale
ultralocrian =
  unsafeMakeScale [ I.min_2, I.dim_3, I.dim_4, I.dim_5, I.min_6, I.dim_7 ]
