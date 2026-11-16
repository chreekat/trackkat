{-# LANGUAGE DeriveTraversable #-}

-- | How each of the clip's reflectors was seen in a frame.
module TrackKat.Sight (
    Three (..),
    reflectors,
    Kind (..),
    Sight (..),
) where

import TrackKat.Pose (Reflector (..))

-- | One of something for each reflector: left, right, top.
data Three a = Three a a a
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Three where
    pure a = Three a a a
    Three f g h <*> Three a b c = Three (f a) (g b) (h c)

-- | Each reflector in its own place.
reflectors :: Three Reflector
reflectors = Three LeftReflector RightReflector TopReflector

-- | How a reflector appeared.
data Kind
    = -- | as a blob of its own
      Whole
    | -- | as two blobs close together, taken as one
      Rejoined
    | -- | sharing one blob with another reflector
      Merged
    deriving (Eq, Show)

-- | Where a reflector was seen, and how many pixels it lit.
data Sight = Sight
    { sightAt :: !(Double, Double)
    , sightPixels :: !Double
    , sightKind :: !Kind
    }
    deriving (Eq, Show)
