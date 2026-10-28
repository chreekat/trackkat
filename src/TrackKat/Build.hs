{-# LANGUAGE CPP #-}

-- | What the build was told about itself.
module TrackKat.Build (buildVersion) where

-- | The version CI gives each build, such as @0.1.123+abc1234@: a number
-- from its run, and its commit. CI defines TRACKKAT_VERSION (see
-- .github/workflows/build.yml); a build without it is a development build,
-- which can't be installed or updated.
buildVersion :: Maybe String
#ifdef TRACKKAT_VERSION
buildVersion = Just TRACKKAT_VERSION
#else
buildVersion = Nothing
#endif
