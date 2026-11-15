-- | Checks of examples, written one after another as they share what they
-- look at, and gathered into a group of tests.
module Test.Check (
    Checks,
    group,
    check,
    checkShowing,
    tree,
    near,
) where

import System.Environment (lookupEnv)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, testCaseInfo)

-- | Tests gathered in the order they are written.
type Checks = (,) [TestTree]

group :: TestName -> Checks () -> TestTree
group name (tests, ()) = testGroup name tests

-- | A test that something holds.
check :: TestName -> Bool -> Checks ()
check name ok = ([testCase name (assertBool "" ok)], ())

-- | A test that something holds, with the figures it holds or fails by.
-- They are told when it fails, and whenever TRACKKAT_SHOW is set, to compare
-- one build's figures with another's. The name can be told without working
-- the figures out, so tests can be listed and chosen without running any.
checkShowing :: TestName -> String -> Bool -> Checks ()
checkShowing name figures ok = ([testCaseInfo name told], ())
  where
    told = do
        assertBool figures ok
        maybe "" (const figures) <$> lookupEnv "TRACKKAT_SHOW"

-- | Tests of another kind, among the checks.
tree :: TestTree -> Checks ()
tree t = ([t], ())

near :: Double -> Double -> Bool
near a b = abs (a - b) < 1e-9
