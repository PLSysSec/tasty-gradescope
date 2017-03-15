{-# LANGUAGE FlexibleInstances #-}
-- | Scored tests and Gradescope JSON output for Tasty
module Test.Tasty.GradeScope where

import Test.Tasty
import Test.Tasty.Providers
import Test.Tasty.Options
  ( IsOption(..)
  , OptionSet(..)
  , OptionDescription(..)
  , lookupOption
  )
import Test.Tasty.Ingredients
  ( Ingredient(..)
  , composeReporters
  )
import Test.Tasty.Runners
  ( Ingredient(TestReporter)
  , Status(Done)
  , StatusMap
  , Time
  , consoleTestReporter
  , resultSuccessful
  , resultDescription
  , testsNames
  )

import Text.JSON hiding (Result)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad ((>=>))
import Data.Char (isDigit)
import Data.IntMap (IntMap)
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import Data.Typeable (Typeable)
import Text.ParserCombinators.ReadP
import qualified Data.IntMap as IntMap

-- | EXAMPLE OUTPUT
--
-- { "score": 44.0, // optional, but required if not on each test case below
--   "execution_time": 136, // optional, seconds
--   "output": "Text relevant to the entire submission", // optional
--   "visibility": "after_due_date" // Optional visibility setting
--   "tests": // Optional, but required if no top-level score
--     [
--         {
--             "score": 2.0, // optional, but required if not on top level submission
--             "max_score": 2.0, // optional
--             "name": "Your name here", // optional
--             "number": "1.1", // optional (will just be numbered in order of array if no number given)
--             "output": "Giant multiline string that will be placed in a <pre> tag and collapsed by default", // optional
--             "tags": ["tag1", "tag2", "tag3"] // optional
--             "visibility": "visible" // Optional visibility setting
--         },
--         // and more test cases...
--     ]
-- }

newtype GSScoreFile = GSScoreFile FilePath deriving Typeable

instance IsOption (Maybe GSScoreFile) where
  defaultValue = Just (GSScoreFile "output.json")
  parseValue   = Just . Just . GSScoreFile
  optionName   = Tagged "scores"
  optionHelp   = Tagged "A file path to output scores, as a JSON file in GradeScope format"

gsConsoleReporter :: Ingredient
gsConsoleReporter = consoleTestReporter `composeReporters` gradescopeReporter

gradescopeReporter :: Ingredient
gradescopeReporter = TestReporter optDesc runner
  where
    optDesc :: [OptionDescription]
    optDesc = [ Option (Proxy :: Proxy (Maybe GSScoreFile)) ]

    runner :: OptionSet -> TestTree -> Maybe (StatusMap -> IO (Time -> IO Bool))
    runner opts tree = do
      GSScoreFile output <- lookupOption opts
      pure $ scoreTests output (buildTestInfos opts tree)

data TestInfo = TestInfo
  { testWorth :: Int
  , testName  :: TestName
  } deriving Show

data TestResult = TestResult Int TestName Score Result
data Score = Score Int Int

instance JSON TestResult where
  readJSON = undefined
  showJSON (TestResult num name (Score got tot) rs) =
    JSObject $ toJSObject
      [ ("name", showJSON name)
      , ("score", showJSON got)
      , ("max_score", showJSON tot)
      , ("number", showJSON num)
      , ("output", showJSON (resultDescription rs))
      ]

testResult :: TestInfo -> Result -> (Int -> TestResult)
testResult (TestInfo tot name) rs num = TestResult num name (Score got tot) rs
  where
    got | resultSuccessful rs = tot
        | otherwise           = 0

-- A test name is a full path group1/group2/.../groupN/test
-- For now we focus only on the leaves.
-- TODO: coarse grained scoring by TestGroup
instance Read TestInfo where
  readsPrec _ = readP_to_S parseTestInfo . last . splitOn "/"

-- "[n] blah blah blah"
parseTestInfo :: ReadP TestInfo
parseTestInfo = do
  scoreWeight <- parseScore <++ pure 1
  skipSpaces
  name <- munch (const True)
  return (TestInfo scoreWeight name)
  where
    parseScore = between (char '[' >> skipSpaces)
                         (skipSpaces >> char ']')
                         (read <$> (many1 (satisfy isDigit)))

buildTestInfos :: OptionSet -> TestTree -> IntMap TestInfo
buildTestInfos opts tests =
  IntMap.fromList . zip [0..] . map read $ testsNames opts tests

scoreTests :: FilePath -> IntMap TestInfo -> (StatusMap -> IO (Time -> IO Bool))
scoreTests outfile testInfo testStatus = do
    return $ \time -> do
      rawResults <- atomically $ traverse waitFinished testStatus
      let testResults = IntMap.foldrWithKey (\key entry rss -> entry key : rss) mempty
                      $ zipMapWith testResult testInfo rawResults
      writeFile outfile $ (encode (toJSObject [ ("execution_time", showJSON (ceiling time :: Int))
                                              , ("tests", showJSON testResults)]))
      return . and $ resultSuccessful <$> rawResults

zipMapWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
zipMapWith f a b = IntMap.mapMaybeWithKey (\k v -> f v <$> IntMap.lookup k b) a

waitFinished :: TVar Status -> STM Result
waitFinished = readTVar >=> \st ->
  case st of
    Done x -> pure x
    _      -> retry
