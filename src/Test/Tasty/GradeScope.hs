{-# LANGUAGE FlexibleInstances, RecordWildCards, DeriveGeneric #-}
-- | Scored tests and Gradescope JSON output for Tasty
module Test.Tasty.GradeScope where

import Test.Tasty
import Test.Tasty.Providers
import Test.Tasty.Options
  ( IsOption(..)
  , OptionSet(..)
  , OptionDescription(..)
  , lookupOption
  , safeRead
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
  , Traversal(..)
  , TreeFold(..)
  )
import qualified Test.Tasty.Runners as Runners
import Text.JSON hiding (Result)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad ((>=>), liftM)
import qualified Control.Monad.State as State
import Data.Functor.Const
import Data.Functor.Compose
import Data.Maybe
import Data.Monoid
import Data.IntMap (IntMap)
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
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

-- * Options

gradeOptions :: [OptionDescription]
gradeOptions =  [ Option (Proxy :: Proxy (Maybe GSScoreFile))
                , Option (Proxy :: Proxy Weight)
                , Option (Proxy :: Proxy Visibility)
                , Option (Proxy :: Proxy NegScoring)
                , Option (Proxy :: Proxy TotalPoints)
                ]

newtype TotalPoints = TotalPoints (Sum Int) deriving (Eq, Ord, Typeable)
instance IsOption TotalPoints where
  defaultValue = TotalPoints mempty
  parseValue   = Just . TotalPoints . pure . read
  optionName   = Tagged "TotalPoints"
  optionHelp   = Tagged "Total Points: if neg scoring this is max possible for the suite; else running total"

newtype NegScoring = NegScoring Bool deriving (Eq, Ord, Typeable)
instance IsOption NegScoring where
  defaultValue = NegScoring False
  parseValue   = Just . NegScoring . read
  optionName   = Tagged "NegScoring"
  optionHelp   = Tagged "Negative Scoring (deduct points from max)"

data Visibility = Hidden | AfterDue | AfterPub | Visible deriving (Eq, Ord, Typeable)
instance IsOption Visibility where
  defaultValue = Visible
  parseValue   = undefined
  optionName   = Tagged "Visibility"
  optionHelp   = Tagged "Is this test visible?"

newtype Weight = Weight Int deriving (Eq, Ord, Typeable)
instance IsOption Weight where
  defaultValue = Weight 1
  parseValue   = fmap Weight . safeRead
  optionName   = Tagged "Weight"
  optionHelp   = Tagged "How many points a test is worth"

newtype GSScoreFile = GSScoreFile FilePath deriving (Eq, Ord, Typeable)
instance IsOption (Maybe GSScoreFile) where
  defaultValue = Just (GSScoreFile "result.json")
  parseValue   = Just . Just . GSScoreFile
  optionName   = Tagged "scores"
  optionHelp   = Tagged "A file path to output scores, as a JSON file in GradeScope format"

-- ** Toggles

scored :: Int -> TestTree -> TestTree
scored n = localOption (Weight n)

visibility :: Visibility -> TestTree -> TestTree
visibility v = localOption v

-- * Reporter

gsConsoleReporter :: Ingredient
gsConsoleReporter = Runners.consoleTestReporter `composeReporters` gradescopeReporter

gradescopeReporter :: Ingredient
gradescopeReporter = TestReporter gradeOptions runner
  where
    runner opts tests = do
      GSScoreFile output <- lookupOption opts
      pure $ scoreTests output opts tests

scoreTests :: FilePath -> OptionSet -> TestTree -> (StatusMap -> IO (Time -> IO Bool))
scoreTests outfile opts tests = \testStatus -> do
  TotalPoints totalPoints <- pure (lookupOption opts)
  Const summary <- flip State.evalStateT 0 $ getCompose $ getTraversal $
    Runners.foldTestTree (foldScores testStatus) opts tests
  return $ \time -> do
    writeFile outfile $
      (encode (toJSObject [ ("execution_time" , showJSON (ceiling time :: Int))
                          , ("score"          , showJSON (getSum (runningTotal summary <> totalPoints)))
                          , ("tests"          , showJSON summary)
                          ]))
    return $ numFailures summary == mempty

-- * Internals

data TestResult = TestResult
  { resultId       :: Int
  , resultName     :: TestName
  , resultWeight   :: Int
  , resultMetadata :: Result
  , resultVisible  :: Visibility
  }

instance JSON TestResult where
  readJSON = undefined
  showJSON tr@TestResult{..} =
    JSObject $ toJSObject
      [ ("name"       , showJSON resultName)
      , ("score"      , showJSON score)
      , ("max_score"  , showJSON resultWeight)
      , ("number"     , showJSON resultId)
      , ("output"     , showJSON (Runners.resultDescription resultMetadata))
      , ("visibility" , showJSON resultVisible)
      ]
    where
      score | resultSuccessful tr = resultWeight
            | otherwise = 0

resultSuccessful :: TestResult -> Bool
resultSuccessful = Runners.resultSuccessful . resultMetadata

instance JSON Visibility where
  readJSON = undefined
  showJSON Hidden    = showJSON "hidden"
  showJSON AfterDue  = showJSON "after_due_date"
  showJSON AfterPub  = showJSON "after_published"
  showJSON Visible   = showJSON "visible"

data ScoreSummary = ScoreSummary
  { individualTests :: [TestResult]
  , runningTotal    :: Sum Int
  , numFailures     :: Sum Int
  } deriving Generic

instance JSON ScoreSummary where
  readJSON = undefined
  showJSON ss = showJSON (individualTests ss)

instance Monoid ScoreSummary where
  mempty = ScoreSummary mempty mempty mempty
  (ScoreSummary ts1 tot1 f1) `mappend` (ScoreSummary ts2 tot2 f2) =
    ScoreSummary (ts1<>ts2) (tot1<>tot2) (f1<>f2)

type ScoreTraversal = Traversal (Compose (State.StateT Int IO) (Const ScoreSummary))

foldScores :: StatusMap -> TreeFold ScoreTraversal
foldScores statusMap = Runners.trivialFold
                       { foldSingle = scoreSingleTest statusMap
                       , foldGroup = scoreGroup
                       }

scoreSingleTest :: IsTest t
                => StatusMap -> OptionSet -> TestName -> t -> ScoreTraversal
scoreSingleTest statusMap options resultName _ = Traversal $ Compose $ do
  resultId <- State.get
  let Weight resultWeight = lookupOption options
      resultVisible = lookupOption options :: Visibility
      NegScoring ns = lookupOption options
  testResult <- State.lift $ do
    resultMetadata <- atomically . waitFinished $ statusMap IntMap.! resultId
    return TestResult{..}
  let
    failed = not (resultSuccessful testResult)
    summary = ScoreSummary [testResult] (scoreQ resultWeight failed ns) (countFail failed)
  Const summary <$ State.modify (+1)
  where
      -- negative scoring, fail
    scoreQ w True True   = Sum (-w)
      -- positive scoring, pass
    scoreQ w False False = Sum w
    scoreQ w _    _      = Sum 0

    countFail True = Sum 1
    countFail False = Sum 0

waitFinished :: TVar Status -> STM Result
waitFinished = readTVar >=> \st ->
  case st of
    Done x -> pure x
    _      -> retry

scoreGroup :: TestName -> ScoreTraversal -> ScoreTraversal
scoreGroup group kids = kids
