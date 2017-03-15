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
  , consoleTestReporter
  , resultSuccessful
  , resultDescription
  , testsNames
  )

import Text.JSON hiding (Result)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad ((>=>), liftM)
import qualified Control.Monad.State as State
import Data.Char (isDigit)
import Data.Functor.Const
import Data.Functor.Compose
import Data.Monoid
import Data.List.Split (splitOn)
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

data VisibilityOpts
  = Hidden | AfterDue | AfterPub | Visible

newtype Visibility = Visibility VisibilityOpts deriving Typeable

instance IsOption Visibility where
  defaultValue = Visibility Visible
  parseValue   = undefined
  optionName   = Tagged "Visibility"
  optionHelp   = Tagged "Is this test visible?"

newtype Weight = Weight Int deriving Typeable

instance IsOption Weight where
  defaultValue = Weight 1
  parseValue   = read
  optionName   = Tagged "Weight"
  optionHelp   = Tagged "How many points a test is worth"

newtype GSScoreFile = GSScoreFile FilePath deriving Typeable

instance IsOption (Maybe GSScoreFile) where
  defaultValue = Just (GSScoreFile "output.json")
  parseValue   = Just . Just . GSScoreFile
  optionName   = Tagged "scores"
  optionHelp   = Tagged "A file path to output scores, as a JSON file in GradeScope format"

gradeOptions :: [OptionDescription]
gradeOptions = [ Option (Proxy :: Proxy (Maybe GSScoreFile))
               , Option (Proxy :: Proxy Weight)
               ]

gsConsoleReporter :: Ingredient
gsConsoleReporter = consoleTestReporter `composeReporters` gradescopeReporter

gradescopeReporter :: Ingredient
gradescopeReporter = TestReporter gradeOptions runner
  where
    runner opts tests = do
      GSScoreFile output <- lookupOption opts
      scoreTests output opts tests

scoreTests :: FilePath -> OptionSet -> TestTree -> (StatusMap -> IO (Time -> IO Bool))
scoreTests outfile opts tests = \testStatus -> do
  Const summary <- flip State.evalStateT 0 $ getCompose $ getTraversal $
    foldTestTree foldScores opts tests
  return $ \time -> do
    writeFile outfile $ (encode (toJSObject [ ("execution_time", showJSON (ceiling time :: Int))
                                            , ("tests", showJSON summary)]))
    return . (0 ==) . getSum . numFailures $ summary

data TestInfo = TestInfo
  { testWorth :: Int
  , testName  :: TestName
  } deriving Show

data TestResult = TestResult
  { resultId       :: Int
  , resultName     :: TestName
  , resultWeight   :: Int
  , resultMetadata :: Result
  , resultVisible  :: VisibilityOpts
  }

instance JSON TestResult where
  readJSON = undefined
  showJSON TestResult{..} =
    JSObject $ toJSObject
      [ ("name", showJSON resultName)
      , ("score", showJSON score)
      , ("max_score", showJSON resultWeight)
      , ("number", showJSON resultId)
      , ("output", showJSON (resultDescription resultMetadata))
      , ("visibility", showJSON resultVisible)
      ]
    where
      score | resultSuccessful resultMetadata = resultWeight
            | otherwise = 0

instance JSON VisibilityOpts where
  readJSON = undefined
  showJSON Hidden    = showJSON "hidden"
  showJSON AfterDue  = showJSON "after_due_date"
  showJSON AfterPub  = showJSON "after_published"
  showJSON Visible   = showJSON "visible"

data ScoreSummary = ScoreSummary
  { individualTests :: [TestResult]
  , numFailures :: Sum Int
  } deriving Generic

instance JSON ScoreSummary where
  readJSON = undefined
  showJSON ss = toJSObject $
    [ ("tests", showJSON ss) ]

instance Monoid ScoreSummary where
  mempty = ScoreSummary mempty mempty
  ss1 `mappend` ss2 =
    ScoreSummary (individualTests ss1 <> individualTests ss2)
                 (numFailures ss1 <> numFailures ss2)

type ScoreTraversal = Traversal (Compose (State.StateT Int IO) (Const ScoreSummary))

scoreSingleTest :: IsTest t
                => StatusMap -> OptionSet -> TestName -> t -> ScoreTraversal
scoreSingleTest statusMap options resultName _ = Traversal $ Compose $ do
  resultId <- State.get
  let Weight resultWeight = lookupOption options
      Visibility resultVisible = lookupOption options
  summary <- liftM $ do
    resultMetadata <- atomically . waitFinished $ resultId IntMap.! statusMap
    return TestResult{..}
  Const summary <$ State.modify (+1)

waitFinished :: TVar Status -> STM Result
waitFinished = readTVar >=> \st ->
  case st of
    Done x -> pure x
    _      -> retry

scoreGroup :: TestName -> ScoreTraversal -> ScoreTraversal
scoreGroup group kids = kids

foldScores :: TreeFold ScoreTraversal
foldScores = trivialFold { foldSingle = scoreSingleTest statusMap
                         , foldGroup = scoreGroup
                         }

scored :: Int -> TestTree -> TestTree
scored n = localOption (Weight n)

visibility :: VisibilityOpts -> TestTree -> TestTree
visibility v = localOption (Visibility v)
