{- Test runner
 -
 - Passed a git repository, makes a temporary clone of the git repository,
 - corrupts part of it, then tries to repair it, and logs the result.
 -}

module Main where

import Options.Applicative

import Common
import qualified Git.Construct
import qualified Git.Config
import qualified Git.Destroyer
import qualified Git.Repair
import qualified Git.Fsck
import Utility.Tmp

data Settings = Settings
	{ originalGitRepo :: FilePath
	, forced :: Bool
	, retryMode :: Bool
	}

parseSettings :: Parser Settings
parseSettings = Settings
	<$> argument str (metavar "REPO")
        <*> switch forceopt
	<*> switch retryopt
  where
	forceopt = long "force"
		<> help "Force repair"
	retryopt = long "retry"
		<> help "Retry tests in test-runner.log"

main :: IO ()
main = execParser opts >>= run
  where
	opts = info (helper <*> parseSettings) desc
	desc = fullDesc
		<> header "test-runner - test command in corrupted git repository"

run :: Settings -> IO ()
run settings
	| retryMode settings = do
		l <- map Prelude.read . lines <$> readFile logFile
		rs <- forM l $ \damage -> 
			runTest settings damage >>= showTestResult
		exitBool $ and rs
	| otherwise = runRandomTest settings

runRandomTest :: Settings -> IO ()
runRandomTest settings = do
	damage <- Git.Destroyer.generateDamage
	logDamage damage
	result <- runTest settings damage
	showTestResult result >>= exitBool

runTest :: Settings -> [Git.Destroyer.Damage] -> IO TestResult
runTest settings damage = withTmpDir "tmprepo" $ \tmpdir -> do
	let cloneloc = tmpdir </> "clone"
	cloned <- boolSystem "git"
		[ Param "clone"
		, Param "--no-hardlinks"
		, File (originalGitRepo settings)
		, File cloneloc
		]
	unless cloned $
		error $ "failed to clone " ++ originalGitRepo settings
	g <- Git.Config.read =<< Git.Construct.fromPath cloneloc
	Git.Destroyer.applyDamage damage g
	repairstatus <- catchMaybeIO $ Git.Repair.successfulRepair
		<$> Git.Repair.runRepair (forced settings) g
	case repairstatus of
		Just True -> TestResult damage repairstatus 
			. Just . not . Git.Fsck.foundBroken
			<$> Git.Fsck.findBroken False g
		_ -> return $ TestResult damage repairstatus Nothing

data TestResult = TestResult
	{ damageList :: [Git.Destroyer.Damage]
	, repairResult :: Maybe Bool
	, fsckResult ::  Maybe Bool
	}
	deriving (Read, Show)

showTestResult :: TestResult -> IO Bool
showTestResult testresult = case (repairResult testresult, fsckResult testresult) of
	(Just True, Just True) -> do
		putStrLn "** repair succeeded"
		return True
	(Just True, Just False) -> do
		putStrLn "** repair succeeded, but final fsck failed"
		return False
	_ -> do
		putStrLn "** repair failed"
		return True

logDamage :: [Git.Destroyer.Damage] -> IO ()
logDamage damage = appendFile logFile $ show damage ++ "\n"

logFile :: FilePath
logFile = "test-runner.log"
