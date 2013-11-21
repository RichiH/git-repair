{- git-repair program
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import Options.Applicative

import Common
import qualified Git.CurrentRepo
import qualified Git.Repair
import qualified Git.Config
import qualified Git.Construct
import qualified Git.Destroyer
import qualified Git.Fsck
import Utility.Tmp

data Settings = Settings
	{ forced :: Bool
	, testMode :: Bool
	, retryTestMode :: Bool
	, numTests :: Int
	}

parseSettings :: Parser Settings
parseSettings = Settings
	<$> switch
		( long "force"
		<> help "Force repair, even if data is lost"
		)
	<*> switch
		( long "test"
		<> help "Clone local repo, damage the clone, and test repair"
		)
	<*> switch
		( long "retry"
		<> help "Retry tests in git-repair-test.log"
		)
	<*> option
		( long "numtests"
		<> short 'n'
		<> metavar "N"
		<> help "Run N tests"
		<> value 1
		)

main :: IO ()
main = execParser opts >>= go
  where
  	opts = info (helper <*> parseSettings) desc
	desc = fullDesc
		<> header "git-repair - repair a damanged git repository" 
	go settings
		| testMode settings = test settings
		| retryTestMode settings = retryTest settings
		| otherwise = repair settings

repair :: Settings -> IO ()
repair settings = do
	g <- Git.Config.read =<< Git.CurrentRepo.get
	ifM (Git.Repair.successfulRepair <$> Git.Repair.runRepair (forced settings) g)
		( exitSuccess
		, exitFailure
		)

test :: Settings -> IO ()
test settings = do
	forM_ [1 .. numTests settings] $ \n -> do
		putStrLn $ "** Test " ++ show n ++ "/" ++ show (numTests settings)
		damage <- Git.Destroyer.generateDamage
		logDamage damage
		runTest settings damage >>= handleTestResult
	allOk

retryTest :: Settings -> IO ()
retryTest settings = do
	l <- map Prelude.read . lines <$> readFile logFile
	forM_ l $ \damage -> 
		runTest settings damage >>= handleTestResult
	allOk

runTest :: Settings -> [Git.Destroyer.Damage] -> IO TestResult
runTest settings damage = withTmpDir "tmprepo" $ \tmpdir -> do
	let cloneloc = tmpdir </> "clone"
	cloned <- boolSystem "git"
		[ Param "clone"
		, Param "--no-hardlinks"
		, File "."
		, File cloneloc
		]
	unless cloned $
		error $ "failed to clone this repo"
	g <- Git.Config.read =<< Git.Construct.fromPath cloneloc
	Git.Destroyer.applyDamage damage g
	repairstatus <- catchMaybeIO $ Git.Repair.successfulRepair
		<$> Git.Repair.runRepair (forced settings) g
	case repairstatus of
		Just True -> TestResult repairstatus 
			. Just . not . Git.Fsck.foundBroken
			<$> Git.Fsck.findBroken False g
		_ -> return $ TestResult repairstatus Nothing

data TestResult = TestResult
	{ repairResult :: Maybe Bool
	, fsckResult ::  Maybe Bool
	}
	deriving (Read, Show)

handleTestResult :: TestResult -> IO ()
handleTestResult testresult = do
	ok <- case (repairResult testresult, fsckResult testresult) of
		(Just True, Just True) -> do
			putStrLn "** repair succeeded"
			return True
		(Just True, Just False) -> do
			putStrLn "** repair succeeded, but final fsck failed"
			return False
		_ -> do
			putStrLn "** repair failed"
			return False
	unless ok $
		exitFailure

allOk :: IO ()
allOk = do
	putStrLn ""
	putStrLn "All tests ok!"

logDamage :: [Git.Destroyer.Damage] -> IO ()
logDamage damage = appendFile logFile $ show damage ++ "\n"

logFile :: FilePath
logFile = "git-repair-test.log"
