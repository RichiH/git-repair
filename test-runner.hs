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
	}

parseSettings :: Parser Settings
parseSettings = Settings
	<$> argument str (metavar "REPO")
        <*> switch forceopt
  where
	forceopt = long "force"
		<> help "Force repair"

main :: IO ()
main = execParser opts >>= runTest
  where
	opts = info (helper <*> parseSettings) desc
	desc = fullDesc
		<> header "test-runner - test command in corrupted git repository"

runTest :: Settings -> IO ()
runTest settings = withTmpDir "tmprepo" $ \tmpdir -> do
	let cloneloc = tmpdir </> "clone"
	cloned <- boolSystem "git"
		[ Param "clone"
		, Param "--no-hardlinks"
		, File (originalGitRepo settings)
		, File cloneloc
		]
	unless cloned $
		error $ "failed to clone " ++ originalGitRepo settings
	-- Note that we read the config before destroying the repo.
	-- Recovering from repos with a corrupted config is not currently
	-- a goal.
	g <- Git.Config.read =<< Git.Construct.fromPath cloneloc
	damage <- Git.Destroyer.generateDamage
	Git.Destroyer.applyDamage damage g
	result <- catchMaybeIO $ Git.Repair.successfulRepair
		<$> Git.Repair.runRepair (forced settings) g
	case result of
		Just True -> do
			fsckok <- not . Git.Fsck.foundBroken
				<$> Git.Fsck.findBroken False g
			logTest damage result (Just fsckok)
			if fsckok
				then do
					putStrLn "** repair succeeded"
					exitSuccess
				else do
					putStrLn "** repair succeeded, but final fsck failed"
					exitFailure
		_ -> do
			logTest damage result Nothing
			putStrLn "** repair failed"
			exitFailure

data TestLog = TestLog
	{ damagelist :: [Git.Destroyer.Damage]
	, resut :: Maybe Bool
	, fsckresult ::  Maybe Bool
	}
	deriving (Read, Show)

logTest :: [Git.Destroyer.Damage] -> Maybe Bool -> Maybe Bool -> IO ()
logTest damage result fsckok =
	appendFile "test-runner.log" $
		show (TestLog damage result fsckok) ++ "\n"
