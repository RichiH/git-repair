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
	logTest damage result
	case result of
		Just True -> exitSuccess
		_ -> exitFailure

data TestLog = TestLog [Git.Destroyer.Damage] (Maybe Bool)
	deriving (Read, Show)

logTest :: [Git.Destroyer.Damage] -> Maybe Bool -> IO ()
logTest damage result =
	appendFile "test-runner.log" $ show (TestLog damage result) ++ "\n"
