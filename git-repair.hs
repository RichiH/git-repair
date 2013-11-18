{- git-repair program
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import Data.Tuple.Utils
import Options.Applicative

import Common
import qualified Git.CurrentRepo
import qualified Git.Repair
import qualified Git.Config

data Settings = Settings
	{ forced :: Bool
	}

parseSettings :: Parser Settings
parseSettings = Settings
	<$> switch forceopt
  where
	forceopt = long "force"
		<> help "Force recovery, even if data is lost"

main :: IO ()
main = execParser opts >>= repair
  where
  	opts = info (helper <*> parseSettings) desc
	desc = fullDesc
		<> header "git-repair - repair a damanged git repository" 

repair :: Settings -> IO ()
repair settings = do
	g <- Git.Config.read =<< Git.CurrentRepo.get
	ifM (fst3 <$> Git.Repair.runRepair (forced settings) g)
		( exitSuccess
		, exitFailure
		)
