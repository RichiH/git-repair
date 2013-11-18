{- Checks system configuration and generates SysConfig.hs. -}

module Build.Configure where

import System.Environment
import Control.Applicative
import Control.Monad.IfElse

import Build.TestConfig
import Git.Version

tests :: [TestCase]
tests =
	[ TestCase "git" $ requireCmd "git" "git --version >/dev/null"
	, TestCase "git version" getGitVersion
	, TestCase "nice" $ testCmd "nice" "nice true >/dev/null"
	]

getGitVersion :: Test
getGitVersion = Config "gitversion" . StringConfig . show
	<$> Git.Version.installed

run :: [TestCase] -> IO ()
run ts = do
	args <- getArgs
	config <- runTests ts
	writeSysConfig config
