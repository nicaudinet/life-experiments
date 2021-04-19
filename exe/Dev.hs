{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.Optional (Optional(Specific))
import qualified Data.Text as T
import Data.Tuple (swap)
import Turtle
import Prelude hiding (FilePath)

data Action = Ghcid Executable | Run Executable | Test Executable
data Executable = Dev | RandomRule | EvolveSimpleRule | EvolveRule
  deriving (Show, Eq)

executablesMap :: [(Executable, Text)]
executablesMap =
  [ (Dev, "dev")
  , (RandomRule, "random-rule")
  , (EvolveSimpleRule, "evolve-simple-rule")
  , (EvolveRule, "evolve-rule")
  ]

executableToText :: Executable -> Text
executableToText
  = fromMaybe (error "Executable was not in the executable map")
  . flip lookup executablesMap

executableFromText :: Text -> Maybe Executable
executableFromText = flip lookup (map swap executablesMap)

parser :: Parser Action
parser
  =   subcommand "ghcid" "Ghcid actions" (fmap Ghcid subParser)
  <|> subcommand "run" "Run actions" (fmap Run subParser)
  where

    subParser :: Parser Executable
    subParser =
      arg
        executableFromText
        "exec"
        (Specific . HelpMessage $ T.intercalate ", " (map snd executablesMap))

ghcid :: Executable -> Shell ExitCode
ghcid exec = proc "ghcid" ["-c", command] mempty
  where
    command :: Text
    command = "cabal repl exe:" <> executableToText exec

run :: Executable -> Shell ExitCode
run exec = proc "cabal" ["run", "exe:" <> executableToText exec] mempty

test :: Executable -> Shell ExitCode
test EvolveSimpleRule = proc "cabal" ["test", "test-evolve-simple-rule"] mempty
test exec = error $ "Tests not implemented for executable " <> show exec

action :: Action -> Shell ExitCode
action (Ghcid exec) = ghcid exec
action (Run exec) = run exec
action (Test exec) = test exec

main :: IO ()
main = options "Dev script for life experiments" parser >>= sh . action
