module Main where

import System.Console.Haskeline
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import System.Directory
import System.Process
import System.Environment
import Control.Exception
import System.IO
import GHC.IO.Exception

data ShSt = ShSt { interactive :: Bool }

type ShM = StateT ShSt IO

main :: IO ()
main = do
  args <- getArgs
  let (beh, st) = case args of
        [] -> (defaultBehavior, ShSt True)
        (fnm:_) -> (useFile fnm, ShSt False)
  flip evalStateT st $ runInputTBehavior beh defaultSettings runShell

chomp :: String -> String
chomp = reverse . chomp' . reverse . chomp' where
  chomp' (' ':s) = chomp' s
  chomp' s = s

runShell :: InputT ShM ()
runShell = do
  interact <- fmap interactive $ lift get
  let prompt = if interact then "$ " else ""
  mln <- getInputLine prompt
  case fmap chomp mln of
    Just "exit" -> return ()
    Nothing -> return ()
    Just ln -> lift (goLine (words ln)) >> runShell

goLine, cd :: [String] -> ShM ()

goLine ("cd":rest) = cd rest
goLine s = liftIO $ exec s

cd [] = do
  home <- liftIO $ getHomeDirectory
  cd [home]
cd (dir:_) = do
  res <- liftIO $ try $ setCurrentDirectory dir
  case res of
    Right () -> return ()
    Left e -> do
      liftIO $ putErr $ dir ++ ": "++ioe_description (e :: IOException)
      return ()

exec :: [String] -> IO ()
exec (cmd:args) = do
  mfp <- findExecutable cmd
  case mfp of
    Nothing -> putErr $ cmd ++ ": command not found"
    Just fp -> do
      (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph) <- createProcess (proc fp args)
      _ <- waitForProcess ph
      return ()

putErr  = hPutStrLn stderr


{- TODO

run script
comments
redirect output, input, err
set variables
piping

-}
