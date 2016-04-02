module Main where

import System.Console.Haskeline
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import System.Directory
import System.Process
import System.Environment
import Control.Exception  hiding (handle)
import System.IO
import GHC.IO.Exception
import Data.List

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
  interact <- haveTerminalUI
  let prompt = if interact then "$ " else ""
  mln <- handle (\Interrupt -> return $ Just [] ) $ withInterrupt $ getInputLine prompt
  case fmap processLn mln of
    Just ("exit":_) -> return ()
    Just [] -> runShell
    Nothing -> return ()
    Just ln -> lift (goLine ln) >> runShell

processLn :: String -> [String]
processLn = words . takeWhile (/='#') . chomp

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

redirect output, input, err
set variables
piping

-}
