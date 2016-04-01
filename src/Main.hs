module Main where

import System.Console.Haskeline
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)

data ShSt = ShSt

initShSt = ShSt

type ShM a = InputT (StateT ShSt IO) a

main :: IO ()
main = flip evalStateT initShSt $ runInputT defaultSettings runShell

chomp :: String -> String
chomp = reverse . chomp' . reverse . chomp' where
  chomp' (' ':s) = chomp' s
  chomp' s = s

runShell :: ShM ()
runShell = do
  mln <- getInputLine "$ "
  case fmap chomp mln of
    Just "exit" -> return ()
    Nothing -> return ()
    Just ln -> goLine (words ln) >> runShell

goLine :: [String] -> ShM ()
goLine s = liftIO $ putStrLn $ "you wrote: "++ unwords s
