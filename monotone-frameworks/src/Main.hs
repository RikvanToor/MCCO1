module Main where

import           AttributeGrammar
import           Data.Graph
import           Data.Set (Set)
import           Lexer
import           Parser
import           System.Directory
import           System.IO
import           Test
import qualified Data.Set as S

main :: IO ()
main =
  do
    hSetBuffering stdin LineBuffering

    fs <- listDirectory "../examples"

    putStrLn $ "Spatie-gescheiden lijst van programmas waarvan je de tests zou "
             ++ "willen zien.\n\n* voor alle programmas\n:"
             ++ "ls voor een lijst\n"
             ++ "prefix met een - voor het vermijden van programmas\n"
             ++ "Noot: test_CP termineert niet op slv_while.c\n"

    putStr "> "

    inp <- getLine

    tf <-
      if inp == "ls" then do
        ls inp

        putStr "> "
        return (files (S.fromList fs) . words <$> getLine)
      else do
        return (files (S.fromList fs) . words <$> getLine)

    testfiles <- tf

    putStrLn $ "\nWelke K wil je hanteren? Voor embellished MF\n"
    putStr "> "

    k <- readLn :: IO Int

    case testfiles of
      [] -> return ()
      xs -> runAllTestFiles k (Just xs)

ls :: String -> IO ()
ls str
  | str == "ls" = listDirectory "../examples" >>= putStrLn . unlines
  | otherwise   = return ()

files :: Set FilePath -> [String] -> [FilePath]
files fs xs
  | elem "*" xs = S.toList fs
  | otherwise   =
    let
      rem  = S.fromList $ fmap (\(s:str) -> if s == '-' then str else "") xs
      pos  = let ys = S.fromList (fmap (\(s:str) -> if s == '-' then str else s:str) xs)
             in if S.null (ys S.\\ rem) -- Als er geen positieven in de input sitten
                then fs S.\\ rem        -- dan is de hele directory positief
                else ys S.\\ rem        -- anders
    in S.toList pos
