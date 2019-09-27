{-# LANGUAGE RecordWildCards #-}
module Lib
    ( someFunc
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import System.Environment

keywords :: S.Set String
keywords = S.fromList
  [ "module"
  , "import"
  , "where"
  , "data"
  , "type"
  , "newtype"
  , "class"
  , "instance"
  , "case"
  , "if"
  , "then"
  , "else"
  , "qualified"
  , "deriving"
  ]

dropSingleLineComments :: [String] -> [String]
dropSingleLineComments [] = []
dropSingleLineComments (('-':'-':_):_) = []
dropSingleLineComments (w:ws) = w:(dropSingleLineComments ws)

takeUntil :: Eq a => [a] -> [a] -> [a]
takeUntil []       as = as
takeUntil sentinel as | sentinel `isPrefixOf` as = []
                      | otherwise = case as of
                          [] -> []
                          (x:xs) -> x : takeUntil sentinel xs

dropUntil :: Eq a => [a] -> [a] -> [a]
dropUntil []       as = as
dropUntil sentinel as | sentinel `isPrefixOf` as = drop (length sentinel) as
                      | otherwise = case as of
                          [] -> []
                          (_:xs) -> dropUntil sentinel xs

splitOn :: Eq a => [a] -> [a] -> ([a],[a])
splitOn []       as = (as,[])
splitOn sentinel as = (takeUntil sentinel as, dropUntil sentinel as)

dropBetween :: Eq a => [a] -> [a] -> Bool -> [[a]] -> (Bool, [[a]])
dropBetween begin end = go
  where go inComment [] = (inComment,[])
        go False (w:ws) | begin `isInfixOf` w =
                            let (pre,post) = splitOn begin w
                             in (pre:) <$> go True (post:ws)
                        | otherwise          = (w:) <$> go False ws
        go True (w:ws)  | end `isInfixOf` w =
                            let post = snd $ splitOn end w
                             in go False (post:ws)
                        | otherwise          = go True ws

dropBetweenWithEscape :: Eq a => [a] -> [a] -> [a] -> Bool -> [[a]] -> (Bool, [[a]])
dropBetweenWithEscape begin end esc = go
  where go inComment [] = (inComment,[])
        go False (w:ws) | (not ((esc ++ begin) `isInfixOf` w))
                          && begin `isInfixOf` w =
                            let (pre,post) = splitOn begin w
                             in (pre:) <$> go True (post:ws)
                        | otherwise = (w:) <$> go False ws
        go True (w:ws)  | (not ((esc ++ end) `isInfixOf` w))
                          && end `isInfixOf` w =
                            let post = snd $ splitOn end w
                             in go False (post:ws)
                        | otherwise = go True ws

dropMultiLineComments :: Bool -> [String] -> (Bool, [String])
dropMultiLineComments = dropBetween "{-" "-}"

dropSqlTH :: Bool -> [String] -> (Bool, [String])
dropSqlTH = dropBetween "[sql|" "|]"

dropStringLiterals :: [String] -> [String]
dropStringLiterals = snd . dropBetweenWithEscape "\"" "\"" "\\" False

dropComments :: Bool -> [String] -> (Bool, [String])
dropComments inComment = fmap dropSingleLineComments . dropMultiLineComments inComment

wrapQuotes :: String -> String
wrapQuotes = (++"\"") . ("\"" ++)

escape :: Char -> String
escape '\'' = "\\\'"
escape c = [c]

isValidOpeningChar :: Char -> Bool
isValidOpeningChar c |    (c >= 'a' && c <= 'z')
                       || (c == '_') = True
                     | otherwise = False

isValidChar :: Char -> Bool
isValidChar c |    isValidOpeningChar c
                || (c >= 'A' && c <='Z')
                || (c >= '0' && c <= '9')
                || (c == '\'') = True
              | otherwise = False

toMaybe :: Eq a => a -> a -> Maybe a
toMaybe a b | a == b = Nothing
            | otherwise = Just b

validateFunDef :: String -> Maybe String
validateFunDef word = fmap (concat . map escape) $ case span isValidChar word of
  ("",_) -> Nothing
  (vWord,[]) -> Just vWord
  (vWord, (x:_)) | (x == ':' || x == ' ') -> Just vWord
                 | otherwise -> Nothing

validateFunAp :: String -> Maybe String
validateFunAp = toMaybe "" . concat . map escape . filter isValidChar

data ParseContext = ParseContext
  { inComment    :: Bool
  , inSqlTH      :: Bool
  , currentFunc  :: String
  , defFuncs     :: S.Set String
  , funcMap      :: M.Map String (S.Set String)
  }

defaultParseContext :: ParseContext
defaultParseContext = ParseContext False False "" S.empty M.empty

someFunc :: IO ()
someFunc = do
  files <- getArgs
  ParseContext{defFuncs=funcs, funcMap=links} <- flip execStateT defaultParseContext $ do
    forM_ files $ \file -> do
      contents <- liftIO $ readFile file
      forM_ (lines contents) $ \line -> do
        let firstWord = listToMaybe $ words line
            possibleFunctionDefinition = maybe False isValidOpeningChar $ listToMaybe line
        pc1 <- get
        let (inComment', ws'') = dropComments (inComment pc1) $ words line
            (inSqlTH', ws) = if inComment'
                               then (inSqlTH pc1, ws'')
                               else let (sql,ws') = dropSqlTH (inSqlTH pc1) ws''
                                     in (sql, if sql then ws' else dropStringLiterals ws')
        put pc1{inComment = inComment', inSqlTH = inSqlTH'}
        if possibleFunctionDefinition
          then for_ (listToMaybe ws) $ \w -> case validateFunDef w of
            Just funcName | not (funcName `S.member` keywords) && (Just w == firstWord) -> do
              pc@ParseContext{..} <- get
              let funcCalls = tail ws
                  s = S.fromList . catMaybes . map validateFunAp $ funcCalls
                  funcMap' = case M.lookup currentFunc funcMap of
                    Nothing -> M.insert currentFunc s funcMap
                    Just s' -> M.insert currentFunc (S.union s' s) funcMap
              put pc{currentFunc = funcName, defFuncs = S.insert funcName defFuncs, funcMap = funcMap'}
            _ -> return ()
          else do
            pc@ParseContext{..} <- get
            let s = S.fromList . catMaybes . map validateFunAp $ ws
                funcMap' = case M.lookup currentFunc funcMap of
                  Nothing -> M.insert currentFunc s funcMap
                  Just s' -> M.insert currentFunc (S.union s' s) funcMap
            put pc{funcMap = funcMap'}
  let links' = M.map (S.intersection funcs) links
      header = "strict digraph deps {"
      footer = "}"
      defs = map ((++ " [style=solid];") . wrapQuotes) $ toList funcs
      maps = M.foldMapWithKey (\k a -> map (\y -> wrapQuotes k ++ " -> " ++ wrapQuotes y ++ ";") $ toList a) links'
  mapM_ putStrLn $ header : (defs ++ maps ++ [footer])

