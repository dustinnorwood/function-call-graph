{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}  -- To derive Show
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where

import Options.Generic
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.State ( execStateT, get, put )
import Control.Monad ( forM_ )
import Data.Foldable ( Foldable(toList), for_ )
import Data.List ( isInfixOf, isPrefixOf )
import Data.Tuple (swap)
import qualified Data.Map.Strict as M
import Data.Maybe ( catMaybes, listToMaybe )
import qualified Data.Set as S
import System.Environment ( getArgs )

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

type FuncName = String
type FuncFile = String
type FuncId = ( FuncName
              , FuncFile
              )
type ColorName = String

data ParseContext = ParseContext
  { inComment    :: Bool
  , inSqlTH      :: Bool
  , currentFunc  :: FuncId
  , defFuncs     :: S.Set FuncId
  , funcMap      :: M.Map FuncId (S.Set FuncName)
  , fileMap      :: M.Map FuncFile ColorName
  }



defaultParseContext :: ParseContext
defaultParseContext = ParseContext False False ("","-") S.empty M.empty M.empty

data Opts w = Opts { clusters :: w ::: Bool <!> "False"
                   , file :: w ::: NoLabel [String] <?> "filename..."
                   }
  deriving (Generic)
instance ParseRecord (Opts Wrapped)
deriving instance Show (Opts Unwrapped)

-- technique for getting varargs argv https://github.com/Gabriel439/Haskell-Optparse-Generic-Library/issues/65
newtype NoLabel a = NoLabel a  deriving (Generic, Show)
instance ParseFields a => ParseRecord (NoLabel a)
instance ParseFields a => ParseFields (NoLabel a) where
  parseFields msg _ _ def = fmap NoLabel (parseFields msg Nothing Nothing def)

someFunc :: Opts Unwrapped -> IO ()
someFunc opts = do
  let getNoLabel (NoLabel x) = x
      files = getNoLabel $ file opts
  ParseContext{defFuncs=funcs, funcMap=links, fileMap=filemap} <- flip execStateT defaultParseContext $ do
    forM_ files $ \file -> do
      contents <- liftIO $ readFile file

      -- assign a color to the file
      pc0 <- get
      put pc0{fileMap = M.insertWith (flip const) file (show $ M.size (fileMap pc0) + 1) $ fileMap pc0}

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
                  funcId = (funcName,file) -- [TODO] should this be (currentFunc,file)?
                  funcMap' = M.insertWith S.union funcId s funcMap
              put pc{currentFunc = funcId, defFuncs = S.insert funcId defFuncs, funcMap = funcMap'}
            _ -> return ()
          else do
            pc@ParseContext{..} <- get
            let s = S.fromList . catMaybes . map validateFunAp $ ws
                funcMap' = M.insertWith S.union currentFunc s funcMap
            put pc{funcMap = funcMap'}
  let links' = M.map (S.intersection (S.map fst funcs)) links
      header = "strict digraph deps {\n  node [colorscheme=set312, style=filled];\n"
      footer = "}"
      file2func = M.fromListWith (<>) (fmap (:[]) . swap <$> toList funcs)
      defs =
        [ commentCluster ("subgraph cluster_" ++ M.findWithDefault "0" file filemap ++ " {\n") ++
          commentCluster ("  label = " ++ wrapQuotes file ++ ";\n") ++
          unlines [ "      " ++ wrapQuotes funcname ++ " [color=" ++ M.findWithDefault "white" file filemap ++ "];"
                  | funcname <- file2func M.! file
                  ] ++
          commentCluster "}\n"
        | file <- M.keys file2func
        ]
      maps = M.foldMapWithKey (\(k,_) a -> map (\y -> wrapQuotes k ++ " -> " ++ wrapQuotes y ++ ";") $ toList a) links'
  mapM_ putStrLn $ header : (defs ++ maps ++ [footer])

  where
    commentCluster x =
      if clusters opts
      then "   "   ++ x
      else "  // " ++ x
