module Dec19
  ( solve,
    countArrangements,
  )
where

import Control.Monad.State.Strict
import Data.List (stripPrefix)
import Data.Map (Map, insert, (!?))
import qualified Data.Map as Map (lookup)
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.Parsec hiding (State)

solve :: String -> (String, String)
solve input = (a, b)
  where
    a = either show (show . uncurry (count' possible)) . parse' $ input
    b = either show (show . uncurry (count' ways)) . parse' $ input

    possible (Just n) | n > 0 = 1
    possible _ = 0

    ways (Just n) = n
    ways Nothing = 0

count' :: (Maybe Int -> Int) -> [String] -> [String] -> Int
count' f towels patterns = sum $ fmap (f . (`Map.lookup` counts)) patterns
  where
    counts = countAll towels patterns

type Cache = Map String Int

countAll :: [String] -> [String] -> Map String Int
countAll towels patterns = execState (mapM (countArrangements towels) patterns) mempty

countArrangements :: [String] -> String -> State Cache Int
countArrangements _ "" = return 1
countArrangements towels pattern =
  let matchTowel = flip stripPrefix pattern
      remainders = mapMaybe matchTowel towels
   in do
        cache <- get
        case cache !? pattern of
          Just c -> return c
          Nothing -> do
            c <- sum <$> mapM (countArrangements towels) remainders
            modify (insert pattern c)
            return c

parse' :: String -> Either ParseError ([String], [String])
parse' = flip parse "" $ do
  towels <- sepBy1 (many1 letter) (string ", ") <* newline
  _ <- newline

  patterns <- sepEndBy1 (many1 letter) newline
  return (towels, patterns)
