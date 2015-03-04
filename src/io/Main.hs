{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-} -- (use -XOptionName on commandLine)
module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Bits (xor)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Word

import Parser

type Byte = GHC.Word.Word8

-- Exercise 1 -----------------------------------------

zipMapBS :: (Byte -> Byte -> Byte)
            -> ByteString -> ByteString -> ByteString
zipMapBS f a b = BS.pack $ map (uncurry f) $ BS.zip a b

xorBS :: ByteString -> ByteString -> ByteString
xorBS a b = zipMapBS xor a b

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret fileA fileB = do
  a <- BS.readFile fileA
  b <- BS.readFile fileB
  return $ BS.filter ((/=) 0) $ xorBS a b

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key decryptedFileName = do
  xs <- BS.readFile $ decryptedFileName ++ ".enc"
  BS.writeFile decryptedFileName $ xorBS (BS.cycle key) xs

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile jsonFileName =
  (BS.readFile jsonFileName) >>= (return . decode)

-- Exercise 4 -----------------------------------------

selectTs :: [Transaction] -> [TId] -> [Transaction]
selectTs ts tIds = filter (\t -> Set.member (tid t) (Set.fromList tIds)) ts

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsFile transactionsFile = do
  tIds' <- parseFile victimsFile
  ts' <- parseFile transactionsFile
  -- XXX Likely there's better way to combine these results
  case tIds' of
    Nothing -> return Nothing
    Just tIds ->
      case ts' of
        Nothing -> return Nothing
        Just ts ->
          return $ Just $ selectTs ts tIds

-- Exercise 5 -----------------------------------------

updateSum :: Num a => a -> Maybe a -> Maybe a
updateSum d Nothing = Just d
updateSum d (Just prev) = Just (prev + d)

updateFlow :: Map String Integer -> Transaction -> Map String Integer
updateFlow flow t = let d = amount t
                    in Map.alter (updateSum d) (to t)
                       $ Map.alter (updateSum (-d)) (from t) flow

getFlow :: [Transaction] -> Map String Integer
getFlow ts = foldl updateFlow Map.empty ts

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
-- XXX Actually everyone who has gained money should be a suspect.
-- XXX If there are only zero sums, or several aacounts that gained same max amount of money then who should we blame?
-- XXX Should we ensure that amount in transaction is non-negative?
-- (\(x, y) -> (y, x)) could also be (uncurry $ flip (,))
-- XXX This would fail on empty transaction list:
getCriminal flow = snd $ maximum $ map (\(x, y) -> (y, x)) $ Map.toList flow

-- Exercise 7 -----------------------------------------

reverseTx :: Transaction -> Transaction
-- XXX Probably it should be better to swap source and destination
reverseTx t = t {amount=(-(amount t)), tid="should-be-generated"}

------ undoTs :: Map String Integer -> [TId] -> [Transaction]
-- undoTs ts tIds = undefined

-- Note not the algorithm given in the assighment (it seemed too contrived,
-- just reversing transactions would introduce least confusion).
undoTs :: String -> Map String Integer -> [TId] -> [Transaction]
undoTs adminAcc flow newTIds = concatMap (uncurry resolveTx) $ Map.toList flow
  where
    resolveTx accId d
      | d == 0 = []
      | d < 0 = [Transaction {
                        from=adminAcc,
                        to=accId,
                        amount=abs d,
                        tid="TODO Generate 222"}]
      | otherwise = [Transaction {
                        from=accId,
                        to=adminAcc,
                        amount=d,
                        tid="TODO Generate 222"}]


-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON jsonFileName x = BS.writeFile jsonFileName (encode x)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
              adminAcc = "Bank admin, the problem resolver"
          writeJSON out (undoTs adminAcc flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
