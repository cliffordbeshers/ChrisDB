{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.List ((\\))
import Data.Set as Set (fromList, member, size)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time (diffUTCTime, getCurrentTime, UTCTime)

default (T.Text)

big = "../tmp.fnd"
little = "../tmp.sdf"

extractKey' :: TL.Text -> T.Text
extractKey'  = T.strip . head . T.split (== ',') . TL.toStrict

extractKey :: TL.Text -> TL.Text
extractKey  = TL.strip . head . TL.split (== ',')

timestamp :: String -> StateT (UTCTime, UTCTime) IO ()
timestamp message = do
  (t0, ti) <- get
  tj <- liftIO getCurrentTime
  liftIO $ do
    (putStr . show) (("running time", diffUTCTime tj t0), ("delta time", diffUTCTime tj ti))
    putStr "    "
    putStrLn message
  put (t0, tj)

myStrict = TL.toStrict
-- myStrict = id

main = getCurrentTime >>= (\t -> return (t,t)) >>= evalStateT findMissingKey

findMissingKey = do
  timestamp "findMissingKey"
  !keys <- Set.fromList . fmap (myStrict . TL.strip) . tail . TL.lines <$> liftIO (TL.readFile little)
  -- liftIO $ print ("keys", Set.size keys)
  timestamp "key file loaded"
  liftIO $ print ("size keys", Set.size keys)
  timestamp "(size keys) printed"
  records <- (tail . TL.lines) <$> liftIO (TL.readFile big)
  liftIO $ mapM_ (printMissing keys) records
  timestamp "record file finished"
    where isMissing ks k = not (Set.member (myStrict k) ks)
          isMissing' ks k = not (Set.member k ks)
          printMissing !ks ln = if isMissing ks (extractKey ln) then TL.putStrLn ln else return ()
          printMissing' ks ln = if isMissing' ks (extractKey' ln) then TL.putStrLn ln else return ()

{-
  records <- (map extractKey . tail . T.lines) <$> T.readFile big
  let isMissing k = Nothing == Map.lookup k keys
  print (filter isMissing records)
-}


