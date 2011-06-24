#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Time
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import League
import Network.MessagePackRpc.Server
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Posix.Files
import System.Posix.Unistd
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import System.Process
import System.Random

resultFile::String
resultFile = "result.txt"

serverMutexFile = "yauj.mutex"

scoreBoard :: TVar (Vector (Vector Int))
scoreBoard = unsafePerformIO $ newTVarIO $ V.replicate aiSize $ V.replicate aiSize 0

matchCount :: TVar (Vector (Vector Int))
matchCount = unsafePerformIO $ newTVarIO $ V.replicate aiSize $ V.replicate aiSize 0

boardRemark :: TVar (Vector (Vector String))
boardRemark = unsafePerformIO $ newTVarIO $ V.replicate aiSize $ V.replicate aiSize ""

boardModified :: TVar Bool
boardModified = unsafePerformIO $ newTVarIO $ True

suggestMatch :: IO (String, String)
suggestMatch = do
  rand <- randomRIO (0,1)
  (i0, i1) <- atomically $ do
          mc <- readTVar matchCount
          case selectMatch rand mc of
            Just (match@(i0, i1)) -> do
                 writeTVar matchCount $  modify2 i0 i1 (+1) $ modify2 i1 i0 (+1) mc
                 return $ match
            Nothing -> do
              return (-1,-1)
  if i0<0 then return ("","") 
  else return (command $ ais!i0, command $ ais!i1)

reportMatch :: (String, String) -> String -> IO ()
reportMatch (cmd0, cmd1) result = do
  hPrint stderr $ "reportMatch:" ++ show mmatch
  recordMatch True mmatch
  where
    wr = words result
    turn' = read $ last wr
    score0'
          | wr !! 1 == "draw" = 1
          | wr!!2=="0" && wr!!3 == "wins"   = if turn' < 100000 then 6 else 2
          | wr!!2=="1" && wr!!3 == "loses"  = if turn' < 100000 then 6 else 2
          | otherwise         = 0
    killRatioStr = head $ filter (elem ':') wr ++ ["-1:-1"]
    [a0,a1] = map read $ words $
              map (\c -> if isDigit c || c=='-'then c else ' ') killRatioStr
    mmatch = if result == "" then Nothing else Just match
    match = Match {
              p0 = AI cmd0 ,
              p1 = AI cmd1 ,
              score0 = score0' ,
              alive0 = a0,
              alive1 = a1,
              turn = turn'
            }

-- results are string like this
-- !! player 0 wins by 256:0 after turn 22218
-- !! draw by 256:256 after turn 100000
-- !! player 1 loses by invalid output at turn 1

matchLimit :: Int
matchLimit = 1

selectMatch :: Double -> Vector (Vector Int) -> Maybe (Int, Int)
selectMatch ratio matchCount' = if weightSum <=0 then Nothing 
                                else Just (ret0, ret1)
    where
      maxMC = min matchLimit $ (+1) $ V.maximum $ V.concat $ V.toList matchCount'
      weighted = V.toList $ V.concat $ V.toList $ imap2 mkWeight matchCount'
      (ret0, ret1) = ret
      weightSum = sum $ map fst weighted
      charge = ratio * weightSum
      ret = extract charge $ cycle weighted

      extract c ((w,x):rest)
              | c - w < 0 = x
              | True   = extract (c-w) rest
      extract _ _ = undefined

      mkWeight ix iy mc = let w = if ix==iy then 0
                                  else ((fromIntegral $ maxMC  - mc)::Double)
                          in (w, (ix, iy))


recordMatchMutex :: TMVar Int
recordMatchMutex = unsafePerformIO $ newTMVarIO 0

maxMatchCount :: Int
maxMatchCount = (matchLimit * aiSize * (aiSize-1))
               

recordMatch :: Bool -> Maybe Match -> IO ()
recordMatch isNew mmatch = when valid $ do  
  if not (isJust mmatch) then atomically $ do
    bd <- readTVar boardRemark
    writeTVar boardRemark $ modify2 i0 i1 (const "fail") bd 
  else do
    atomically $ do
      bd <- readTVar scoreBoard
      writeTVar scoreBoard $ modify2 i0 i1 (+(100*s0+1)) bd 
    atomically $ do
      bd <- readTVar boardRemark
      writeTVar boardRemark $ modify2 i0 i1 (const remark) bd 
    when isNew rec
        where
        remark = "<br/>" ++ show (alive0 match) ++ ":" ++ show (alive1 match) ++
                 "<br/>" ++ show (turn match)
        valid = isJust i0m && isJust i1m
        i0 = fromJust i0m
        i1 = fromJust i1m
        match = fromJust mmatch
        i0m = aiIndex $ p0 match
        i1m = aiIndex $ p1 match
        s0 = score0 match
        rec = do
          count <- atomically $ takeTMVar recordMatchMutex
          h <- openFile resultFile AppendMode
          hPutStrLn h $ show match
          hClose h
          hPutStrLn stderr $ show (count+1) ++ " / " ++ show maxMatchCount
          atomically $ putTMVar recordMatchMutex (count+1)
          hFlush stderr
          atomically $ writeTVar boardModified True

printHoshitori :: IO Bool
printHoshitori = do
  (b, bd,mc,brm,rmc) <- atomically $ do
                   b <- readTVar boardModified   
                   bd <- readTVar scoreBoard
                   mc <- readTVar matchCount
                   brm <- readTVar boardRemark
                   rmc <- takeTMVar recordMatchMutex
                   writeTVar boardModified False
                   return (b, bd,mc,brm, rmc)
  atomically $ putTMVar recordMatchMutex rmc
  hPutStrLn stderr $ "need hoshitori ?" ++ show b
  if b == False then return False
  else do
    tz <- getCurrentTimeZone 
    ut <- getCurrentTime
    let
        tr i = command (ais!i) : show i : nakami i
    
        nakami i = [ nonDiago i j $ htmlTag "center" $ show (bd!j!i) ++ "/" ++  show (mc!j!i) ++ (brm!j!i)
                         | j <- [0..aiSize-1] ] ++ [ketsu i]
        ketsu i = htmlTag "center" $ show(numerator i) ++ "/" ++ show(denominator i)
    
        nonDiago i j = if i==j then const "" else id
    
        numerator   i =       sum [bd!j!i | j <- [0..aiSize-1] ]
        denominator i = 1+100*sum [mc!j!i | j <- [0..aiSize-1] ]
    
        strongness :: Int -> Double    
        strongness i = (fromIntegral $ numerator i) / (fromIntegral $denominator i)
    
        ranking = map snd $ reverse $ sort $ [((strongness i,-i), i)| i <- [0..aiSize-1]]
    
        rankingTbl = htmlTbl $ [ [command $ ais ! ri, ketsu ri] | ri <- ranking]
    
        headline :: [String]
        headline = "" : "" : map show [0..aiSize-1]
        tbl :: [[String]]
        tbl = headline : map tr [0..aiSize-1]
    
        htmlTag :: String -> String -> String
        htmlTag tag str = "<" ++ tag ++ ">" ++ str ++ "</" ++ tag ++ ">" 
    
        htmlTag' :: String -> String -> String -> String
        htmlTag' tag flag str = "<" ++ tag ++ " " ++ flag ++  ">" ++ str ++ "</" ++ tag ++ ">" 
                          
        htmlTd :: String -> String
        htmlTd str = htmlTag "td" str 
    
        htmlTr :: [String] -> String
        htmlTr strs =  htmlTag "tr" $ unwords $ map htmlTd strs
    
        htmlTbl :: [[String]] -> String
        htmlTbl tbl' = htmlTag' "table" "border=1 align=center" $ unlines $ map htmlTr tbl'
    
        localTimeStr = show $ utcToLocalTime tz ut
    
        banner1 = htmlTag "p" $ "Last Update : " ++ localTimeStr ++ "<br/>" ++
                  "<a href='http://www.paraiso-lang.org/Walpurgisnacht/'>Back to Toppage</a>"
        banner2 = htmlTag "center" $ htmlTag "h1" $ "ranking"
        banner3 = htmlTag "center" $ rankingTbl
        banner4 = "<br/>" ++ (htmlTag "center" $ htmlTag "h1" $ "league")
        banner = unlines [banner1, banner2, banner3, banner4]
    
        htmlHead = unlines ["<head>",
                            "<title>Incubating...</title>",
                            "</head>"]
    
    writeFile "scoreboard.html" $ htmlTag "html" $ (htmlHead ++) $ htmlTag "body" $ banner ++ htmlTbl tbl
    _ <- system "scp scoreboard.html paraiso-lang.org:/var/www/html/Walpurgisnacht/store/"
    return (rmc >= maxMatchCount)
  
hoshitoriPrinter :: ProcessID -> IO ()
hoshitoriPrinter pid = do
  sleep 10
  done <- printHoshitori
  if done then do
            -- League ga owatte shimatta to iunonara,
            -- minna shinu shika naijanai!!
            system $ "rm -fr " ++ serverMutexFile
            system $ "kill " ++ show pid -- anatamo!!
            exitSuccess -- watashimo !!
            return ()
  else hoshitoriPrinter pid

main :: IO ()
main = do
  system $ "touch " ++ serverMutexFile
  printHoshitori
  putStrLn "ready."
  pid <- getProcessID
  forkIO $ hoshitoriPrinter pid
  serve port [ ("suggestMatch", fun suggestMatch),
               ("reportMatch" , fun reportMatch )]


---- Vector Libraries ----
map2 :: (a -> b) -> Vector (Vector a) -> Vector (Vector b)
map2 = V.map . V.map 

imap2 :: (Int -> Int -> a -> b) -> Vector (Vector a) -> Vector (Vector b)
imap2 f = V.imap (\iy -> V.imap (\ix -> f ix iy)) 

foldl2 :: (a -> a -> a) -> Vector (Vector a) -> a
foldl2 f = V.foldl1 f . V.map (V.foldl1 f)

write2 :: Int -> Int -> a -> Vector (Vector a) -> Vector (Vector a) 
write2 ix iy val vv = vv//[(iy, (vv!iy)//[(ix, val)])]

modify2 :: Int -> Int -> (a->a) -> Vector (Vector a) -> Vector (Vector a) 
modify2 ix iy f vv = vv//[(iy, (vv!iy)//[(ix, f (vv ! iy ! ix))])]

print2 :: (Show a) => Vector (Vector a) -> IO ()
print2 vv = print $ map V.toList $ V.toList vv