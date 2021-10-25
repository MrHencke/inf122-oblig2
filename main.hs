import Control.Concurrent

main :: IO ()
main =
  do
    putStrLn "Hello and welcome to Hencke's Hanoi Simulator 3000"
    putStrLn "Start a new game with: b <nbOfRings> or quit with: q"
    putStr "Starting the game"
    countdown 3
    gameLoop

gameLoop :: IO ()
gameLoop = do
  clr
  -- Draw board
  -- write out moves
  cmd <- getLine
  case words cmd of
    ["b", n] -> putStrLn "You're gaming now brother"
    ["z", n] -> putStrLn "Aborting that move dog"
    ["help"] -> help
    ["h"] -> putStrLn "Yet to be implemented, probably wont be either"
    ["q"] -> return ()
    [f, t] -> putStrLn ("Making a move from " ++ f ++ " to " ++ t)
    _ -> do
      putStrLn "Yo, input a proper command dog" --Fix leaving game
      putStr "Returning"
      countdown 3
      gameLoop

---------------Hjelpemetoder---------------

writeRows :: Int -> Int -> Int -> IO ()
writeRows i n mh
  | mh == 0 = return ()
  | n == 0 =
    do
      threadDelay 100000
      writeBars i mh --Write "|"
      writeRows i n (mh - 1)
  | otherwise =
    do
      writeRow i n mh --Write "#"
      writeRows (i + 1) (n - 1) (mh - 1)

writeRow :: Int -> Int -> Int -> IO ()
writeRow i n mh = do
  goto i mh
  putStrLn (concat (replicate n " #"))

writeBars :: Int -> Int -> IO ()
writeBars i mh = do
  goto i mh
  putStrLn "|"

clr :: IO ()
clr = do
  putStr "\ESC[2J"
  goto 0 0

goto :: Int -> Int -> IO ()
goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

stdErrMsg :: String
stdErrMsg = "You done goofed"

trekanter :: Int -> Int -> Int -> IO ()
trekanter t1 t2 t3 =
  if min t1 (min t2 t3) < 0
    then putStrLn "Input valid numbers"
    else do
      clr
      let mh = max t1 (max t2 t3)
      helper 0 t1 mh
      helper (4 + 2 * t1) t2 mh
      helper (8 + 2 * (t1 + t2)) t3 mh
      goto 0 (mh + 2) --return to lines under pyramids
  where
    helper i = writeRows (i + 1)

promptLine :: String -> IO String
promptLine prompt = do
  putStrLn prompt
  getLine

help :: IO ()
help = do
  putStrLn "\nThere are 4 commands:"
  putStrLn "b <number of rings>: This commands starts a new game with a given number of rings"
  putStrLn "q: This command quits the game, losing all state"
  putStrLn "<f> <t>: This command moves a ring from pole f to pole t, if the move is legal"
  putStrLn "z <n>: Regrets n moves\n"
  _ <- promptLine "Press any key to return to the game"
  gameLoop

initialState :: (Num a, Enum a) => a -> [[a]]
initialState x = [[0 .. x], [], []]

drawBoard board = 1

drawTowers :: [Int] -> [Int] -> [Int] -> IO ()
drawTowers t1 t2 t3 = do
  clr

-- let mh = maximum (t1 ++ t2 ++ t3)
--  helper 0 t1 mh
--  helper (4 + 2 * t1) t2 mh
--  helper (8 + 2 * (t1 + t2)) t3 mh
--  goto 0 (mh + 2) --return to lines under pyramids
--where
--  helper i = writeRowsList (i + 1)

writeRowsList i n mh
  | mh == 0 = return ()
  | n == 0 =
    do
      threadDelay 100000
      writeBars i mh --Write "|"
      writeRowsList i n (mh - 1)
  | otherwise =
    do
      writeRow i n mh --Write "#"
      writeRowsList (i + 1) (n - 1) (mh - 1)

countdown x =
  if x > 0
    then do
      putStr "."
      threadDelay 800000
      countdown (x - 1)
    else return ()