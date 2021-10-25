import Control.Concurrent (threadDelay)

main :: IO ()
main =
  do
    clr
    putStrLn "Hello and welcome to Hencke's Hanoi Simulator 3000"
    putStr "Starting the game"
    countdown 3
    gameLoop [] 0

type Board = [[Int]]

gameLoop :: Board -> Int -> IO ()
gameLoop [] nm = do
  clr
  cmd <- promptLine "Start a new game with: b <nbOfRings> or quit with: q"
  case words cmd of
    ["b", n] -> do
      let newState = initialState (read n)
      gameLoop newState nm
    _ -> do
      putStrLn "Yo, input a proper command dog" --Fix leaving game
      putStr "Returning"
      countdown 3
      gameLoop [] nm
gameLoop board nm = do
  clr
  -- Draw board from board
  drawTowers board
  drawMoves nm
  -- write out moves
  cmd <- getLine
  case words cmd of
    ["b", n] -> do
      let newState = initialState (read n)
      gameLoop newState nm
    ["z", n] -> do
      putStrLn ("Aborting that move by: " ++ n ++ " moves dog")
      gameLoop board nm
    ["help"] -> help board nm
    ["h"] -> do
      putStrLn "Yet to be implemented, probably wont be either"
      gameLoop board nm
    ["q"] -> return ()
    [f, t] -> do
      putStrLn ("Making a move from " ++ f ++ " to " ++ t)
      gameLoop board (nm + 1)
    _ -> do
      putStrLn "Yo, input a proper command dog" --Fix leaving game
      putStr "Returning"
      countdown 3
      gameLoop board nm

---------------Hjelpemetoder---------------

move board 1 2 = "Valid move"
move board 1 3 = "Valid move"
move board 2 3 = "Valid move"
move board 2 1 = "Valid move"
move board 3 1 = "Valid move"
move board 3 2 = "Valid move"
move _ _ _ = "Your move is invalid"

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

help :: Board -> Int -> IO ()
help board nm = do
  putStrLn "\nThere are 4 commands:"
  putStrLn "b <number of rings>: This commands starts a new game with a given number of rings"
  putStrLn "q: This command quits the game, losing all state"
  putStrLn "<f> <t>: This command moves a ring from pole f to pole t, if the move is legal"
  putStrLn "z <n>: Regrets n moves\n"
  _ <- promptLine "Enter any key to return to the game"
  gameLoop board nm

initialState :: (Num a, Enum a) => a -> [[a]]
initialState x = [[0 .. x], [], []]

drawTowers :: Board -> IO ()
drawTowers [t1, t2, t3] = do
  clr
  let mh = maximum (t1 ++ t2 ++ t3)
  helper 0 (reverse t1) mh
  helper (4 + 2 * maximum t1) (reverse t2) mh
  helper (8 + 2 * maximum (t1 ++ t2)) (reverse t3) mh
  goto 0 (mh + 2) --return to lines under pyramids
  where
    helper i = writeRowsList (i + 1)
drawTowers _ = return ()

writeRowsList :: Int -> [Int] -> Int -> IO ()
writeRowsList i [] mh
  | mh == 0 = return ()
  | otherwise = do
    threadDelay 100000
    writeBars i mh --Write "|"
    writeRowsList i [] (mh - 1)
writeRowsList i n mh
  | mh == 0 = return ()
  | otherwise = do
    writeRow i (head n) mh --Write "#"
    writeRowsList (i + 1) (tail n) (mh - 1)

drawMoves :: Int -> IO ()
drawMoves nm = putStrLn ("Number of moves: " ++ show nm)

countdown x =
  if x > 0
    then do
      putStr "."
      threadDelay 800000
      countdown (x - 1)
    else return ()