import Control.Concurrent (threadDelay)

-- TODO  Stolpene skal hele tiden stå i ro på skjermen og kommandoer skrives alltid samme sted på skjermen.
type Board = [[Int]]

type State = [Board]

main :: IO ()
main =
  do
    clr
    putStrLn "Hello and welcome to Hencke's Hanoi Simulator 3000"
    putStrLn "To list all available commands, use the: \"help\" command"
    putStr "Starting the game"
    countdown 5
    gameLoop [] [] 0

gameLoop :: Board -> State -> Int -> IO ()
gameLoop [] state nm = do
  clr
  cmd <- promptLine "Start a new game with: b <nbOfRings> or quit with: q"
  case words cmd of
    ["b", n] -> do
      let num = read n :: Int
      let newBoard = initialBoard num
      putStrLn ("A game with " ++ n ++ " rings will at the very least take " ++ show (2 ^ num - 1) ++ " moves.")
      putStr "Press any key to continue..." >> getLine
      gameLoop newBoard (newBoard : state) nm
    _ -> do
      putStrLn "You have to initialize the game to input other commands"
      putStrLn "Start a new game with: b <nbOfRings>"
      putStr "Returning"
      countdown 3
      gameLoop [] state nm
gameLoop [[], [], xs] _ nm = do
  clr
  putStrLn "You won, congratulations!"
  putStrLn ("This game contained " ++ show (last xs) ++ " rings")
  putStrLn ("You used " ++ show nm ++ " moves")
  let time = 10
  putStrLn ("Bringing you back to the main menu in " ++ show time ++ " seconds")
  countdown time
  gameLoop [] [] 0
gameLoop board state nm = do
  clr
  drawTowers board
  drawMoves nm
  cmd <- getLine
  case words cmd of
    ["b", n] -> do
      let newBoard = initialBoard (read n)
      gameLoop newBoard (newBoard : state) nm
    ["z", n] ->
      if read n > nm
        then do
          putStrLn "You cant go back further than your moves"
          putStr "Returning"
          countdown 3
          gameLoop board state nm
        else do
          putStrLn ("Aborting that move by: " ++ n ++ " moves dog")
          let num = read n
          countdown 3
          gameLoop (state !! num) (drop num state) (nm - num)
    ["help"] -> help board state nm
    ["h"] -> do
      putStrLn "Yet to be implemented, dont hold your breath"
      countdown 3
      gameLoop board state nm
    ["q"] -> return ()
    ["board"] -> do
      print board
      countdown 3
      gameLoop board state nm
    ["state"] -> do
      print state
      countdown 3
      gameLoop board state nm
    [f, t] ->
      if legalMove board (read f) (read t)
        then do
          let newBoard = move board (read f) (read t)
          gameLoop newBoard (newBoard : state) (nm + 1)
        else do
          putStrLn "That move is illegal"
          putStr "Returning"
          countdown 3
          gameLoop board state nm
    _ -> do
      putStrLn "Yo, input a proper command dog"
      putStr "Returning"
      countdown 3
      gameLoop board state nm

---------------Hjelpemetoder---------------

move :: Board -> Int -> Int -> Board
move [a : as, b, c] 1 2 = [as, a : b, c]
move [a : as, b, c] 1 3 = [as, b, a : c]
move [a, b : bs, c] 2 3 = [a, bs, b : c]
move [a, b : bs, c] 2 1 = [b : a, bs, c]
move [a, b, c : cs] 3 1 = [c : a, b, cs]
move [a, b, c : cs] 3 2 = [a, c : b, cs]
move board _ _ = board

writeRow :: Int -> Int -> Int -> IO ()
writeRow i n mh = do
  goto i mh
  putStrLn (concat (replicate n " #"))

writeBars :: Int -> Int -> IO ()
writeBars i mh = do
  goto i mh
  putStrLn "|"

help :: Board -> State -> Int -> IO ()
help board state nm = do
  putStrLn "\nThere are 4 commands:"
  putStrLn "b <number of rings>: This commands starts a new game with a given number of rings"
  putStrLn "q: This command quits the game, losing all state"
  putStrLn "<f> <t>: This command moves a ring from pole f to pole t, if the move is legal"
  putStrLn "z <n>: Regrets n moves\n"
  _ <- promptLine "Enter any key to return to the game"
  gameLoop board state nm

initialBoard :: (Num a, Enum a) => a -> [[a]]
initialBoard x = [[1 .. x], [], []]

drawTowers :: Board -> IO ()
drawTowers [t1, t2, t3] = do
  clr
  let mh = maximum (t1 ++ t2 ++ t3)
  helper (mh `div` 2) (reverse t1) mh 0
  helper (4 * mh) (reverse t2) mh 0
  helper (6 * mh) (reverse t3) mh 0
  goto 0 (mh + 2)
  where
    helper i = writeRowsList (i + 1)
drawTowers _ = return ()

writeRowsList :: Int -> [Int] -> Int -> Int -> IO ()
writeRowsList _ _ 0 _ = return ()
writeRowsList i [] mh prev = do
  threadDelay 30000
  writeBars (i + (2 * prev `div` 2) - 1) mh --Write "|"
  writeRowsList i [] (mh - 1) prev
writeRowsList i n mh _ = do
  writeRow i (head n) mh --Write "#"
  writeRowsList (i + 1) (tail n) (mh - 1) (head n)

drawMoves :: Int -> IO ()
drawMoves nm = putStrLn ("Number of moves: " ++ show nm)

countdown :: Int -> IO ()
countdown x =
  if x > 0
    then do
      putStr "."
      threadDelay 800000
      countdown (x - 1)
    else return ()

legalMove :: Board -> Int -> Int -> Bool
legalMove board x y
  | null (board !! (x - 1)) = False
  | null (board !! (y - 1)) = legalTowers x y
  | otherwise = legalTowers x y && head (board !! (x - 1)) < head (board !! (y - 1))

legalTowers :: Int -> Int -> Bool
legalTowers x y = x /= y && legalTower x && legalTower y

legalTower :: Int -> Bool
legalTower x = 0 < x && x < 4

------------- Utility functions -------------

clr :: IO ()
clr = do
  putStr "\ESC[2J"
  goto 0 0

goto :: Int -> Int -> IO ()
goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

promptLine :: String -> IO String
promptLine prompt = do
  putStrLn prompt
  getLine

--hanoiSolver board = True