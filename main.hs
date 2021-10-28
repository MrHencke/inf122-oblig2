import Control.Concurrent (threadDelay)
import GHC.Unicode (isDigit)

type Board = [[Int]]

type State = [Board]

main :: IO ()
main = startGame

gameLoop :: Board -> State -> Int -> IO ()
gameLoop [] state nm = do
  clr
  cmd <- promptLine "Start a new game with: b <nbOfRings> or quit with: q"
  case words cmd of
    ["b", n] -> newGame [] n state nm
    ["q"] -> return ()
    _ -> do
      putStrLn "You have to initialize the game to input other commands"
      putStrLn "Start a new game with: b <nbOfRings>"
      putStr "Returning"
      countdown 3
      gameLoop [] state nm
gameLoop [[], [], xs] _ nm = winState xs nm
gameLoop board state nm = do
  drawTowers board
  drawMoves nm
  putStr "\n> " --gives the user a more useful prompt
  cmd <- getLine
  case words cmd of
    ["b", n] -> newGame board n state nm
    ["z", n] ->
      if checkDigit n
        then
          if read n > nm
            then do
              putStrLn "You can't go back further than your moves"
              putStrLn "Taking you back to the starting position"
              putStr "Returning"
              countdown 3
              let startBoard = state !! nm
              gameLoop startBoard [startBoard] (nm - nm)
            else do
              putStrLn ("Regretting: " ++ n ++ " moves")
              let num = read n
              countdown 3
              gameLoop (state !! num) (drop num state) (nm - num)
        else numErr board state nm
    ["help"] -> do
      help
      gameLoop board state nm
    ["h"] -> do
      putStrLn "Yet to be implemented, dont hold your breath"
      countdown 3
      gameLoop board state nm
    ["q"] -> return ()
    [f, t] ->
      if checkDigit f && checkDigit t
        then do
          if legalMove board (read f) (read t)
            then do
              let newBoard = move board (read f) (read t)
              gameLoop newBoard (newBoard : state) (nm + 1)
            else do
              putStrLn "That move is illegal"
              putStr "Returning"
              countdown 3
              gameLoop board state nm
        else numErr board state nm
    _ -> do
      putStrLn "This is not a valid command"
      putStr "Returning to game"
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
writeRow i n height = do
  goto i height
  putStrLn (concat (replicate n " #"))

writeBars :: Int -> Int -> IO ()
writeBars pivot height = do
  goto pivot height
  putStrLn "|"

help :: IO ()
help = do
  putStrLn "\nThere are 4 commands:"
  putStrLn "b <number of rings>: This commands starts a new game with a given number of rings"
  putStrLn "q: This command quits the game, losing all state"
  putStrLn "<f> <t>: This command moves a ring from pole f to pole t, if the move is legal"
  putStrLn "z <n>: Regrets n moves\n"
  _ <- promptLine "Enter any key to return to the game"
  return ()

initializeBoard :: (Num a, Enum a) => a -> [[a]]
initializeBoard x = [[1 .. x], [], []]

drawTowers :: Board -> IO ()
drawTowers [t1, t2, t3] = do
  clr
  let mh = maximum (t1 ++ t2 ++ t3) + 1
  writeRows (2 + mh) (reverse t1) mh
  writeRows (3 * (2 + mh)) (reverse t2) mh
  writeRows (5 * (2 + mh)) (reverse t3) mh
  goto 0 (mh + 2)
drawTowers _ = return ()

writeRows :: Int -> [Int] -> Int -> IO ()
writeRows _ _ 0 = return ()
writeRows pivot [] mh = do
  threadDelay 20000
  writeBars pivot mh --Write "|"
  writeRows pivot [] (mh - 1)
writeRows pivot n mh = do
  threadDelay 20000
  let el = head n
  writeRow (pivot - el) el mh --Write "#"
  writeRows pivot (tail n) (mh - 1)

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

------------- Game loop functions that are used more than once, or are large -------------
startGame :: IO ()
startGame = do
  clr
  titlecard
  putStrLn "Hello and welcome to Hencke's Hanoi Simulator 3000"
  putStrLn "To list all available commands, use the: \"help\" command"
  putStr "Starting"
  countdown 7
  gameLoop [] [] 0

newGame :: Board -> String -> State -> Int -> IO ()
newGame board n state nm =
  if checkDigit n
    then do
      let num = read n :: Int
      let newBoard = initializeBoard num
      let optimalSol = 2 ^ num - 1 :: Int
      putStrLn ("A game with " ++ n ++ " rings will at the very least take " ++ show optimalSol ++ " moves.")
      putStr "Good luck"
      countdown 5
      gameLoop newBoard (newBoard : state) nm
    else numErr board state nm

numErr :: Board -> State -> Int -> IO ()
numErr board state nm = do
  putStrLn "Please input a valid number"
  putStr "Returning"
  countdown 3
  gameLoop board state nm

winState xs nm = do
  clr
  putStrLn "You won, congratulations!"
  let rings = last xs
  let optimalSol = 2 ^ rings - 1 :: Int
  putStrLn ("This game contained " ++ show rings ++ " rings")
  let message =
        if optimalSol == nm
          then "You used " ++ show nm ++ " moves, just like the optimal solution. Good job!"
          else "You used " ++ show nm ++ " moves, while the optimal solution uses " ++ show optimalSol ++ ". Better luck next time!"
  putStrLn message
  let time = 10
  putStrLn ("Bringing you back to the main menu in " ++ show time ++ " seconds")
  countdown time
  gameLoop [] [] 0

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
  putStr "> "
  getLine

checkDigit :: String -> Bool
checkDigit [] = False
checkDigit [x] = isDigit x
checkDigit (x : xs) = checkDigit [x] && checkDigit xs

titlecard :: IO ()
titlecard =
  do
    putStrLn "██╗░░██╗███████╗███╗░░██╗░█████╗░██╗░░██╗███████╗░██████╗   ██╗░░██╗░█████╗░███╗░░██╗░█████╗░██╗"
    putStrLn "██║░░██║██╔════╝████╗░██║██╔══██╗██║░██╔╝██╔════╝██╔════╝   ██║░░██║██╔══██╗████╗░██║██╔══██╗██║"
    putStrLn "███████║█████╗░░██╔██╗██║██║░░╚═╝█████═╝░█████╗░░╚█████╗░   ███████║███████║██╔██╗██║██║░░██║██║"
    putStrLn "██╔══██║██╔══╝░░██║╚████║██║░░██╗██╔═██╗░██╔══╝░░░╚═══██╗   ██╔══██║██╔══██║██║╚████║██║░░██║██║"
    putStrLn "██║░░██║███████╗██║░╚███║╚█████╔╝██║░╚██╗███████╗██████╔╝   ██║░░██║██║░░██║██║░╚███║╚█████╔╝██║"
    putStrLn "╚═╝░░╚═╝╚══════╝╚═╝░░╚══╝░╚════╝░╚═╝░░╚═╝╚══════╝╚═════╝░   ╚═╝░░╚═╝╚═╝░░╚═╝╚═╝░░╚══╝░╚════╝░╚═╝\n"

hanoiSolver :: p1 -> p2 -> Bool
hanoiSolver board akk = True