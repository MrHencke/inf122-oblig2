import GHC.Unicode (isDigit)

type Board = [[Int]]

type State = [Board]

main :: IO ()
main = gameLoop [] [] 0 ""

gameLoop :: Board -> State -> Int -> String -> IO ()
gameLoop [] state nm msg = do
  clr
  titlecard
  putStrLn "To list all available commands, use the: \"help\" command"
  drawMessage msg
  putStrLn "Start a new game with: b <nbOfRings> or quit with: q"
  cmd <- promptLine ""
  case words cmd of
    ["b", n] -> newGame [] n state nm
    ["help"] -> do
      clr
      help
      gameLoop [] state nm ""
    ["q"] -> return ()
    _ -> gameLoop [] state nm "You have to initialize the game to input other commands"
gameLoop [[], [], xs] _ nm _ = do
  clr
  winMessage xs nm
  gameLoop [] [] 0 "You've won, but can you do it again?"
gameLoop board state nm msg = do
  clr
  drawTowers board
  drawMoves nm
  drawMessage msg
  cmd <- promptLine ""
  case words cmd of
    ["b", n] -> newGame board n state nm
    ["z", n] ->
      if checkDigit n
        then
          if read n > nm
            then gameLoop (state !! nm) [] 0 "You can't go back further than your moves \nTaking you back to the starting position"
            else do
              let num = read n
              gameLoop (state !! num) (drop num state) (nm - num) ("Regretting: " ++ n ++ " moves")
        else gameLoop board state nm "Please input a valid number"
    ["help"] -> do
      clr
      help
      gameLoop board state nm ""
    ["h"] -> gameLoop board state nm "Yet to be implemented, dont hold your breath"
    ["q"] -> return ()
    [f, t] ->
      if checkDigit f && checkDigit t
        then do
          if legalMove board (read f) (read t)
            then do
              let newBoard = move board (read f) (read t)
              gameLoop newBoard (newBoard : state) (nm + 1) ""
            else gameLoop board state nm "That move is illegal"
        else gameLoop board state nm "Please input a valid number"
    xs -> gameLoop board state nm ("\"" ++ unwords xs ++ "\"" ++ " is not a valid command, check available commands with \"help\"")

---------------Hjelpemetoder---------------

-- Cant really think of a dynamic way to do this without making spaghetti code
move :: Board -> Int -> Int -> Board
move [a : as, b, c] 1 2 = [as, a : b, c]
move [a : as, b, c] 1 3 = [as, b, a : c]
move [a, b : bs, c] 2 3 = [a, bs, b : c]
move [a, b : bs, c] 2 1 = [b : a, bs, c]
move [a, b, c : cs] 3 1 = [c : a, b, cs]
move [a, b, c : cs] 3 2 = [a, c : b, cs]
move board _ _ = board

help :: IO ()
help = do
  putStrLn "\nThere are 4 commands:"
  putStrLn "b <number of rings>: Starts a new game with a given number of rings"
  putStrLn "q: Quits the game, losing all state"
  putStrLn "<f> <t>: Moves a ring from pole f to pole t, if the move is legal"
  putStrLn "z <n>: Regrets n moves"
  putStrLn "h: This command is supposed to give you a hint\n"
  _ <- promptLine "Enter any key to return to the game"
  return ()

initializeBoard :: (Num a, Enum a) => a -> [[a]]
initializeBoard x = [[1 .. x], [], []]

drawTowers :: Board -> IO ()
drawTowers [t1, t2, t3] = do
  let mh = maximum (t1 ++ t2 ++ t3) + 1
  writeRows (2 + mh) (reverse t1) mh
  writeRows (3 * (2 + mh)) (reverse t2) mh
  writeRows (5 * (2 + mh)) (reverse t3) mh
  goto 0 (mh + 2)
drawTowers _ = return ()

writeRows :: Int -> [Int] -> Int -> IO ()
writeRows _ _ 0 = return ()
writeRows pivot [] mh = do
  writeAt "|" 1 pivot mh
  writeRows pivot [] (mh - 1)
writeRows pivot n mh = do
  let el = head n
  writeAt " #" el (pivot - el) mh --Write "#"
  writeRows pivot (tail n) (mh - 1)

writeAt :: String -> Int -> Int -> Int -> IO ()
writeAt str n pivot height = do
  goto pivot height
  putStrLn (concat (replicate n str))

drawMoves :: Int -> IO ()
drawMoves nm = putStrLn ("Number of moves: " ++ show nm)

drawMessage :: String -> IO ()
drawMessage msg = if msg == "" then putStr "\n" else putStr ("\ESC[31m" ++ msg ++ "\ESC[0m\n")

legalMove :: Board -> Int -> Int -> Bool
legalMove board x y =
  legalTowers x y
    && (board !! (x - 1) /= [])
    && (null (board !! (y - 1)) || (head (board !! (x - 1)) < head (board !! (y - 1))))

legalTowers :: Int -> Int -> Bool
legalTowers x y = x /= y && legalTower x && legalTower y

legalTower :: Int -> Bool
legalTower x = 0 < x && x < 4

newGame :: Board -> String -> State -> Int -> IO ()
newGame board n state nm =
  if checkDigit n
    then do
      let num = read n :: Int
      if num > 12
        then gameLoop board state nm "Right now, 12 is the maximum amount of rings playable."
        else do
          let newBoard = initializeBoard num
          gameLoop newBoard [newBoard] 0 ("A game with " ++ n ++ " rings will at the very least take " ++ show (2 ^ num - 1 :: Int) ++ " moves.")
    else gameLoop board state nm "Please input a valid number"

winMessage :: [Int] -> Int -> IO ()
winMessage xs nm = do
  clr
  putStrLn "You won, congratulations!"
  let rings = last xs
  let optimalSol = 2 ^ rings - 1 :: Int
  putStrLn ("This game contained " ++ show rings ++ " rings")
  let message =
        "You used " ++ show nm
          ++ if optimalSol == nm
            then " moves, just like the optimal solution. Good job!"
            else " moves, while the optimal solution uses " ++ show optimalSol ++ ". Better luck next time!"
  putStrLn message
  _ <- promptLine "Press enter to continue to the main menu"
  return ()

------------- Utility functions -------------

clr :: IO ()
clr = do
  putStr "\ESC[2J"
  goto 0 0

goto :: Int -> Int -> IO ()
goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

promptLine :: String -> IO String
promptLine [] = do
  putStr "> "
  getLine
promptLine prompt = do
  putStrLn prompt
  putStr "> "
  getLine

checkDigit :: String -> Bool
checkDigit = all isDigit

titlecard :: IO ()
titlecard =
  putStrLn
    " ████████╗ █████╗ ██╗       ██╗███████╗██████╗  ██████╗    █████╗ ███████╗  ██╗  ██╗ █████╗ ███╗  ██╗ █████╗ ██╗\n \
    \╚══██╔══╝██╔══██╗██║  ██╗  ██║██╔════╝██╔══██╗██╔════╝   ██╔══██╗██╔════╝  ██║  ██║██╔══██╗████╗ ██║██╔══██╗██║\n \
    \   ██║   ██║  ██║╚██╗████╗██╔╝█████╗  ██████╔╝╚█████╗    ██║  ██║█████╗    ███████║███████║██╔██╗██║██║  ██║██║\n \
    \   ██║   ██║  ██║ ████╔═████║ ██╔══╝  ██╔══██╗ ╚═══██╗   ██║  ██║██╔══╝    ██╔══██║██╔══██║██║╚████║██║  ██║██║\n \
    \   ██║   ╚█████╔╝ ╚██╔╝ ╚██╔╝ ███████╗██║  ██║██████╔╝   ╚█████╔╝██║       ██║  ██║██║  ██║██║ ╚███║╚█████╔╝██║\n \
    \   ╚═╝    ╚════╝   ╚═╝   ╚═╝  ╚══════╝╚═╝  ╚═╝╚═════╝     ╚════╝ ╚═╝       ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚══╝ ╚════╝ ╚═╝\n"

testHanoi :: Board
testHanoi = [[1, 2, 3], [], []]

n = 3

hS :: Board -> Int -> State -> State
hS board 0 akk = akk -- base case
hS board n akk
  | n `elem` c = if (n - 1) `elem` b then hS (move board 2 3) (n - 1) ((move board 2 3) : akk) else hS (move board 1 3) (n - 1) ((move board 1 3) : akk) --rek case 1
  | n `elem` b = if (n - 1) `elem` c then hS (move board 3 1) (n - 1) ((move board 3 1) : akk) else hS board n akk -- flytt (n-1) til a, flytt n til c, flytt (n-1) til c
  | n `elem` a = if (n - 1) `elem` c then hS (move board 3 2) (n - 1) ((move board 3 2) : akk) else hS board n akk -- flytt (n-1) til b, flytt n til c, flytt (n-1) til c
  where
    [a, b, c] = board

--hS testHanoi 3 []