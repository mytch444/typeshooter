module Main where
import Prelude
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Context
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent 
import Control.Exception
import Data.Int
import Foreign.Ptr
import Data.Bits
import System.Random
import Control.Exception
import System.IO
import System.Directory
import Text.Regex

type Location = (Double, Double)
type Word = (([Char], [Char], [Char]), Location)
type Bullet = (Word, Location, Location)
type Player = (Double, Location)

highscoresFileName = "/.typeshooter"
tmpFilePath = "/tmp/typeshooter"

wordList :: [String]
wordList = [
  "yo",
  "sup",
  "lol",
  "what",
  "the",
  "hell",
  "umm",
  "egg",
  "queen",
  "rats",
  "gun",
  "hello",
  "hi",
  "who",
  "when",
  "how",
  "why",
  "haskell",
  "c",
  "python",
  "break",
  "delay",
  "system",
  "random",
  "string",
  "words",
  "graphics",
  "x11",
  "xorg",
  "3281",
  "b3cnt",
  "37bec",
  "amvios"
  ]

fps :: Float
fps = 60

delay :: Int
delay = ceiling (1000 / fps) * 1000

bulletSpeed = -6
wordSpeed = 1

newWordChance :: Int -> Int
newWordChance s = 5000 `div` score
                  where score = if s < 100 then 100 else s

playerSide = 20

fontList :: [String]
fontList = ["-misc-ubuntu mono-medium-r-normal--0-0-0-0-m-0-iso8859-16",
            "-xos4-terminus-medium-r-normal--18-240-72-72-c-120-iso10646-1",
            "-misc-liberation mono-medium-r-normal--0-0-0-0-p-0-iso8859-16",
            "fixed"]

collideErrorMargin = 1

maxNameLength = 40

main :: IO ()
main = do
    dpy <- openDisplay ""

    let dflt = defaultScreen dpy
        black = 0

    rootw <- rootWindow dpy dflt
    win <- createSimpleWindow dpy rootw 0 0 500 700 1 black black
    mapWindow dpy win
    selectInput dpy win (exposureMask .|. keyPressMask .|. keyReleaseMask)

    gc <- createGC dpy win

    fontstruct <- getFont dpy 0
    setFont dpy gc (fontFromFontStruct fontstruct)

    showStartMenu dpy win gc fontstruct
    loop dpy win gc fontstruct (0.0, (0.0, 0.0)) [] [] (("", "", ""), (0, 0)) 0

    freeGC dpy gc
    freeFont dpy fontstruct

    exitWith ExitSuccess

getFont :: Display -> Int -> IO FontStruct
getFont dpy try = do
  fontstruct <- handle
                ((\_ -> (getFont dpy (try + 1))) :: IOException -> IO FontStruct)
                (loadQueryFont dpy (fontList !! try))
  return fontstruct

textHeight :: FontStruct -> String -> Double
textHeight font string = fromIntegral (ascent + descent)
                  where (_, ascent, descent, _) = textExtents font string

showStartMenu :: Display -> Window -> GC -> FontStruct -> IO ()
showStartMenu dpy win gc font = do
  (_, _, _, width, height, _, _) <- getGeometry dpy win

  setForeground dpy gc 0x000000
  fillRectangle dpy win gc 0 0 width height
  setForeground dpy gc 0xffffff
  
  drawString dpy win gc
    (fromIntegral width `div` 2 - fromIntegral (textWidth font "TYPESHOOTER") `div` 2)
    (fromIntegral height `div` 2 - ceiling (textHeight font "TYPESHOOTER")) "TYPESHOOTER"

  drawString dpy win gc
    (fromIntegral width `div` 2 - fromIntegral (textWidth font "Type out the words to shoot, don't let them hit you.") `div` 2)
    (fromIntegral height `div` 2) "Type out the words to shoot, don't let them hit you."

  drawString dpy win gc
    (fromIntegral width `div` 2 - fromIntegral (textWidth font "Press any key to play. Escape pauses, escape again to exit.") `div` 2)
    (fromIntegral height `div` 2 + ceiling (textHeight font "Press any key to play. Escape pauses, escape again to exit.")) "Press any key to play. Escape pauses, escape again to exit."

  allocaXEvent $ \e -> do
    nextEvent dpy e
    et <- get_EventType e
    if (et == keyPress)
      then return ()
      else showStartMenu dpy win gc font
           
pause :: Display -> Window -> GC -> FontStruct -> Player -> [Bullet] -> [Word] -> Word -> Int -> IO ()
pause dpy win gc font player bullets words word score = do
  (_, _, _, width, height, _, _) <- getGeometry dpy win

  drawString dpy win gc
    (fromIntegral width `div` 2 - fromIntegral (textWidth font "Space to resume.") `div` 2)
    (fromIntegral height `div` 2) "Space to resume."

  allocaXEvent $ \e -> do
    putStr "Pausing\n"
    nextEvent dpy e
    et <- get_EventType e
    if (et == keyPress)
      then do
      (_, _, _, _, _, _, _, mod, keycode, _) <- get_KeyEvent e
      keysym <- keycodeToKeysym dpy keycode 0
      let string = keysymToString keysym
      if (string == "Escape")
        then return ()
        else if (string == "space")
             then loop dpy win gc font player bullets words word score
             else pause dpy win gc font player bullets words word score
      else pause dpy win gc font player bullets words word score

gameOver :: Display -> Window -> GC -> FontStruct -> Dimension -> Dimension -> Int -> String -> IO ()
gameOver dpy win gc font width height score name = do
  setForeground dpy gc 0x000000
  fillRectangle dpy win gc bx by bw bh
  setForeground dpy gc 0xffffff
  drawRectangle dpy win gc bx by bw bh

  drawString dpy win gc
    (ceiling (x - fromIntegral (textWidth font "Game Over") / 2))
    (ceiling (y - (textHeight font "Game Over") * 2)) "Game Over"
  drawString dpy win gc
    (ceiling (x - fromIntegral (textWidth font scoretext) / 2))
    (ceiling (y - (textHeight font scoretext))) scoretext
  drawString dpy win gc
    (ceiling (x - fromIntegral (textWidth font "Enter Your Name:") / 2))
    (ceiling y) "Enter Your Name:"
  drawString dpy win gc
    (ceiling (x - fromIntegral (textWidth font name) / 2))
    (ceiling (y + textHeight font name)) name

  allocaXEvent $ \e -> do
    nextEvent dpy e
    et <- get_EventType e
    if (et == keyPress)
      then do
      (_, _, _, _, _, _, _, mod, keycode, _) <- get_KeyEvent e
      keysym <- keycodeToKeysym dpy keycode 0
      let keystring = keysymToString keysym
      putStrLn ("Got key: " ++ keystring)
      if (keystring == "Return")
        then
         do
           saveScore score name
           showHighscores dpy win gc font width height
        else gameOver dpy win gc font width height score (handleKeyGameOver name keystring)
      else gameOver dpy win gc font width height score name
  where scoretext = "You got a score of " ++ (show score)
        x = ((fromIntegral width) :: Double) / 2
        y = ((fromIntegral height) :: Double) / 2
        possiblew = textWidth font name + 50
        bw = fromIntegral (if (possiblew > textWidth font scoretext + 50)
                           then possiblew else textWidth font scoretext + 50) :: Dimension
        bh = ceiling ((textHeight font name) * 7) :: Dimension
        bx = ceiling (x - (fromIntegral bw) / 2)
        by = ceiling (y - (fromIntegral bh) / 2)

handleKeyGameOver :: String -> String -> String
handleKeyGameOver name keystring =
  if (keystring == "BackSpace")
  then allButLast name
  else if (length name > maxNameLength)
       then name
       else if (keystring == "space")
            then (name ++ " ")
            else if (length keystring == 1)
                 then (name ++ keystring)
                 else name
    
allButLast :: [a] -> [a]
allButLast [] = []
allButLast (f:[]) = []
allButLast (f:rest) = f : allButLast rest

saveScore :: Int -> String -> IO ()
saveScore score name = do
  home <- getHomeDirectory
  let highscoresFilePath = (home ++ highscoresFileName)
  inh <- openFile highscoresFilePath ReadMode
  outh <- openFile tmpFilePath WriteMode
  saveScoreReal score name inh outh
  hClose outh
  hClose inh

  tmp <- readFile tmpFilePath
  writeFile highscoresFilePath tmp

saveScoreReal :: Int -> String -> Handle -> Handle -> IO ()
saveScoreReal score name inh outh = do
  ineof <- hIsEOF inh
  if ineof
    then if name == "" then return () else writeScore
    else do
    input <- hGetLine inh
    let (cs, cn) = getNameScoreFromString input
    if cs < score
      then do
           writeScore
           hPutStrLn outh input
           saveScoreReal 0 "" inh outh
      else do
           hPutStrLn outh input
           saveScoreReal score name inh outh
  where writeScore = hPutStrLn outh (name ++ ":" ++ (show score))

getNameScoreFromString :: String -> (Int, String)
getNameScoreFromString input =
  (score, name)
  where regex = mkRegex ":"
        (name : scorestring : []) = splitRegex regex input
        score = read scorestring

showHighscores :: Display -> Window -> GC -> FontStruct -> Dimension -> Dimension -> IO ()        
showHighscores dpy win gc font width height = do
  setForeground dpy gc 0x000000
  fillRectangle dpy win gc bx by bw bh
  setForeground dpy gc 0xffffff
  drawRectangle dpy win gc bx by bw bh

  drawString dpy win gc
    (fromIntegral x - (textWidth font menuMessage) `div` 2)
    (by + ceiling (textHeight font menuMessage) * 2) menuMessage

  home <- getHomeDirectory
  let highscoresFilePath = (home ++ highscoresFileName)
  inh <- openFile highscoresFilePath ReadMode

  drawHighscoresFromFile dpy win gc font bw (fromIntegral by + bh) bx sy inh

  hClose inh

  allocaXEvent $ \e -> do
    nextEvent dpy e
    et <- get_EventType e
    if (et == keyPress)
      then return ()
      else showHighscores dpy win gc font width height
  where x = width `div` 2
        y = height `div` 2
        bw = fromIntegral (((fromIntegral (textWidth font "a")) :: Int) * (maxNameLength + 20)) :: Dimension
        bh = height - 100
        bx = fromIntegral (x - bw `div` 2) :: Position
        by = fromIntegral (y - bh `div` 2) :: Position
        menuMessage = "Highscores So Far..."
        sy = by + ceiling (textHeight font menuMessage) * 4

drawHighscoresFromFile :: Display -> Window -> GC -> FontStruct -> Dimension -> Dimension -> Position -> Position -> Handle -> IO ()
drawHighscoresFromFile dpy win gc font width bottom x y inh = do
  ineof <- hIsEOF inh
  if (ineof || y > fromIntegral bottom - 20)
     then return ()
    else do
    input <- hGetLine inh
    let (score, name) = getNameScoreFromString input
      
    drawString dpy win gc (x + 20) y name
    drawString dpy win gc
      (fromIntegral (x + fromIntegral width - 20 - (textWidth font (show score))) :: Position)
      y (show score)
    drawHighscoresFromFile dpy win gc font width bottom x (y + ceiling (textHeight font input)) inh
         
loop :: Display -> Window -> GC -> FontStruct -> Player -> [Bullet] -> [Word] -> Word -> Int -> IO ()
loop dpy win gc font (pa, (px, py)) bullets words word score = do
  (_, _, _, width, height, _, _) <- getGeometry dpy win
  let player = (pa, ((fromIntegral width :: Double) / 2, (fromIntegral height :: Double) - 30))

  setForeground dpy gc 0x000000
  fillRectangle dpy win gc 0 0 width height
  drawBullets dpy win gc bullets
  drawWords dpy win gc font words
  drawPlayer dpy win gc player
  drawScore dpy win gc font width height score

  threadDelay delay  

  rand <- newStdGen

  p <- pending dpy

  if (wordsPlayerCollide font words player)
    then gameOver dpy win gc font width height score ""
    else if (p == 0)
         then loop dpy win gc font player
              (updateBullets font player bullets)
              (updateWords font height player bullets (possiblyAddWord rand width height score words))
              word score
         else handleEvent dpy win gc font width height rand player bullets words word score

handleEvent :: Display -> Window -> GC -> FontStruct -> Dimension -> Dimension -> StdGen -> Player -> [Bullet] -> [Word] -> Word -> Int -> IO ()
handleEvent dpy win gc font width height rand player bullets words word score =
  allocaXEvent $ \e -> do
    nextEvent dpy e
    et <- get_EventType e
    if (et == keyPress)
      then do
      (_, _, _, _, _, _, _, mod, keycode, _) <- get_KeyEvent e
      keysym <- keycodeToKeysym dpy keycode 0
      putStr ("Key pressed " ++ (keysymToString keysym) ++ "\n")
      handleKeyPress dpy win gc font width height rand player bullets words word score keysym
      else loop dpy win gc font player
           (updateBullets font player bullets)
           (updateWords font height player bullets (possiblyAddWord rand width height score words))
           word score

handleKeyPress :: Display -> Window -> GC -> FontStruct -> Dimension -> Dimension -> StdGen -> Player -> [Bullet] -> [Word] -> Word -> Int -> KeySym -> IO ()
handleKeyPress dpy win gc font width height rand player bullets words word@((_, _, alive), _) score keysym =
  if (string == "Escape")
  then pause dpy win gc font player bullets words word score
  else if (not (newalive == "") && newalive !! 0 == char)
       then next (pointPlayerTowards newword player)
            (updateBullets font player ((createBullet font player updatednewword) : bullets))
            (updateWords font height player bullets (swapWordInList updatednewword newword words))
            (updateWord font player bullets updatednewword) (score + 1)
       else next player
           (updateBullets font player bullets)
           (updateWords font height player bullets (possiblyAddWord rand width height score words))
           word score
  where string = keysymToString keysym
        char = string !! 0
        newword@((total, newdead, newalive), newpos) =
          if (alive == "") then findWordWithChar words char else findWord word words
        updatednewword = moveLetterToDead font newword
        next = loop dpy win gc font
  
drawBullets :: Display -> Window -> GC -> [Bullet] -> IO ()
drawBullets _ _ _ [] = return ()
drawBullets dpy win gc ((word, (x, y), (x1, y1)):bullets) = do
  setForeground dpy gc 0x00aaff
  drawLine dpy win gc (ceiling x) (ceiling y) (ceiling x1) (ceiling y1)
  drawBullets dpy win gc bullets

drawWords :: Display -> Window -> GC -> FontStruct -> [Word] -> IO ()
drawWords _ _ _ _ [] = return ()
drawWords dpy win gc font (((total, dead, alive), (x, y)):words) = do
  setForeground dpy gc 0x00ff00
  drawString dpy win gc (ceiling (x - fromIntegral (textWidth font dead))) (ceiling y) (dead ++ alive)
  drawWords dpy win gc font words

drawPlayer :: Display -> Window -> GC -> Player -> IO ()
drawPlayer dpy win gc (angle, (ox, oy)) = do
  setForeground dpy gc 0x00ff00
  drawLine dpy win gc x y x1 y1
  drawLine dpy win gc x y x2 y2
  drawLine dpy win gc x1 y1 x2 y2
  where x = ceiling ox
        y = ceiling oy
        la = pi / 6 - angle
        x1 = ceiling (ox - playerSide * sin la)
        y1 = ceiling (oy + playerSide * cos la)
        ra = -pi / 6 - angle
        x2 = ceiling (ox - playerSide * sin ra)
        y2 = ceiling (oy + playerSide * cos ra)

drawScore :: Display -> Window -> GC -> FontStruct -> Dimension -> Dimension -> Int -> IO ()
drawScore dpy win gc font width height score = do
  setForeground dpy gc 0xffffff
  drawString dpy win gc x y text
  where text = show score
        x = (fromIntegral width) - 20 - textWidth font text
        y = 20
        
pointPlayerTowards :: Word -> Player -> Player
pointPlayerTowards (_, (wx, wy)) (oa, (px, py)) = (a, (px, py))
  where a = atan ((wx - px) / (wy - py))

updateBullets :: FontStruct -> Player -> [Bullet] -> [Bullet]
updateBullets _ _ [] = []
updateBullets font player@(_, playerpos) (b@(word@(text@(_, d, _), (wx, wy)), _, (x2, y2)) : bullets) =
  if (collide wp ww wh (x2, y2))
  then updateBullets font player bullets
  else (
    (updateWord font player [] word,
     (x2, y2),
     moveBulletTo font (text, wp) (x2, y2))
    : updateBullets font player bullets)
  where targetChar = if d == "" then "" else [head d]
        ww = fromIntegral (textWidth font targetChar)
        wh = textHeight font targetChar
        wp = (wx - fromIntegral (textWidth font targetChar), wy)

updateWords :: FontStruct -> Dimension -> Player -> [Bullet] -> [Word] -> [Word]
updateWords _ _ _ _ [] = []
updateWords font height player bullets (word@(((_, dead, alive)), (_, y)):words) =
  if (dead ++ alive == "" || y > fromIntegral height)
  then updateWords font height player bullets words
  else ((updateWord font player bullets word) : (updateWords font height player bullets words))

updateWord :: FontStruct -> Player -> [Bullet] -> Word -> Word
updateWord font player bullets (((total, dead, alive)), pos@(x, y)) =
  ((total, dying, alive),
   moveWordTo font player (dead ++ alive) pos)
  where collides = bulletsWordCollide font bullets
                   ((total, dead, alive), (x - fromIntegral (textWidth font dead), y))
        dying = if collides then tail dead else dead

swapWordInList :: Word -> Word -> [Word] -> [Word]
swapWordInList _ _ [] = []
swapWordInList n o (word : words) =
  if (o == word)
  then n : words
  else word : (swapWordInList n o words)

moveLetterToDead :: FontStruct -> Word -> Word
moveLetterToDead font word@((total, dead, alive), (x, y)) =
  ((total, dead ++ [head alive], tail alive),
   (x + fromIntegral (textWidth font [head alive]), y))

createBullet :: FontStruct -> Player -> Word -> Bullet
createBullet font (_, player) word =
  (word, player, moveBulletTo font word player)

findWordWithChar :: [Word] -> Char -> Word
findWordWithChar [] _  = (("", "", ""), (0, 0))
findWordWithChar (word@((_, _, alive), _) : words) char =
  if (alive == "" || not (alive !! 0 == char))
  then (findWordWithChar words char)
  else word

findWord :: Word -> [Word] -> Word
findWord _ [] = (("", "", ""), (0, 0))
findWord w@((t, _, _), _) (word@((total, _, _), _) : words) =
  if (t == total)
  then word
  else findWord w words

moveTo :: Location -> Double -> Double -> Location -> Double -> Location
moveTo (gx, gy) gw gh (sx, sy) speed = (sx + speed * sin o, sy + speed * cos o)
  where o = atan (xd / yd)
        xd = (gx + gw / 2) - sx
        yd = gy - sy

moveWordTo :: FontStruct -> Player -> String -> Location -> Location
moveWordTo font (_, (px, py)) text pos =
  moveTo (nx, py) playerSide playerSide pos wordSpeed
  where textW = fromIntegral (textWidth font text) :: Double
        nx = px - textW / 2

moveBulletTo :: FontStruct -> Word -> Location -> Location
moveBulletTo font ((_, dead, _), wp) pos =
  moveTo wp width height pos bulletSpeed
  where width = fromIntegral(textWidth font [head dead])
        height = textHeight font [head dead]

collide ::Location -> Double -> Double -> Location -> Bool
collide (ax, ay) w h (bx, by) =
  ax - collideErrorMargin <= bx &&
  ax + collideErrorMargin + w >= bx &&
  ay - collideErrorMargin <= by &&
  ay + collideErrorMargin + h >= by

bulletsWordCollide :: FontStruct -> [Bullet] -> Word -> Bool
bulletsWordCollide _ [] _ = False
bulletsWordCollide font (b@(((total, d, alive), (wx, wy)), _, bp):bullets) word@((t, _, _), _) =
  if (total == t && collide wp ww wh bp)
  then True
  else bulletsWordCollide font bullets word
  where targetChar = if d == "" then "" else [head d]
        ww = fromIntegral (textWidth font targetChar)
        wh = textHeight font targetChar
        wp = (wx - ww, wy)

wordsPlayerCollide :: FontStruct -> [Word] -> Player -> Bool
wordsPlayerCollide _ [] _ = False
wordsPlayerCollide font (((_, dead, alive), wp) : words) p@(_, pp) =
  if (collide wp ww wh pp)
  then True
  else wordsPlayerCollide font words p
  where ww = fromIntegral (textWidth font (dead ++ alive)) :: Double
        wh = textHeight font (dead ++ alive)
  
possiblyAddWord :: StdGen -> Dimension -> Dimension -> Int -> [Word] -> [Word]
possiblyAddWord rand width height score words =
  if (words == [] || randTrue rand (newWordChance score))
  then if (string == "")
       then words
       else (newWord rand width height string) : words
  else words
  where string = pickStringForWord rand words

newWord :: StdGen -> Dimension -> Dimension -> String -> Word
newWord rand width height string = ((string, "", string), (x, y))
  where x = fromIntegral (randNum rand (fromIntegral width)) :: Double
        y = -20.0

canUseWord :: String -> [Word] -> Bool
canUseWord _ [] = False
canUseWord string (((total, _, _), _) : words) =
  if ((total !! 0) == (string !! 0))
  then True
  else canUseWord string words

pickStringForWord :: StdGen -> [Word] -> String
pickStringForWord rand words =
  if (canUseWord string words)
  then ""
  else string
  where string = wordList !! (randNum rand ((length wordList) - 1))

randNum :: StdGen -> Int -> Int
randNum rand max = num
  where (num, _) = randomR (0, max) rand

randTrue :: StdGen -> Int -> Bool
randTrue rand chance = (num == 0)
  where (num, _) = randomR (0, chance) rand
