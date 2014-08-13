module Main where
import Prelude hiding (catch)
import Graphics.X11.Xlib hiding (Angle)
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Misc (drawLine, drawLines)
import Graphics.X11.Xlib.Context
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent 
import Control.Exception
import System.Posix.Types (Fd(..))
import Data.Int
import Foreign.Ptr
import Data.Bits
import System.Random

type Location = (Double, Double)
type Word = (([Char], [Char], [Char]), Location)
type Bullet = (Word, Location, Location)
type Player = (Double, Location)

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
  "xorg"
  ]

nwords :: Int
nwords = length wordList

fps :: Float
fps = 60

delay :: Int
delay = ceiling (1000 / fps) * 1000

bulletSpeed = -6
wordSpeed = 1

newWordChance = 1000

playerSide = 20

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

    fontstruct <- loadQueryFont dpy "fixed"
    setFont dpy gc (fontFromFontStruct fontstruct)

    loop dpy win gc fontstruct (0.0, (0.0, 0.0)) [] [] (("", "", ""), (0, 0))

    freeGC dpy gc
    freeFont dpy fontstruct

    exitWith ExitSuccess

textHeight :: FontStruct -> String -> Double
textHeight font string = fromIntegral (ascent + descent)
                  where (_, ascent, descent, _) = textExtents font string

pause :: Display -> Window -> GC -> FontStruct -> Player -> [Bullet] -> [Word] -> Word -> IO ()
pause dpy win gc font player bullets words word = do
  allocaXEvent $ \e -> do
    putStr "Pausing\n"
    nextEvent dpy e
    et <- get_EventType e
    if (et == keyPress)
      then do
      (_, _, _, _, _, _, _, mod, keycode, _) <- get_KeyEvent e
      keysym <- keycodeToKeysym dpy keycode 0
      if ((keysymToString keysym) == "Escape")
        then return ()
        else loop dpy win gc font player bullets words word
      else pause dpy win gc font player bullets words word

loop :: Display -> Window -> GC -> FontStruct -> Player -> [Bullet] -> [Word] -> Word -> IO ()
loop dpy win gc font (pa, (px, py)) bullets words word = do
--  putStr ("In loop\n")
  (_, _, _, width, height, _, _) <- getGeometry dpy win
  let player = (pa, ((fromIntegral width :: Double) / 2, (fromIntegral height :: Double) - 30))

  setForeground dpy gc 0x000000
  fillRectangle dpy win gc 0 0 width height
  drawBullets dpy win gc bullets
--  putStr ("Words\n")
  drawWords dpy win gc font words
--  putStr ("Done words\n")
  drawPlayer dpy win gc player

  threadDelay delay  

  rand <- newStdGen

  p <- pending dpy

  if (p == 0)
    then loop dpy win gc font player
         (updateBullets font player bullets)
         (updateWords font height player bullets (possiblyAddWord rand width height words))
         word
    else
    handleEvent dpy win gc font width height rand player bullets words word

handleEvent :: Display -> Window -> GC -> FontStruct -> Dimension -> Dimension -> StdGen -> Player -> [Bullet] -> [Word] -> Word -> IO ()
handleEvent dpy win gc font width height rand player bullets words word =
  allocaXEvent $ \e -> do
    putStr ("Waiting for event\n")
    nextEvent dpy e
--    putStr ("Got event\n")
    et <- get_EventType e
    if (et == keyPress)
      then do
      (_, _, _, _, _, _, _, mod, keycode, _) <- get_KeyEvent e
      keysym <- keycodeToKeysym dpy keycode 0
      putStr ("Keypress\n")
      handleKeyPress dpy win gc font width height rand player bullets words word keysym
      else loop dpy win gc font player
           (updateBullets font player bullets)
           (updateWords font height player bullets (possiblyAddWord rand width height words))
           word

handleKeyPress :: Display -> Window -> GC -> FontStruct -> Dimension -> Dimension -> StdGen -> Player -> [Bullet] -> [Word] -> Word -> KeySym -> IO ()
handleKeyPress dpy win gc font width height rand player bullets words word@((_, _, alive), _) keysym =
  if (string == "Escape")
  then pause dpy win gc font player bullets words word
  else if (not (newalive == "") && newalive !! 0 == char)
       then next (pointPlayerTowards newword player)
            (updateBullets font player ((createBullet font player updatednewword) : bullets))
            (updateWords font height player bullets (swapWordInList updatednewword newword words))
            (updateWord font player bullets updatednewword)
       else next player
           (updateBullets font player bullets)
           (updateWords font height player bullets (possiblyAddWord rand width height words)) word
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
drawWords dpy win gc font (((_, dead, alive), (x, y)):words) = do
--  putStr ("Drawing " ++ total ++ "\n")
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

pointPlayerTowards :: Word -> Player -> Player
pointPlayerTowards (_, (wx, wy)) (oa, (px, py)) = (a, (px, py))
  where a = atan ((wx - px) / (wy - py))

updateBullets :: FontStruct -> Player -> [Bullet] -> [Bullet]
updateBullets _ _ [] = []
updateBullets font player@(_, playerpos) (b@(word@(text@(_, d, _), (wx, wy)), _, (x2, y2)) : bullets) =
  if (bulletWordCollide font b wp ww wh)
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
   moveWordTo font player pos)
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

moveWordTo :: FontStruct -> Player -> Location -> Location
moveWordTo font (_, player) pos =
  moveTo player playerSide playerSide pos wordSpeed

moveBulletTo :: FontStruct -> Word -> Location -> Location
moveBulletTo font ((_, dead, _), wp) pos =
  moveTo wp width height pos bulletSpeed
  where width = fromIntegral(textWidth font [head dead])
        height = textHeight font [head dead]

bulletWordCollide :: FontStruct -> Bullet -> Location -> Double -> Double -> Bool
bulletWordCollide font (_, (bx, by), _) (wx, wy) w h =
  wx - 2.5 <= bx && wx + 2.5 + w >= bx && wy >= by

bulletsWordCollide :: FontStruct -> [Bullet] -> Word -> Bool
bulletsWordCollide _ [] _ = False
bulletsWordCollide font (b@(((total, d, alive), (wx, wy)), _, _):bullets) word@((t, _, _), _) =
  if (total == t && bulletWordCollide font b wp ww wh)
  then True
  else bulletsWordCollide font bullets word
  where targetChar = if d == "" then "" else [head d]
        ww = fromIntegral (textWidth font targetChar)
        wh = textHeight font targetChar
        wp = (wx - fromIntegral (textWidth font targetChar), wy)

possiblyAddWord :: StdGen -> Dimension -> Dimension -> [Word] -> [Word]
possiblyAddWord rand width height words =
  if (words == [] || randTrue rand newWordChance)
  then (newWord rand width height words) : words
  else words

newWord :: StdGen -> Dimension -> Dimension -> [Word] -> Word
newWord rand width height words = ((total, "", total), (x, y))
  where total = pickStringForWord rand words
        x = fromIntegral (randNum rand (fromIntegral width)) :: Double
        y = 20.0

canUseWord :: String -> [Word] -> Bool
canUseWord _ [] = False
canUseWord string (((total, _, _), _) : words) =
  if ((total !! 0) == (string !! 0))
  then True
  else canUseWord string words

pickStringForWord :: StdGen -> [Word] -> String
pickStringForWord rand words =
  if (canUseWord string words)
  then pickStringForWord rand words
  else string
  where string = wordList !! (randNum rand (nwords - 1))

randNum :: StdGen -> Int -> Int
randNum rand max = num
  where (num, _) = randomR (0, max) rand

randTrue :: StdGen -> Int -> Bool
randTrue rand chance = (num == 0)
  where (num, _) = randomR (0, chance) rand
