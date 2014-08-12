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
  "fucker",
  "cunt",
  "yo",
  "sup",
  "brother",
  "lol",
  "what",
  "the",
  "hell",
  "umm",
  "porn",
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

newWordChance = 500

playerSide = 20

main :: IO ()
main = do
    dpy <- openDisplay ""

    let dflt = defaultScreen dpy
        black = 0
        words = []

    rootw <- rootWindow dpy dflt
    win <- createSimpleWindow dpy rootw 0 0 500 700 1 black black
    mapWindow dpy win
    selectInput dpy win (exposureMask .|. keyPressMask .|. keyReleaseMask)

    gc <- createGC dpy win

    fontstruct <- loadQueryFont dpy "fixed"
    setFont dpy gc (fontFromFontStruct fontstruct)

    loop dpy win gc fontstruct (0.0, (0.0, 0.0)) [] words (("", "", ""), (0, 0))

    freeGC dpy gc
    freeFont dpy fontstruct

    exitWith ExitSuccess

textheight :: FontStruct -> String -> Double
textheight font string = fromIntegral (ascent + descent)
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
  (_, _, _, width, height, _, _) <- getGeometry dpy win
  let player = (pa, ((fromIntegral width :: Double) / 2, (fromIntegral height :: Double) - 30))

  setForeground dpy gc 0x000000
  fillRectangle dpy win gc 0 0 width height
  drawBullets dpy win gc bullets
  drawWords dpy win gc font words
  drawPlayer dpy win gc player

  threadDelay delay  

  rand <- newStdGen

  p <- pending dpy

  if (p == 0)
    then loop dpy win gc font player
         (updateBullets font player bullets)
         (updateWords font height player bullets (possiblyAddWord rand width height words))
         word
    else handleEvent dpy win gc font width height rand player bullets words word

handleEvent :: Display -> Window -> GC -> FontStruct -> Dimension -> Dimension -> StdGen -> Player -> [Bullet] -> [Word] -> Word -> IO ()
handleEvent dpy win gc font width height rand player bullets words word =
  allocaXEvent $ \e -> do
    nextEvent dpy e
    et <- get_EventType e
    if (et == keyPress)
      then do
      (_, _, _, _, _, _, _, mod, keycode, _) <- get_KeyEvent e
      keysym <- keycodeToKeysym dpy keycode 0
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
            (updateBullets font player ((createBullet font player newword) : bullets))
            (updateWords font height player bullets (swapWordInList updatednewword words))
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

pointPlayerTowards :: Word -> Player -> Player
pointPlayerTowards (_, (wx, wy)) (oa, (px, py)) = (a, (px, py))
  where a = atan ((wx - px) / (wy - py))

updateBullets :: FontStruct -> Player -> [Bullet] -> [Bullet]
updateBullets _ _ [] = []
updateBullets font player@(_, playerpos) (b@(word@(text, w), _, (x2, y2)) : bullets) =
  if (bulletWordCollide font b word)
  then updateBullets font player bullets
  else (
    ((text, moveTo font playerpos w wordSpeed),
     (x2, y2),
     moveTo font w (x2, y2) bulletSpeed)
    : updateBullets font player bullets)

updateWords :: FontStruct -> Dimension -> Player -> [Bullet] -> [Word] -> [Word]
updateWords _ _ _ _ [] = []
updateWords font height player bullets (word@(((_, dead, alive)), (_, y)):words) =
  if (dead ++ alive == "")
  then updateWords font height player bullets words
  else ((updateWord font player bullets word) : (updateWords font height player bullets words))

updateWord :: FontStruct -> Player -> [Bullet] -> Word -> Word
updateWord font player@(_, playerpos) bullets word@(((total, dead, alive)), pos@(x, y)) =
    ((total, dying, alive),
     moveTo font playerpos pos wordSpeed)
  where collides = bulletsWordCollide font bullets
                   ((total, "", ""), (x - fromIntegral (textWidth font dead), y))
        dying = if collides then tail dead else dead

swapWordInList :: Word -> [Word] -> [Word]
swapWordInList _ [] = []
swapWordInList w@((t, _, _), _) (word@((total, _, _), _) : words) =
  if (t == total)
  then w : words
  else word : (swapWordInList w words)

moveLetterToDead :: FontStruct -> Word -> Word
moveLetterToDead font word@((total, dead, alive), (x, y)) =
  ((total, dead ++ [head alive], tail alive),
   (x + fromIntegral (textWidth font [head alive]), y))

createBullet :: FontStruct -> Player -> Word -> Bullet
createBullet font (_, player) ((total, dead, alive), pos) =
  (((total, "", ""), pos), player, moveTo font pos player 5)

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

moveTo :: FontStruct -> Location -> Location -> Double -> Location
moveTo font (gx, gy) (sx, sy) speed = (sx + speed * sin o, sy + speed * cos o)
  where o = atan (xd / yd)
        xd = (gx + fromIntegral (textWidth font "a") / 2) - sx
        yd = (gy + fromIntegral (textWidth font "a") / 2) - sy

bulletWordCollide :: FontStruct -> Bullet -> Word -> Bool
bulletWordCollide font (_, _, (bx, by)) ((word, _, _), (wx, wy)) =
  wx - 5 <= bx && wx + fromIntegral (textWidth font word) + 5 >= bx &&
  wy - 5 <= by && wy + (textheight font word) + 5 >= by

bulletsWordCollide :: FontStruct -> [Bullet] -> Word -> Bool
bulletsWordCollide _ [] _ = False
bulletsWordCollide font (b@(((total, _, _), _), _, _):bullets) word@((t, _, _), _) =
  if (total == t && bulletWordCollide font b word) then True
  else bulletsWordCollide font bullets word

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
