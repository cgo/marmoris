{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Marmoris.Game
       (
         Game
       , GameState(..)
       , field
       , stoneTextures
       , setStone
       , setPlayer1
       , setPlayer2
       , movePlayer
       , movePlayer_
       , switchPlayer
       , StoneMotion(..)
       , startGame
       , contGame
       , contGame_
       , module Marmoris.Field
       , module Marmoris.Rendering
       ) where

import Control.Monad (when)
import Control.Monad.State
import Data.Array ((//), (!))
import Data.IORef
import Data.List (find)
import Marmoris.Image
import Marmoris.Field
import Marmoris.Rendering


newtype Game a = Game { runGame :: StateT GameState IO a }

data GameState = GameState { gameField :: Field
                           , gameCurrentPlayer :: IORef Int
                           , gameStoneTextures :: StoneTextures }

instance Monad Game where
  (Game a) >>= f = Game (a >>= runGame . f)
  return = Game . return

instance MonadIO Game where
  liftIO = Game . liftIO

instance MonadState GameState Game where
  get = Game get
  put = Game . put

instance Functor Game where
  fmap f (Game a) = Game $ fmap f a


field :: Game Field
field = fmap gameField get


fieldBounds :: Game ((Int,Int),(Int,Int))
fieldBounds = fmap fieldData field >>= liftIO . getBounds


positionInsideField :: Position -> Game Bool
positionInsideField (x,y) = fmap (flip inRange (x,y)) fieldBounds


stoneTextures :: Game StoneTextures
stoneTextures = fmap gameStoneTextures get


setStone :: StoneType -> Position -> Game ()
setStone s (x,y) = field >>= \a -> liftIO $ writeArray (fieldData a) (x,y) $ toStoneID s


getStone :: Position -> Game StoneType
getStone (x,y) = fmap fieldData field >>= \a -> fmap fromStoneID $ liftIO $ readArray a (x,y)


currentPlayer :: Game Int
currentPlayer = gets gameCurrentPlayer >>= liftIO . readIORef


setCurrentPlayer :: Int -> Game ()
setCurrentPlayer x | 0 <= x && x <= 1 = gets gameCurrentPlayer >>= liftIO . flip writeIORef x
                   | otherwise = error "setCurrentPlayer: invalid player."


setPlayer1 :: Position -> Game ()
setPlayer1 (x,y) = field >>= \f@(Field _ _ pos) ->
  liftIO (writeArray pos 0 (x,y))
  -- modify (\s -> s { gameField = f { playerPositions = pos//[(0,(x,y))] } })


setPlayer2 :: Position -> Game ()
setPlayer2 (x,y) = field >>= \f@(Field _ _ pos) ->
  liftIO (writeArray pos 1 (x,y))
  -- modify (\s -> s { gameField = f { playerPositions = pos//[(1,(x,y))] } })


playerPosition :: Int -> Game Position
playerPosition player | player < 0 || player > 1 = error "playerPosition: invalid player number."
                      | otherwise = fmap playerPositions field >>= liftIO . flip readArray player
                                    

data StoneMotion = MoveUp | MoveDown | MoveLeft | MoveRight

positionAdd :: Position -> StoneMotion -> Position
positionAdd (x,y) MoveUp = (x,y+1)
positionAdd (x,y) MoveDown = (x,y-1)
positionAdd (x,y) MoveLeft = (x-1,y)
positionAdd (x,y) MoveRight = (x+1,y)


-- | Move a player in a given direction, if that is possible. If it is not
--   possible, nothing happens.
--   Triggers all necessary actions if the move requires it.
movePlayer :: Int -> StoneMotion -> Game ()
movePlayer player m = playerPosition player >>= f m
  where f m (x,y) = do
          mst <- getStone' (x,y) m
          maybe (return ()) (\st -> do
            case st of
              Regular -> f' player $ positionAdd (x,y) m
                where
                  f' 0 = setPlayer1
                  f' 1 = setPlayer2
              Acid -> liftIO $ print "Acid was pushed!"
              _ -> return ()
            ) mst 


movePlayer_ :: StoneMotion -> Game ()
movePlayer_ m = currentPlayer >>= \p -> movePlayer p m


-- | Get the stone at a given position plus the given motion.
--   If one of the players is at (p+m), then Just 'Player1' or Just 'Player2' is returned.
getStone' :: Position -- ^ The Position /p/
             -> StoneMotion -- ^ The motion /m/ from /p/
             -> Game (Maybe StoneType) -- ^ The stone at (positionAdd p m), if that's a valid position.
getStone' (x,y) m = do
  fb <- fieldBounds
  let p' = positionAdd (x,y) m
  if inRange fb p'
    then do
    st <- getStone p'
    pps <- field >>= liftIO . getElems . playerPositions
    return $ maybe (Just st) (Just . snd) $ find ((==p') . fst) $ zip pps [Player1, Player2]
    else return Nothing


-- | Switch the player between Heinz and Harry.
switchPlayer :: Game ()
switchPlayer = currentPlayer >>= setCurrentPlayer . f
  where f 0 = 1
        f 1 = 0


-- | Initialize a 'GameState' with an empty field, load all textures,
--   and define them.
initState :: IO GameState
initState =
  makeEmptyField >>= \f ->
  defineStoneTextures f >>= \st ->
  newIORef 0 >>= \cp ->
  return $ GameState { gameField = f
                     , gameCurrentPlayer = cp
                     , gameStoneTextures = st }


-- | Initialises everything and returns a new 'GameState' that can be used in subsequent
--   'contGame' and 'contGame_'. This is done so that 'Game' actions can be used
--   in IO callbacks needed by the GUI framework (such as GLUT).
--   It is not too pretty.
startGame :: Game a -> IO (a,GameState)
startGame (Game a) = initRendering >> initState >>= runStateT a


contGame :: GameState -> Game a -> IO (a,GameState)
contGame st (Game a) = runStateT a st


contGame_ :: GameState -> Game a -> IO GameState
contGame_ st a = fmap snd $ contGame st a