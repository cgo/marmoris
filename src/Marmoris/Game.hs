{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ExistentialQuantification #-}

module Marmoris.Game
       (
         Game
       , GameState(..)
       , fieldBounds
       , field
       , stoneTextures
       , setStone
       , setStoneById
       , currentGold
       , setPlayer1
       , setPlayer2
       , movePlayer
       , movePlayer_
       , switchPlayer
       , StoneMotion(..)
       , startGame
       , contGame
       , contGame_
       , Command(..)
       , Action(..)
       , Actions
       , dispatch
       , createCommandAddHandler
       , startNetwork
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
import Reactive.Banana
import Reactive.Banana.Frameworks

newtype Game a = Game { runGame :: StateT GameState IO a }

data GameState = GameState { gameField :: Field
                           , gameGold :: IORef Int
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


setStoneById :: StoneID -> Position -> Game ()
setStoneById i (x,y) = field >>= \a -> liftIO $ writeArray (fieldData a) (x,y) i


addGold :: Int -> Game ()
addGold n = gets gameGold >>= liftIO . flip modifyIORef (+n)


currentGold :: Game Int
currentGold = gets gameGold >>= liftIO . readIORef


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
                                    

data StoneMotion = MoveUp | MoveDown | MoveLeft | MoveRight deriving Show

positionAdd :: Position -> StoneMotion -> Position
positionAdd (x,y) MoveUp = (x,y+1)
positionAdd (x,y) MoveDown = (x,y-1)
positionAdd (x,y) MoveLeft = (x-1,y)
positionAdd (x,y) MoveRight = (x+1,y)


-- | Move a player in a given direction, if that is possible. If it is not
--   possible, nothing happens.
--   Triggers all necessary actions if the move requires it.
movePlayer :: Int -> StoneMotion -> Game Actions
movePlayer player m = playerPosition player >>= f m
  where f m (x,y) = do
          currentstone <- getStone (x,y)
          mst <- getStone' (x,y) m
          let p'   = positionAdd (x,y) m
              f' 0 = setPlayer1
              f' 1 = setPlayer2
              g    = f' player p' >> if (currentstone == Loose)
                                     then (setStone Empty (x,y)) >> return (LooseTriggered (x,y))
                                     else return NoAction
          maybe (return [NoAction]) (\st -> do
            case st of
              Regular         -> fmap return g
              Loose           -> fmap return g
              Gold            -> gold p' >>= \a -> g >>= \b -> return [a,b]
              Acid            -> acid p' m >>= maybe (return [NoAction]) (\a -> g >>= \b -> return [a,b])
              ReplacementTile -> replacement p' m >>= maybe (return [NoAction]) (\a -> g >>= \b -> return [a,b])
              _               -> return [NoAction]
            ) mst 


gold :: Position -> Game Action
gold (x,y) = setStone Regular (x,y) >> addGold 1 >> return (AddGold (x,y))


replacement :: Position -> StoneMotion -> Game (Maybe Action)
replacement p m = do
  mst <- getStone' p m
  let p' = positionAdd p m
  maybe (return Nothing)
    (\st -> case st of
        Regular -> setStone ReplacementTile p' >> setStone Regular p >> return (Just NoAction)
        Empty   -> setStone Regular p' >> setStone Regular p >> return (Just $ ReplaceEmpty p')
        _ -> return Nothing)
    mst

-- | The player pushed an acid stone that sits on the given position, in the direction
--   given in the second argument.
acid :: Position -> StoneMotion -> Game (Maybe Action)
acid p m = do
  mst <- getStone' p m
  let p' = positionAdd p m
  maybe
    (return Nothing)
    (\st -> case st of
        Regular -> setStone Acid p' >> setStone Regular p >> return (Just NoAction)
        Wall -> setStone Regular p' >> setStone Regular p >> return (Just (AcidDissolvesWall p'))
        _ -> return Nothing)
    mst
  


movePlayer_ :: StoneMotion -> Game Actions
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
  newIORef 0 >>= \g ->
  return $ GameState { gameField = f
                     , gameGold = g
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


--------------------------------------------------------------------------------
-- Network stuff for FRP

-- | The commands coming in from the user interface.
data Command = MovePlayer StoneMotion
             | SwitchPlayer
             | SomeOtherCommand deriving Show


createCommandAddHandler :: IO (AddHandler (GameState,Command), (GameState,Command) -> IO ())
createCommandAddHandler = newAddHandler


-- | The actions that go back to the user interface via a callback, see 'networkDesc' and 'startNetwork'.
data Action = AcidDissolvesWall Position
            | ReplaceEmpty Position
            | AddGold Position
            | LooseTriggered Position
            | NoAction

type Actions = [Action]

dispatch :: Command -> Game Actions
dispatch c = case c of
  MovePlayer m -> movePlayer_ m
  SwitchPlayer -> switchPlayer >> return [NoAction]
  SomeOtherCommand -> (liftIO . print) "SomeOtherCommand was sent!" >> return [NoAction]


-- | The network description.
-- TODO: Add an event that triggers different animations or other feedback for the user.
--       The whole game state should be implemented in the network of events.
--       The positions could be behaviours. The field could also be a behaviour,
--       and whenever one changes, an event is triggered so that the UI can
--       create some nice output. The event for a field change should contain a list of positions
--       and a type encoded change, e.g. old stone / new stone, change from stone A to stone B, etc.
--       so that the UI can decide what user feedback it needs to produce.
networkDesc :: forall t. Frameworks t => (GameState -> Actions -> IO ()) -> AddHandler (GameState,Command) -> Moment t ()
networkDesc commandAction addHandler = do
  cmdE <- fromAddHandler addHandler  -- The command event
  reactimate $ fmap (\(st,cmd) -> contGame st (dispatch cmd) >>= commandAction st . fst) cmdE
  

startNetwork :: (GameState -> Actions -> IO ()) -> AddHandler (GameState,Command) -> IO ()
startNetwork commandAction addHandler = compile (networkDesc commandAction addHandler) >>= actuate
