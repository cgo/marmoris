module Main where

import Marmoris.Game
import Marmoris.Game.UI
import Control.Monad.IO.Class
import Control.Monad (zipWithM_)

main = run act


act :: Game ()
act = --setPlayer1 (1,1) >>
      --setPlayer2 (2,2) >>  
      liftIO (print "Tick") >>
      setStone Wall (2,2) >>
      setStone Wall (3,3) >>
      setStone Acid (3,4) >>
      setStone Acid (1,2) >>
      setStone Empty (3,4) >>
      setStone ReplacementTile (1,4) >>
      setStone Loose (3,0) >>
      setStone ReplacementTile (4,0) >>
      mapM_ (setStone Gold) [(1,3),(1,4)]
      
      --fieldBounds >>=
      --zipWithM_ setStoneById [1..42] . range