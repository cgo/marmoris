module Main where

import Marmoris.Game
import Marmoris.Game.UI
import Control.Monad.IO.Class

main = run act


act :: Game ()
act = --setPlayer1 (1,1) >>
      --setPlayer2 (2,2) >>  
      liftIO (print "Tick") >>
      setStone Wall (2,2) >>
      setStone Wall (3,3) >>
      setStone Acid (3,4)