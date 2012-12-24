module Main where

import Marmoris.Editor.GUI
import Graphics.UI.Gtk

main = do
  initGUI
  window <- mainWindow
  windowSetDefaultSize window 640 480
  windowSetPosition window WinPosCenter
  widgetShowAll window
  on window hideSignal $ mainQuit
  mainGUI