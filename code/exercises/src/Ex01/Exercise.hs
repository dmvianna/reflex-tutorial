{-# LANGUAGE CPP #-}
module Ex01.Exercise where

import           Data.Text   (Text)
import           Reflex
#ifndef ghcjs_HOST_OS
import           Util.Run
#endif

import           Ex01.Common
import           Ex01.Run

ex01 ::
  Reflex t =>
  Int ->
  Inputs t ->
  Outputs t
ex01 money (Inputs eCarrot eCelery eCucumber eRefund) =
  let
    events = [carrot <$ eCarrot
             ,celery <$ eCelery
             ,cucumber <$ eCucumber]
    eName = pName <$> leftmost events
    eCost = pCost <$> leftmost events
    eVend =
      difference eName eNotEnoughMoney
    eSpend =
      ffilter (money >=) eCost
    eChange =
      money <$ eRefund
    eNotEnoughMoney =
      () <$ ffilter (money <) eCost
  in
    Outputs eVend eSpend eChange eNotEnoughMoney

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex01
#endif
