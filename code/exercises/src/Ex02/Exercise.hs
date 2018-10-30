{-# LANGUAGE CPP #-}
module Ex02.Exercise where

import           Reflex

#ifndef ghcjs_HOST_OS
import           Util.Run
#endif

import           Ex02.Common
import           Ex02.Run

ex02 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex02 (Inputs bMoney eCarrot eCelery eCucumber eRefund) =
  let
    eProduct =
      leftmost [
      carrot   <$ eCarrot
      , celery   <$ eCelery
      , cucumber <$ eCucumber
      ]
    checkNotEnoughMoney m p =
      if pCost p > m
      then Just $ pCost p
      else Nothing
    checkError m p =
      pCost p > m
    eNotEnoughMoney =
      attachWithMaybe checkNotEnoughMoney bMoney eProduct
    eSale =
      difference eProduct eNotEnoughMoney
    eVend =
      pName <$> eSale
    eSpend =
      pCost <$> eSale
    eChange =
      tag bMoney eRefund
    eError =
      NotEnoughMoney <$ attachWith checkError bMoney eProduct
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex02
#endif
