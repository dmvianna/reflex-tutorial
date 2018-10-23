{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}
module Ex01.Exercise where

import           Control.Monad (zipWithM)
import qualified Data.Text     as Text
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
    products = [carrot, celery, cucumber]
    events = [eCarrot, eCelery, eCucumber]
    spends = (<$) <$> pCost <$> products
    vends = (<$) <$> pName <$> products
    eVend =
      mergeWith (<>) $ vends <*> events
    eSpend =
      mergeWith (+) $ spends <*> events
    eChange =
      ffilter (0 <) $ (money -) <$> eSpend
    eNotEnoughMoney =
     fmapMaybe f $ (0 >) <$> (money -) <$> eSpend
        where
          f True  = Just ()
          f False = Nothing
  in
    Outputs eVend eSpend eChange eNotEnoughMoney

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex01
#endif
