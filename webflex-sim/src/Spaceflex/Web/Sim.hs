{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Spaceflex.Web.Sim where

import Prelude hiding (filter)
import Spaceflex.Web.Base
import Reflex
import Reflex.Dom hiding (Value)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap(..))
import qualified Data.Text as T
import Data.Aeson
import Data.Semigroup hiding (Any)
import Control.Monad.Trans
import Data.Witherable
import Reflex.Id.Class
import Control.Monad
import Control.Monad.Fix
import qualified Reflex.Id.Impure as IdImpure
import Reflex.Wormhole.Class
import Reflex.Wormhole.Base
-- FIXME: Get rid of `delay 0.1`, figure out why it was needed.

-- Type of client identifier.
type C_ = Int
-- Type of connection identifier
type Cn_ = Int


runSimImpure :: (MonadIO m) => IdImpure.IdT m a -> m a
runSimImpure = IdImpure.runIdT

-- TODO: Deleting clients.
sim :: forall t m a b. ((DomBuilder t m, PostBuild t m,
                       PerformEvent t m, TriggerEvent t m, MonadIO (Performable m),
                       MonadHold t m, MonadFix m, Id m ~ Cn_, GetId m)) => ClientT t m a -> ServerT Cn_ t m b -> m ()
sim (ClientT cm) (ServerT sm) = mdo
  elAttr "div" ("style" =: "border: 1px solid gray; padding: 2em") $ do
    el "h1" $ text "The simulator"
    text "Connections"
    el "ul" $
      dyn_
      . fmap (mapM (\(c,t) ->
                       el "li" $ text (T.pack . show $ (c,t)))
              . Map.toList)
      . incrementalToDynamic
      $ conns_
    el "p" $ do
      text "Last message from client:"
      el "br" blank
      dynText =<< holdDyn "" (T.pack . show <$> rcvS)
  newClientE <- el "p" $ button "New client"
  let rcvS :: Event t (Cn_, Map Int Value) = fmap getFirst rcvS'
  ~(_,sndS) :: (b, Event t (MonoidalMap Cn_ (Map Int Value))) <-
    elAttr "div" ("style" =: "border: 1px solid gray; padding: 2em")
    $ evalREWST sm (rcvS, conns_) 0
  -- TODO: this really needs an incremental map in which the values are also incremental
  conns_ :: Incremental t (PatchMap Cn_ ()) <-
    holdIncremental mempty . fmap PatchMap $ conDisconC
  clientNum <- count newClientE
  ( ( _clients :: Dynamic t (Map C_ ())
    , conDisconC :: Event t (Map Cn_ (Maybe ()))
    )
    , rcvS' :: Event t (First (Cn_, Map Int Value))
    ) <- runEventWriterT
      . runEventWriterT
      . listHoldWithKey mempty (leftmost [ Map.singleton <$> current clientNum <@> (Just <$> newClientE)
--                                         , deleteClient
                                         ])
      $ \n () -> elAttr "div" ("style" =: "margin-top: 1em") $ do
          text $ "Client " <> T.pack (show n) -- <> ", delay: "
          text ", connected "
          connectedDyn :: Dynamic t Bool <- value <$> checkbox True def
          let getId' = getId -- TODO: hash it
          ~(id0,idE) <- runWithReplace getId' (getId' <$ filter id (updated connectedDyn))
          connIdDyn :: Dynamic t (Maybe Cn_) <-
            holdDyn (Just id0) (leftmost [ Nothing <$ filter not (updated connectedDyn)
                                         , Just <$> idE
                                         ])
          dynText (maybe "" ((" as " <>) . T.pack . show) <$> connIdDyn)
          (tellEvent
            =<< delay 0.1 . fmap (Map.singleton id0 . Just)
            =<< getPostBuild)
          text ", "
          (tellEvent <=< delay 0.1 $ (\old new ->
                       case (old,new) of
                         (Nothing, Just cn) -> Map.singleton cn (Just ())
                         -- FIXME: this doesn't seem to disconnect
                         (Just cn, Nothing) -> Map.singleton cn Nothing
                         _ -> error "???")
                    <$> current connIdDyn <@> updated connIdDyn)
--          deleteE <- button "delete"
          rcvC <- delay 0.5 . catMaybes $ (\maybeCn (MonoidalMap msg) -> do
                                              cn <- maybeCn
                                              Map.lookup cn msg)
                                         <$> current connIdDyn
                                         <@> sndS
          ~(_, sndC) <- elAttr "div" ("style" =: "border: 1px solid gray; padding: 2em") . lift . lift $ evalREWST cm (rcvC, connectedDyn) 0
          lift . tellEvent <=< delay 0.5 . catMaybes $
            (\maybeCn msg -> (\cn -> First . (cn,) $ msg) <$> maybeCn)
            <$> current connIdDyn
            <@> sndC
 --         tellEvent (Map.singleton n Nothing <$ deleteE)
          pure ()
  pure ()
