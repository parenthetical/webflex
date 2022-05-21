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

module Webflex.Sim where

import Prelude hiding (filter)
import Webflex.Base
import Reflex
import Reflex.Dom hiding (Value)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Monoidal (MonoidalMap(..))
import qualified Data.Text as T
import Data.Aeson
import Data.Semigroup hiding (Any)
import Control.Monad.Trans
import Data.Witherable
import Reflex.Id.Class
import Control.Monad
import Control.Monad.Fix
import Reflex.Wormhole.Class
import Reflex.Wormhole.Base
import Reflex.REWST

-- FIXME: Get rid of `delay 0.1`, figure out why it was needed.

-- TODO: hard coded delay 0.5 on send and receive.

-- Type of client identifier.
type C_ = Int

-- TODO: Deleting clients.
sim :: forall t m a b conn. (DomBuilder t m, PostBuild t m,
                       PerformEvent t m, TriggerEvent t m, MonadIO (Performable m),
                       MonadHold t m, MonadFix m, GetId m
                       , Wormholed t m
                       -- Type of connection identifier
                       , conn ~ Id m, Ord conn, Show conn
                       ) => ClientT t m a -> ServerT conn t m b -> m ()
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
  rcvS :: Event t (conn, Map Int Value) <-
    delay 0.5 . fmap getFirst $ clientMsgSentE
  ~(_,sndS) :: (b, Event t (MonoidalMap conn (Map Int Value))) <-
    elAttr "div" ("style" =: "border: 1px solid gray; padding: 2em")
    $ evalREWST sm (rcvS, conns_) 0
  -- TODO: this really needs an incremental map in which the values are also incremental
  conns_ :: Incremental t (PatchMap conn ()) <-
    holdIncremental mempty . fmap PatchMap $
      (Map.fromList . (fmap (,Just ())) <$> clientConnectedAtServerE)
      <> (Map.fromList  . (fmap (,Nothing)) <$> clientDisconnectedE)
  -- TODO: better names for connected events
  -- FIXME: transmission delay repeated at message send
  clientConnectedAtServerE <- delay 0.5 clientConnectedE
  -- TODO: think about client disconnect:
  -- clientDisconnectedAtServerE <- delay 0.5 clientDisconnectedE
  (clientDeletedE, addClientDeletedE) <- wormhole
  (clientConnectedE, addClientConnectedE) <- wormhole
  (clientDisconnectedE, addClientDisconnectedE) <- wormhole
  (clientMsgSentE, addClientMsgSentE) <- wormhole
  clientNum <- count newClientE
  --     , conDisconC :: Event t (Map Cn_ (Maybe ()))
  --     , rcvS' :: Event t (First (Cn_, Map Int Value))
  -- When a client connects it gets a new connection ID.
  _clients :: Dynamic t (Map C_ ()) <-
    listHoldWithKey mempty (leftmost [ Map.singleton <$> current clientNum <@> (Just <$> newClientE)
                                     -- WASHERE: fix map fromlist
                                     , Map.fromSet (const Nothing) . Set.fromList <$> clientDeletedE
                                     ])
      $ \n () -> elAttr "div" ("style" =: "margin-top: 1em") $ do
          text $ "Client " <> T.pack (show n) -- <> ", delay: "
          text ", connected "
          connectedDyn :: Dynamic t Bool <- value <$> checkbox True def
          let getId' = getId -- TODO: hash it
          let disconnectE = filter not (updated connectedDyn)
          let connectE = filter id (updated connectedDyn)
          -- Sequence of connection ids the client has (new one on every disconnect/connect)
          ~(id0,idE) <- runWithReplace getId' (getId' <$ connectE)
          -- TODO: Can we make a kind of incremental map wormhole for this?
          addClientConnectedE
            . ([id0] <$)
            =<< getPostBuild
          addClientConnectedE ((:[]) <$> idE)
          connIdDyn :: Dynamic t (Maybe conn) <-
            holdDyn (Just id0) (leftmost [ Nothing <$ disconnectE
                                         , Just <$> idE
                                         ])
          -- TODO: why is this clunky?
          addClientDisconnectedE ((:[]) <$> catMaybes (current connIdDyn <@ disconnectE))
          -- TODO: Make sure a client "connected" behavior respects information travel.
          -- Client connects on creation:
          dynText (maybe "" ((" as " <>) . T.pack . show) <$> connIdDyn)
          text ", "
          addClientDeletedE . ([n] <$) =<< button "delete"
          
          rcvC <- delay 0.5 . catMaybes $ (\maybeCn (MonoidalMap msg) -> do
                                              cn <- maybeCn
                                              Map.lookup cn msg)
                                         <$> current connIdDyn
                                         <@> sndS
          -- Client program is evaluated here:
          ~(_, sndC) <- elAttr "div" ("style" =: "border: 1px solid gray; padding: 2em") $ evalREWST cm (rcvC, connectedDyn) 0
          -- TODO: Again awkward catMaybes with connIdDyn like above.
          addClientMsgSentE (catMaybes ((\maybeConnId msg -> First . (,msg) <$> maybeConnId) <$> current connIdDyn <@> sndC))
          pure ()
  pure ()
