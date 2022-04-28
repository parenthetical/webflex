{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- FIXME: Original Reflex TodoMVC copyright notice

module TodoMVC 
(todomvc)
where

import Prelude hiding (mapM, mapM_, sequence,filter)

import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom (run)
import Data.Bifunctor
import Reflex
import Reflex.Dom.Core
import Data.Bool
import Reflex.Wormhole.Class
import Reflex.Id.Class
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad (mapM)
import Control.Lens ((&), (%~), (.~))
import Data.Proxy
import Data.Witherable (catMaybes,filter)
import Webflex.Class
import Data.Data (Data)
import Data.Aeson (ToJSONKey, FromJSONKey, FromJSON,ToJSON)
import GHC.Generics (Generic)

type SetWormhole t m a = Event t a -> m ()

data Task
   = Task { taskDescription :: Text
          , taskCompleted :: Bool
          }
   deriving (Show, Read, Eq, Generic)

instance FromJSON Task
instance ToJSON Task

data Task' t m
  = Task' { task :: Task
          , setCompleted :: SetWormhole t m Bool
          , setDeleted :: SetWormhole t m ()
          , setDescription :: SetWormhole t m Text
          }

type TaskList k t m = Map k (Task' t m)

-- | Subsets of the task list that can be selected by the user
data Filter
   = All -- ^ All tasks
   | Active -- ^ Uncompleted tasks
   | Completed -- ^ Completed tasks
   deriving (Show, Eq)

-- | Determine whether this Task should be shown when this Filter is in effect
satisfiesFilter :: Filter -> Task -> Bool
satisfiesFilter f = case f of
  All -> const True
  Active -> not . taskCompleted
  Completed -> taskCompleted

atAllCIncMap :: forall k a c s m. (JSON a, JSON k, Ord k, WebM c s m, Reflex s, Reflex c, MonadHold c (CM m), Monad m, FromJSONKey k, ToJSONKey k, MonadFix m) => Map k a -> Incremental s (PatchMap k a) -> m (Incremental c (PatchMap k a))
atAllCIncMap init d = mdo
  conns <- askConnections
  let newCon :: Event s (Map (C m) (Map k a)) =
        pushAlways (\p -> do
                       v <- sample (currentIncremental d)
                       pure . (v <$) . catMaybes . unPatchMap $ p)
        $ updatedIncremental conns
  reconnectValCE <- atCE newCon
  newPatchCE <- atAllCE (unPatchMap <$> (updatedIncremental d))
  let reconnectPatchCE :: Event c (Map k (Maybe a)) =
        fmap (fmap Just) reconnectValCE
  let oldReset :: Event c (Map k (Maybe a)) =
        fmap (const Nothing) <$> currentIncremental mapAtC  <@ reconnectPatchCE
  -- FIXME: Thoroughly test this
  -- On reconnect this tries to reset the local state by deleting' all keys, except the ones which still exist on the server.
  mapAtC <- fmap flattenBIncremental . liftC' $ holdIncremental init . fmap PatchMap . mconcat $
    [ oldReset
    , reconnectPatchCE
    , newPatchCE
    ]
  pure mapAtC

data Webhole c s m a = Webhole
  { whSE :: Event s (C m, a)
  , whCF :: Event c a -> CM m ()
  , whCE :: Event c a
  }

webhole :: forall c s m a. (Monad m, Wormholed c (CM m), Semigroup a, Reflex c,
                   FromJSON a, ToJSON a, WebM c s m, MonadSample c (CM m)) => m (Webhole c s m a)
webhole = do
  -- (ce, cf)
  cecf <- liftC' wormhole
  let ce = flattenB . fmap fst $ cecf
  let cf :: Event c a -> CM m ()
      cf e = ($ e) =<< sample (fmap snd cecf)
  se <- atSE ce
  pure (Webhole se cf ce)

unsafeWebhole :: (Wormholed c (CM m), Reflex c, FromJSON a, ToJSON a,
                   WebM c s m, Monad m, Monad (CM m), MonadSample c (CM m)) => m (Webhole c s m a)
unsafeWebhole = do
  cecf <- liftC' unsafeWormhole
  let ce = flattenB . fmap fst $ cecf
  let cf e = ($ e) =<< sample (fmap snd cecf)
  se <- atSE ce
  pure (Webhole se cf ce)


todomvc :: forall c s m. (WebM c s m, DomBuilder c (CM m), Wormholed c (CM m), PostBuild c (CM m),
                   MonadHold c (CM m), MonadHold s (SM m), MonadFix m,
                   MonadFix (CM m), MonadFix (SM m), Reflex s) => m ()
todomvc = do
  -- CRASH: happens when I use wormhole and once I add the count for the task ids
  newTaskH <- liftC' $ unsafeWormhole
  let newTaskCE = flattenB . fmap fst $ newTaskH
  let newTaskCF e = ($ e) =<< sample (fmap snd newTaskH)
  newTaskE <- liftC_ taskEntry
  newTaskSE <- atSE newTaskCE
  nextTaskNum :: Dynamic s Integer <- liftS $ count newTaskSE
  taskListSDyn :: Dynamic s (Map Integer Task) <- liftS $ foldDyn (\(k,t) ts -> Map.insert k t ts) mempty (attach (current nextTaskNum) (flip Task False . snd <$> newTaskSE))
  taskListCDyn <- atAllCDyn mempty taskListSDyn
  liftC_ . dynText . fmap (T.pack . show) $ taskListCDyn

todomvcNoCrash :: forall c s m. (WebM c s m, DomBuilder c (CM m), Wormholed c (CM m), PostBuild c (CM m),
                   MonadHold c (CM m), MonadHold s (SM m), MonadFix m,
                   MonadFix (CM m), MonadFix (SM m), Reflex s) => m ()
todomvcNoCrash = do
  newTaskSE <- atSE =<< liftC taskEntry
  nextTaskNum :: Dynamic s Integer <- liftS $ count newTaskSE
  taskListSDyn :: Dynamic s (Map Integer Task) <- liftS $ foldDyn (\(k,t) ts -> Map.insert k t ts) mempty (attach (current nextTaskNum) (flip Task False . snd <$> newTaskSE))
  taskListCDyn <- atAllCDyn mempty taskListSDyn
  liftC_ . dynText . fmap (T.pack . show) $ taskListCDyn


todomvc' :: forall c s m. (WebM c s m, DomBuilder c (CM m), Wormholed c (CM m), PostBuild c (CM m),
                   MonadHold c (CM m), MonadHold s (SM m), MonadFix m,
                   MonadFix (CM m), MonadFix (SM m), Reflex s) => m ()
todomvc' = mdo
  liftC_ $ mainHeader
  newTask :: Event s Text <- atSE_ =<< liftC taskEntry
  nextTaskNum :: Dynamic s Integer <- liftS $ count newTask
  taskListInc :: Incremental s (PatchMap Integer Task) <- fmap flattenBIncremental . liftS' $
    holdIncremental mempty
      (((\k v -> PatchMap (Map.singleton k (Just (Task v False))))
       <$> current nextTaskNum
       <@> newTask)
       <> deleteCompletedPatch)
  let deleteCompletedPatch =
        (PatchMap . fmap (const Nothing) . filter taskCompleted)
        <$> currentIncremental taskListInc
        <@ whSE whDeleteCompleted
  taskListAtC <- atAllCIncMap mempty taskListInc
  whTaskComplete  :: Webhole c s m (Integer, Bool) <- unsafeWebhole
  whTaskDelete  :: Webhole c s m Integer <- unsafeWebhole
  whTaskDescription :: Webhole c s m (Integer, Text) <- unsafeWebhole
  let fooTask :: Integer -> Task -> Task' c (CM m)
      fooTask k t = Task' t (whCF whTaskComplete . fmap (k,))
                    (whCF whTaskDelete . (k <$))
                    (whCF whTaskDescription . fmap (k,))
  -- TODO: Functions to make using PatchMap Incrementals easier would be nice.
  let taskListAtC' :: Incremental c (PatchMap Integer (Task' c (CM m))) =
        unsafeMapIncremental (Map.mapWithKey fooTask) (PatchMap . Map.mapWithKey (\k -> fmap (fooTask k)) . unPatchMap) taskListAtC
  -- TODO: make this take Incremental
  whDeleteCompleted :: Webhole c s m () <- unsafeWebhole
  let taskListAtCDyn :: Dynamic c (TaskList Integer c (CM m)) = incrementalToDynamic taskListAtC'
  liftC_ $ taskList activeFilter taskListAtCDyn (const (pure ()))
  activeFilter :: Dynamic c Filter <- liftC $ controls (fmap task <$> incrementalToDynamic taskListAtC') (whCF whDeleteCompleted)
  liftC_ infoFooter
  pure ()

-- | Display the main header
mainHeader :: DomBuilder t m => m ()
mainHeader = el "h1" $ text "todos"

-- | Strip leading and trailing whitespace from the user's entry, and discard it if nothing remains
stripDescription :: Text -> Maybe Text
stripDescription d =
  let trimmed = T.strip d
  in if T.null trimmed
     then Nothing
     else Just trimmed

keyCodeIs :: Key -> KeyCode -> Bool
keyCodeIs k c = keyCodeLookup c == k

-- | Display an input field; produce new Tasks when the user creates them
taskEntry
  :: forall t m.
     ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     )
  => m (Event t Text)
taskEntry = el "header" $ do
  -- Create the textbox; it will be cleared whenever the user presses enter
  rec let newValueEntered = keypress Enter descriptionBox
      descriptionBox <- inputElement $ def
        & inputElementConfig_setValue .~ fmap (const "") newValueEntered
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            mconcat [ "class" =: "new-todo"
                    , "placeholder" =: "What needs to be done?"
                    , "name" =: "newTodo"
                    , "type" =: "text"
                    ]
  -- -- Request focus on this element when the widget is done being built
  -- schedulePostBuild $ liftIO $ focus $ _textInput_element descriptionBox
  let -- | Get the current value of the textbox whenever the user hits enter
      newValue = tag (current $ value descriptionBox) newValueEntered
  -- -- Set focus when the user enters a new Task
  -- performEvent_ $ fmap (const $ liftIO $ focus $ _textInput_element descriptionBox) newValueEntered
  return $ fmapMaybe stripDescription newValue

-- | Display the user's Tasks, subject to a Filter; return requested modifications to the Task list
taskList
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Ord k
     )
  => Dynamic t Filter
  -> Dynamic t (TaskList k t m)
  -> (Event t Bool -> m ())
  -> m ()
taskList activeFilter tasksDyn toggleAllEF = elAttr "section" ("class" =: "main") $ do
  -- TODO: Explore the possibility of using Incremental more
  -- thoroughly, rather than switching over to Dynamic.
  -- This would be interesting for things like the "toggle all state".
  let toggleAllState = not . all (taskCompleted . task) . Map.elems <$> tasksDyn
      toggleAllAttrs = ffor tasksDyn $ \t ->
        "class" =: "toggle-all" <> "name" =: "toggle" <> if Map.null t then "style" =: "visibility:hidden" else mempty
  void $ toggleInput toggleAllAttrs toggleAllState
  toggleAllEF . tag (current toggleAllState) -- =<< button "toggle" --
    <=< fmap (domEvent Click . fst) . elAttr' "label" ("for" =: "toggle-all") $ text "Mark all as complete"
  -- Filter the item list
  let visibleTasks = zipDynWith (\af ts -> Map.filter (satisfiesFilter af . task) ts) activeFilter tasksDyn
  -- Hide the item list itself if there are no items
  let itemListAttrs = ffor visibleTasks $ \t -> mconcat
        [ "class" =: "todo-list"
        , if Map.null t then "style" =: "visibility:hidden" else mempty
        ]
  -- Display the items
  void $ elDynAttr "ul" itemListAttrs $ list visibleTasks todoItem
  pure ()

toggleInput
  :: forall t m
  .  ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t (Map AttributeName Text)
  -> Dynamic t Bool
  -> m (Event t ())
toggleInput dynAttrs dynChecked = do
  let attrs = (<> "class" =: "toggle") . ("type" =: "checkbox" <>) <$> dynAttrs
      updatedAttrs = fmap Just <$> updated dynAttrs
      updatedChecked = updated dynChecked
  initialAttrs <- sample $ current attrs
  initialChecked <- sample $ current dynChecked
  fmap (domEvent Click) . inputElement $ (def :: InputElementConfig EventResult t (DomBuilderSpace m))
    & inputElementConfig_initialChecked .~ initialChecked
    & inputElementConfig_setChecked .~ updatedChecked
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ updatedAttrs
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initialAttrs
    -- Prevent the checkbox from changing outside of updates:
    & inputElementConfig_elementConfig . elementConfig_eventSpec %~
        addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const preventDefault)

buildCompletedCheckbox
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t (Task' t m)
  -> m (Event t ())
buildCompletedCheckbox tDyn = elAttr "div" ("class" =: "view") $ do
  t <- sample (current tDyn)
  -- Display the todo item's completed status, and allow it to be set
  let todoDyn = task <$> tDyn
  completed <- holdUniqDyn $ fmap taskCompleted todoDyn
  setCompleted t . tag (fmap not $ current completed) =<< toggleInput (constDyn mempty) completed
  -- Display the todo item's name for viewing purposes
  (descriptionLabel, _) <- el' "label" $ dynText (fmap taskDescription todoDyn)
  -- Display the button for deleting the todo item
  setDeleted t . domEvent Click . fst <=< elAttr' "button" ("class" =: "destroy") $ return ()
  pure (void $ domEvent Dblclick descriptionLabel)

-- | Display an individual todo item
todoItem
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t (Task' t m)
  -> m ()
todoItem tDyn = do
  let todo = task <$> tDyn
  description <- holdUniqDyn $ fmap taskDescription todo
  t <- sample (current tDyn)
  rec -- Construct the attributes for our element
      let attrs = ffor2 todo editing $ \t' e -> Map.singleton "class" $ T.unwords $ concat
            [ [ "completed" | taskCompleted t' ]
            , [ "editing" | e ]
            ]
        
      editing <- elDynAttr "li" attrs $ do
        -- (setCompleted, destroy, startEditing) <-
        startEditing <- buildCompletedCheckbox tDyn
        -- Set the current value of the editBox whenever we start editing (it's not visible in non-editing mode)
        let setEditValue = tag (current description) $ ffilter id $ updated editing
        editBox <- inputElement $ def
          & inputElementConfig_setValue .~ setEditValue
          & inputElementConfig_elementConfig . elementConfig_initialAttributes
            .~ ("class" =: "edit" <> "name" =: "title")
        -- Set the todo item's description when the user leaves the textbox or presses enter in it
        let setDescriptionE = leftmost [ keypress Enter editBox, domEvent Blur editBox ]
        setDescription t . mapMaybe stripDescription . tag (current $ value editBox) $ setDescriptionE
        -- Cancel editing (without changing the item's description) when the user presses escape in the textbox
        let cancelEdit = keypress Escape editBox
        -- Determine the current editing state; initially false, but can be modified by various events
        holdDyn False $ leftmost [ True <$ startEditing
                                 , False <$ setDescriptionE
                                 , False <$ cancelEdit
                                 ]
  -- Return an event that fires whenever we change ourselves
  return ()

buildActiveFilter
  :: forall t m.
     ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => m (Dynamic t Filter)
buildActiveFilter = elAttr "ul" ("class" =: "filters") $ do
  rec activeFilter <- holdDyn All setFilter
      let filterButton :: Filter -> m (Event t Filter)
          filterButton f = el "li" $ do
            let buttonAttrs = ffor activeFilter $ \af -> "class" =: if f == af then "selected" else ""
            (e, _) <- elDynAttr' "a" buttonAttrs $ text $ T.pack $ show f
            return $ fmap (const f) (domEvent Click e)
      allButton <- filterButton All
      text " "
      activeButton <- filterButton Active
      text " "
      completedButton <- filterButton Completed
      let setFilter = leftmost [allButton, activeButton, completedButton]
  return activeFilter

-- | Display the control footer; return the user's currently-selected filter and an event that fires when the user chooses to clear all completed events
controls
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t (Map k Task)
  -> SetWormhole t m ()
  -> m (Dynamic t Filter)
controls tasks clearCompletedEF = do
  -- Determine the attributes for the footer; it is invisible when there are no todo items
  let controlsAttrs = ffor tasks $ \t -> "class" =: "footer" <> if Map.null t then "style" =: "visibility:hidden" else mempty
  elDynAttr "footer" controlsAttrs $ do
    -- Compute the number of completed and uncompleted tasks
    let (tasksCompleted, tasksLeft) = splitDynPure $ ffor tasks $ \m ->
          let completed = Map.size $ Map.filter taskCompleted m
          in (completed, Map.size m - completed)
    elAttr "span" ("class" =: "todo-count") $ do
      el "strong" $ dynText $ fmap (T.pack . show) tasksLeft
      dynText $ fmap (\n -> (if n == 1 then " item" else " items") <> " left") tasksLeft
    activeFilter <- buildActiveFilter
    let clearCompletedAttrs = ffor tasksCompleted $ \n -> mconcat
          [ "class" =: "clear-completed"
          , if n > 0 then mempty else "hidden" =: ""
          ]
    (clearCompletedAttrsButton, _) <- elDynAttr' "button" clearCompletedAttrs $ dynText $ ffor tasksCompleted $ \n -> "Clear completed (" <> T.pack (show n) <> ")"
    clearCompletedEF (domEvent Click clearCompletedAttrsButton)
    return activeFilter

-- | Display static information about the application
infoFooter :: DomBuilder t m => m ()
infoFooter = elAttr "footer" ("class" =: "info") $ do
  el "p" $ text "Click to edit a todo"
  el "p" $ do
    text "Written by "
    elAttr "a" ("href" =: "https://github.com/ryantrinkle") $ text "Ryan Trinkle"
  el "p" $ do
    text "Part of "
    elAttr "a" ("href" =: "http://todomvc.com") $ text "TodoMVC"
