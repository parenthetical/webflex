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
{-# LANGUAGE TypeFamilies #-}

-- FIXME: Original Reflex TodoMVC copyright notice

module TodoMVC2
(todoMVC)
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
import Data.Aeson (ToJSONKey, FromJSONKey, FromJSON,ToJSON)
import GHC.Generics (Generic)
import Reflex
import Reflex.Dom.Core
    ( (&),
      keyCodeLookup,
      (.~),
      elementConfig_initialAttributes,
      elementConfig_modifyAttributes,
      inputElementConfig_elementConfig,
      inputElementConfig_initialChecked,
      inputElementConfig_setChecked,
      inputElementConfig_setValue,
      (=:),
      dynText,
      el,
      el',
      elAttr,
      elAttr',
      elDynAttr,
      elDynAttr',
      text,
      keypress,
      def,
      Key(Escape, Enter),
      KeyCode,
      AttributeName,
      DomBuilder(inputElement),
      HasDomEvent(domEvent),
      EventName(Click, Dblclick, Blur),
      HasValue(value) )
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
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NonEmpty
import Reflex.Persist.Class

data Task
   = Task { taskDescription :: Text
          , taskCompleted :: Bool
          }
   deriving (Show, Eq, Read, Generic)

-- | Add a new value to a map; automatically choose an unused key
insertNew_ :: (Enum k, Ord k) => v -> Map k v -> Map k v
insertNew_ v m = case Map.maxViewWithKey m of
  Nothing -> Map.singleton (toEnum 0) v
  Just ((k, _), _) -> Map.insert (succ k) v m

initialTasks :: Map Int Task
initialTasks = Map.empty

--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

data Action
  = ClearCompleted
  | NewTask Text
  | ChangeTask Int TaskAction
  | ToggleAll
  deriving (Show,Read,Generic)

data TaskAction
  = Update Task
  | DeleteTask
  deriving (Show,Read,Generic)

instance FromJSON Task
instance ToJSON Task
instance FromJSON TaskAction
instance ToJSON TaskAction
instance FromJSON Action
instance ToJSON Action

modifyTasks :: Action -> Map Int Task -> Map Int Task
modifyTasks a = case a of
  ClearCompleted -> Map.filter (not . taskCompleted)
  NewTask d -> insertNew_ (Task d False)
  ChangeTask k ta -> case ta of
    Update t -> Map.insert k t
    DeleteTask -> Map.delete k
  ToggleAll -> fmap (\(Task d c) -> Task d (not c))

todoMVC :: forall c s m. (WebM c s m, DomBuilder c (CM m), PostBuild c (CM m),
                   MonadHold c (CM m), MonadHold s (SM m), MonadFix m,
                   MonadFix (CM m), MonadFix (SM m), Reflex s, Persist s (SM m)) => m ()
todoMVC = mdo
  tasksDynS <- liftS $ mdo
    initialTasks' <- persistE initialTasks (updated tasksDynS)
    tasksDynS' <- foldDyn modifyTasks initialTasks' (snd <$> taskActionsES)
    pure tasksDynS'
  tasksDynC <- atAllCDyn mempty tasksDynS
  taskActionsEC <- liftC (todoMVCLocal tasksDynC)
  taskActionsES <- atSE taskActionsEC
  pure ()


todoMVCLocal
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t (Map Int Task)
  -> m (Event t Action)
todoMVCLocal tasks = mdo
  el "div" $ do
    taskActionsE <- elAttr "section" ("class" =: "todoapp") $ mdo
      mainHeader
      newTask <- taskEntry
      taskActionsE <- taskList activeFilter tasks
      (activeFilter, clearCompleted) <- controls tasks
      return . leftmost $ [ taskActionsE
                          , NewTask <$> newTask
                          , ClearCompleted <$ clearCompleted
                          ]
    infoFooter
    pure taskActionsE

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
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
--     , DomBuilderSpace m ~ GhcjsDomSpace
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
  :: forall t m k.
     ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Ord k
     , k ~ Int
     )
  => Dynamic t Filter
  -> Dynamic t (Map k Task)
  -> m (Event t Action)
taskList activeFilter tasks = elAttr "section" ("class" =: "main") $ do
  let toggleAllState = all taskCompleted . Map.elems <$> tasks
      toggleAllAttrs = ffor tasks $ \t -> "class" =: "toggle-all" <> "name" =: "toggle" <> if Map.null t then "style" =: "visibility:hidden" else mempty
  toggleAll <- toggleInput toggleAllAttrs toggleAllState
  elAttr "label" ("for" =: "toggle-all") $ text "Mark all as complete"
  -- Filter the item list
  let visibleTasks = zipDynWith (Map.filter . satisfiesFilter) activeFilter tasks
  -- Hide the item list itself if there are no items
  let itemListAttrs = ffor visibleTasks $ \t -> mconcat
        [ "class" =: "todo-list"
        , if Map.null t then "style" =: "visibility:hidden" else mempty
        ]
  -- Display the items
  items <- elDynAttr "ul" itemListAttrs $ list visibleTasks todoItem
  -- Aggregate the changes produced by the elements
  let combineItemChanges :: Map k (Event t TaskAction) -> Event t Action
      combineItemChanges = fmap NonEmpty.head . mergeList . map (\(k, e) -> ChangeTask k <$> e) . Map.toList
      itemChangeEvent = fmap combineItemChanges items
      itemChanges = switch $ current itemChangeEvent
  return (leftmost [ ToggleAll <$ toggleAll
                   , itemChanges])

toggleInput
  :: ( DomBuilder t m
--     , DomBuilderSpace m ~ GhcjsDomSpace
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
  domEvent Click <$> inputElement (def
    & inputElementConfig_initialChecked .~ initialChecked
    & inputElementConfig_setChecked .~ updatedChecked
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ updatedAttrs
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initialAttrs)

buildCompletedCheckbox
  :: ( DomBuilder t m
--     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t Task
  -> Dynamic t Text
  -> m (Event t Bool, Event t (), Event t ())
buildCompletedCheckbox todo description = elAttr "div" ("class" =: "view") $ do
  -- Display the todo item's completed status, and allow it to be set
  completed <- holdUniqDyn $ fmap taskCompleted todo
  checkboxClicked <- toggleInput (constDyn mempty) completed
  let setCompleted = fmap not $ tag (current completed) checkboxClicked
  -- Display the todo item's name for viewing purposes
  (descriptionLabel, _) <- el' "label" $ dynText description
  -- Display the button for deleting the todo item
  (destroyButton, _) <- elAttr' "button" ("class" =: "destroy") $ return ()
  return ( setCompleted
         , domEvent Click destroyButton
         , void $ domEvent Dblclick descriptionLabel
         )

-- | Display an individual todo item
todoItem
  :: ( DomBuilder t m
--     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t Task
  -> m (Event t TaskAction)
todoItem todo = do
  description <- holdUniqDyn $ fmap taskDescription todo
  rec -- Construct the attributes for our element
      let attrs = ffor2 todo editing' $ \t e -> Map.singleton "class" $ T.unwords $ concat
            [ [ "completed" | taskCompleted t ]
            , [ "editing" | e ]
            ]
      (editing', changeTodo) <- elDynAttr "li" attrs $ do
        (setCompleted, destroy, startEditing) <- buildCompletedCheckbox todo description
        -- Set the current value of the editBox whenever we start editing (it's not visible in non-editing mode)
        let setEditValue = tag (current description) $ ffilter id $ updated editing'
        editBox <- inputElement $ def
          & inputElementConfig_setValue .~ setEditValue
          & inputElementConfig_elementConfig . elementConfig_initialAttributes
            .~ ("class" =: "edit" <> "name" =: "title")
        let -- Set the todo item's description when the user leaves the textbox or presses enter in it
            setDescription = tag (current $ value editBox) $ leftmost
              [ keypress Enter editBox
              , domEvent Blur editBox
              ]
            -- Cancel editing (without changing the item's description) when the user presses escape in the textbox
            cancelEdit = keypress Escape editBox
            -- Put together all the ways the todo item can change itself
            changeSelf = leftmost [ fmap (\c t -> Update $ t { taskCompleted = c }) setCompleted
                                  , fmap (const (const DeleteTask)) destroy
                                  , fmap (\d t ->
                                            fromMaybe DeleteTask
                                            . fmap (\trimmed -> Update $ t { taskDescription = trimmed })
                                            $ stripDescription d)
                                    setDescription
                                  ]
        -- Set focus on the edit box when we enter edit mode
--        postGui <- askPostGui
--        performEvent_ $ fmap (const $ liftIO $ void $ forkIO $ threadDelay 1000 >> postGui (liftIO $ focus $ _textInput_element editBox)) startEditing -- Without the delay, the focus doesn't take effect because the element hasn't become unhidden yet; we need to use postGui to ensure that this is threadsafe when built with GTK
        -- Determine the current editing state; initially false, but can be modified by various events
        editing <- holdDyn False $ leftmost [ fmap (const True) startEditing
                                            , fmap (const False) setDescription
                                            , fmap (const False) cancelEdit
                                            ]
        return (editing, (\t a -> a t) <$> current todo <@> changeSelf)
  -- Return an event that fires whenever we change ourselves
  return changeTodo

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
      let filterButton :: (Filter -> m (Event t Filter))
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
  -> m (Dynamic t Filter, Event t ())
controls tasks = do
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
    return (activeFilter, domEvent Click clearCompletedAttrsButton)

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
