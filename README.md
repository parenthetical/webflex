# Webflex

Purely functional web applications for Reflex FRP.

## Interface
Found in [webflex-core:Webflex.Class](webflex-core/src/Webflex/Class.hs).

```haskell
-- | Client-server web programming in Reflex. The @c@ and @s@ type
-- variables are the type of Reflex timelines of the clients and the
-- server. There are corresponding @CM@ and @SM@ contexts for clients
-- and server.
class (Ord (C m)) => WebM c s m | m -> c, m -> s where
  -- | Client identifiers.
  type C m :: *
  -- | Client-side Reflex monad.
  type CM m :: * -> *
  -- | Server-side Reflex monad.
  type SM m :: * -> *
  -- TODO: Consider making this Event c [a] (enabling "batch processing").
  -- | Takes a server-side event with client-tagged occurrences, and
  -- returns a client-side event. The client-side event will have an
  -- occurrence when the server-side occurrence tagged with that
  -- particular client's identifier arrives at the client.
  atCE :: (JSON a) => Event s (Map (C m) a) -> m (Event c a)
  -- TODO: Consider making this `Event (Map (C m) a)` (batch processing).
  -- | Knowledge of client-side event occurrences at the server.
  atSE :: (JSON a) => Event c a -> m (Event s (C m, a))
  -- | Make the client-side Reflex program available in the 'WebM' monad. The result is
  -- wrapped in a (Behavior c) so that client-values cannot leak into
  -- server-side contexts.
  liftC' :: CM m a -> m (Behavior c a)
  -- | Make the server-side Reflex program available in the 'WebM' monad. The result is
  -- wrapped in a (Behavior s) so that server-values cannot leak into
  -- client-side contexts.
  liftS' :: SM m a -> m (Behavior s a)
```

## Implementation
The implementation has two sides:
- A pure base implementation found in
  [webflex-core:Webflex.Base](webflex-core/src/Webflex/Base.hs). This
  implements the interface by translating the functions to
  operations on two monad transformer stacks. One for the server
  (`ServerT`) and one for the client (`ClientT`).
- Simulator, client, and server implementations. These take the result
  of running the client/server transformer stacks and either simulate
  a Webflex program as a regular Reflex program playing both client
  and server sides
  ([weflex-sim:Webflex.Sim](webflex-sim/src/Webflex/Sim.hs)), or as a HTTP
  server and a browser implementation
  ([webflex-server:Webflex.Server](webflex-server/src/Webflex/Server.hs) and [webflex-client:Webflex.Client](webflex-client/src/Webflex/Client.hs)).

### Pure implementation
The `ServerT` asd `ClientT` transformers are Reader-EventWriter-State stacks:
- They number uses of `atCE` and `atSE` via a counter (State) such
that the number for is the same on client and server.
- The argument events to those functions are then tagged with the
number and passed to `tellEvent` (EventWriter).
- The appropriate return values of the functions are looked up up from
an `Event t (Map Int Value)` value (Reader).

The transformers only implement *one side* of the `WebM` class. The
design of the class makes sure that a client-tagged value or monad
(`c` or `CM`) will never be evaluated on the server and vice-versa.
To keep the compiler happy I've added a "Void" implementation of
Reflex and Reflex-DOM classes in which all functions are `undefined`
([reflex-extras:Reflex.Void](reflex-extras/src/Reflex/Void.hs)).

## Examples

There is a simulator for Webflex so that you can easily test out programs.

See `webflex-todomvc` for a client-server version of `reflex-todomvc`.

Run `nix-shell --run "cabal run exe:sim-test"` for a simulated button-click counter app (shows the total number of clicks of a button).
