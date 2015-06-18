-module(calypso_listener_app).
-author("Sergey Loguntsov").

-behaviour(application).

%% Application callbacks
-export([
  start/0, start/2, stop/1,
  version/0
]).

-define(APP, calypso_listener).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start() ->
  calypso_starter:start(?APP),
  ok.

version() -> "0.01".

-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  cl_device_command:init(),
  Pid = spawn_link(fun() ->
    calypso_starter:empty_loop(
      fun() -> ok end
    ) end),
  { ok,Pid }.

-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

