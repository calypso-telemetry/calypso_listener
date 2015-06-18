-module(cl_object_event).
-author("Sergey Loguntsov").

%% API
-export([
  start_link/0,
  add_handler/3, add_sup_handler/3,
  telemetry/2, control/2
]).



%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

-spec(start_link() -> {ok, pid()} | {error, {already_started, pid()}}).
start_link() ->
  gen_event:start_link().

add_handler(Pid, Module, Options) ->
  gen_event:add_handler(Pid, Module, Options).

add_sup_handler(Pid, Module, Options) ->
  gen_event:add_sup_handler(Pid, Module, Options).

telemetry(Pid, Telemetry) ->
  gen_event:notify(Pid, { telemetry, Telemetry }).

control(Pid, Msg) ->
  gen_event:notify(Pid, { control, Msg }).


