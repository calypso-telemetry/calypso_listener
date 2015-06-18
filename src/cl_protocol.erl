-module(cl_protocol).
-author("begemot").

-include_lib("calypso_core/include/logger.hrl").

%% API
-export([
  info/1, start/3, stop/2,
  init/3, terminate/4,
  handle_frame_in/4, handle_frame_out/4, handle_info/4
]).

-type options() :: proplists:proplist().
-type state() :: term().
-type port_number() :: pos_integer().
-type protocol() :: cl_tcp_transport:protocol().
-type action() :: ok | { stop, Reason :: term() } | { error, Reason :: term() } | { upgrade, Module :: atom() } | { reply, Binary :: binary() } |  {telemetry, Telemetry ::maps:map() }.
-type actions() :: action() | [action()].
-type answer() :: { actions(), NewState :: state(), NewProtocol :: protocol() }.


-callback start(Port :: port_number(), Options :: options()) -> ok | { error, Reason :: state() }.
-callback stop(Port :: port_number()) -> ok.

-callback init(Options :: options(), Protocol :: protocol()) -> answer().
-callback terminate(Reason :: term(), State :: state(), Protocol :: protocol()) -> ok.

-callback handle_frame_in(Binary :: binary(), State :: state(),Protocol :: protocol() ) -> answer().
-callback handle_frame_out(Msg :: term(), State :: state(), Protocol :: protocol()) -> answer().
-callback handle_info(Msg :: term(), State :: state(), Protocol :: protocol()) -> answer().

info(Module) ->
  Module:init().

start(Module, Port, Options) ->
  Module:start(Port, Options).

stop(Module, Port) ->
  Module:stop(Port).

init(Module, Options, Protocol) ->
  Module:init(Options, Protocol).

terminate(Module, Reason, State, Protocol) ->
  ?INFO("Terminate ~p ~p", [ Module, Reason ]),
  Module:terminate(Reason, State, Protocol).

handle_frame_in(Module, Binary, State, Protocol) ->
  ?INFO("Receive data ~p ~p ~p ~p", [ Binary, Module, State, Protocol ]),
  try Module:handle_frame_in(Binary, State, Protocol) catch
    throw:Result -> Result
  end.

handle_frame_out(Module, Msg, State, Protocol) ->
  try Module:handle_frame_out(Msg, State, Protocol) catch
    throw:Result -> Result
  end.

handle_info(Module, Msg, State, Protocol) ->
  try Module:handle_info(Msg, State, Protocol) catch
    throw:Result -> Result
  end.


%% INTERNALS


