-module(cl_udp_transport_sup).
-author("Sergey Loguntsov").

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  add_udp/3,
  delete_udp/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
add_udp(Port, Module, Options) ->
  supervisor:start_child(?SERVER, { Port, { cl_udp_transport, start_link, [ Module, Port, Options ] }, permanent, 2000, worker, [ cl_udp_transport ]}).

delete_udp(Port) ->
  supervisor:terminate_child(?SERVER, Port),
  supervisor:delete_child(?SERVER, Port).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
