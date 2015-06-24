-module(cl_udp_listener).
-author("Sergey Loguntsov").

-include_lib("calypso_core/include/cl_device.hrl").

%% API
-export([
  start_link/3
]).

-behaviour(gen_server).
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  socket :: port(),
  module :: atom(),
  options :: term()
}).

-define(CONNECTION_TIMEOUT, 600). %% Default connection timeout for udp devices, seconds

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, Module, Options ) when is_atom(Module), is_integer(Port) ->
  gen_server:start_link(?MODULE, [ Port, Module, Options ], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ Port, Module, Options ]) ->
  { ok, Socket } = gen_udp:open(Port, [
    { active, once },
    binary
  ]),
  {ok, #state{
    socket = Socket,
    module = Module,
    options = Options
  }}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({udp, Socket, _IP, _InPortNo, Packet}, State) ->
  Result =try
    case cl_protocol:get_device_login(State#state.module, Packet) of
      { ok, Login} ->
        DevicePid = case cl_udp_device:pid(Login) of
          undefined ->
            case calypso_db:get(device, {by_login, Login}) of
              { ok, Device } when ?IS_DEVICE(Device) ->
                Timeout = proplists:get_value(connection_timeout, State#state.options, ?CONNECTION_TIMEOUT),
                NewOptions = proplists:delete(connection_timeout, State#state.options),
                { ok, Pid } = cl_udp_device:start_link(State#state.socket, State#state.module, Login, Device, Timeout, NewOptions),
                Pid;
              _ ->
                throw({noreply, State})
            end;
          Pid when is_pid(Pid) -> Pid
        end,
        cl_udp_device:receive_packet(DevicePid, Packet);
      _ -> throw({noreply, State})
    end
  catch
    throw:Reason-> Reason
  end,
  inet:setopts(Socket, { active, once }),
  Result;

handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
