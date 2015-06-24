-module(cl_udp_device).
-author("Sergey Loguntsov").

-behaviour(gen_server).

-include("udp_transport.hrl").
-include_lib("calypso_core/include/cl_device.hrl").

%% API
-export([
  start_link/6, pid/1,
  receive_packet/2,
  send/2, close/2, device_update/2
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  udp_protocol :: #udp_transport{},
  timeout :: pos_integer(),
  stop_reason :: term(),
  timer_pid :: pid()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket, Module, DeviceLogin, Device, Timeout, Options) when is_atom(Module), is_integer(Timeout), ?IS_DEVICE(Device) ->
  case pid(DeviceLogin) of
    undefined ->
      gen_server:start_link(?MODULE, [Socket, Module, DeviceLogin, Device, Timeout, Options], []);
    Pid when is_pid(Pid) ->
      link(Pid),
      { ok, Pid }
  end.

pid(Login) ->
  cl_transport_handler:process({ udp, Login}).

receive_packet(Pid, Binary) when is_binary(Binary) ->
  gen_server:cast(Pid, { packet, Binary }).

send(_Pid, _Msg) ->
  { error, unsupported }.

close(_Pid, _Reason) ->
  ok.

device_update(Pid, Device) when ?IS_DEVICE(Device) ->
  gen_server:cast(Pid, { device_update, Device}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ Socket, Module, DeviceLogin, Device, Timeout, Options ]) ->
  { ok, State, NewProtocol } = cl_protocol:init(Module, Options, #udp_transport{
    socket = Socket,
    module = Module
  }),
  cl_transport_handler:register({ udp, DeviceLogin}, cl_udp_transport),
  cl_transport_handler:register(cl_device:id(Device), cl_udp_transport),
  {ok, #state{
    udp_protocol = NewProtocol#udp_transport{
      state = State,
      device = Device,
      module = Module
    },
    timeout = Timeout,
    timer_pid = start_timer(Timeout)
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
handle_cast({ packet, Binary}, State) ->
  wakeup_timer(State#state.timer_pid),
  Protocol = State#state.udp_protocol,
  answer_for_cast(cl_protocol:handle_frame_in(Protocol#udp_transport.module, Binary, Protocol#udp_transport.state, Protocol), State);

handle_cast({ device_update, NewDevice}, State) ->
  Protocol = State#state.udp_protocol,
  Device = Protocol#udp_transport.device,
  Device1 = cl_device:merge(NewDevice, Device),
  NewState = State#state{
    udp_protocol = Protocol#udp_transport{
      device = Device1
    }
  },
  case cl_device:is_active(Device1) of
    true ->
      { noreply, NewState };
    false ->
      do_stop(non_active, NewState)
  end;

handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(timeout, State) ->
  { stop, normal, State#state{
    stop_reason = timeout
  } };
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(R, State) ->
  Reason = case State#state.stop_reason of
    undefined -> R;
    Any -> Any
  end,
  Protocol = State#state.udp_protocol,
  cl_protocol:terminate(Protocol#udp_transport.module, Reason, Protocol#udp_transport.state, Protocol),
  stop_timer(State#state.timer_pid),
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

timeout_loop(Owner, Timeout) when is_pid(Owner), is_integer(Timeout) ->
  receive
    stop -> ok;
    _ -> timeout_loop(Owner, Timeout)
  after Timeout ->
    Owner ! timeout
  end.

start_timer(Timeout) ->
  Owner = self(),
  spawn_link(fun() ->
    timeout_loop(Owner, 1000 * Timeout)
  end).

stop_timer(Pid) ->
  Pid ! stop.

wakeup_timer(Pid) ->
  Pid ! wakeup.

answer_for_cast({ Answer, St, Protocol }, State) when is_tuple(Answer),?IS_UDP_TRANSPORT(Protocol) ->
  answer_for_cast_actions(Answer,
    State#state{
      udp_protocol = Protocol#udp_transport{
        state = St
      }
    }).
answer_for_cast_actions(ok, State) ->
  { noreply, State };
answer_for_cast_actions(Answer, State) when is_tuple(Answer) ->
  answer_for_cast_actions([Answer], State);

answer_for_cast_actions([], State) ->
  { noreply, State };
answer_for_cast_actions([Action | Tail ], State) ->
  case action(Action, State) of
    { ok, NewState } when is_record(NewState, state) ->
      answer_for_cast(Tail, NewState);
    { stop, Reason, NewState } when is_record(NewState, state) ->
      do_stop(Reason, NewState)
  end.

action({rest, _}, State) -> { ok, State };
action({stop, Reason}, State) -> { stop, Reason, State };
action({send, _ }, State) -> { ok, State }.

do_stop(Reason, State) ->
  { stop, normal, State#state{
    stop_reason = Reason
  }}.
