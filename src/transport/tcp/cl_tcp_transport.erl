-module(cl_tcp_transport).
-author("begemot").

-include("logger.hrl").
-include("tcp_transport.hrl").

-include_lib("calypso_core/include/cl_device.hrl").

-behaviour(ranch_protocol).
-export([start_link/4, init/4]).

-behaviour(gen_server).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

-behaviour(cl_transport_handler).
-export([send/2, close/2, start_listener/3, stop_listener/1, device_update/2]).

-define(TIMEOUT, 4 * 60 * 1000).

%% API
-export([]).

-type protocol() :: #cl_tcp_transport{}.
-export_type([ protocol/0 ]).

start_listener(Module, fake, Config) ->
  State = #cl_tcp_transport{
    module = Module,
    socket = fake,
    transport = fake
  },
  { ok, St, NewProt } = cl_protocol:init(Module, Config, State),
  { ok, cl_transport:set_state(St, NewProt)};

start_listener(Module, Port, Config) when is_integer(Port) ->
  ?INFO("Open port ~p for ~p", [ Port, Module ]),
  { ok, _Pid } = ranch:start_listener({ tcp, Port }, 1,
		ranch_tcp, [{port, Port}], ?MODULE, {Module, Config}),
	cl_port:register_port({ tcp, Port}, Module),
  ok.

stop_listener(fake) -> ok;
stop_listener({ tcp, Port}) ->
  ok = ranch:stop_listener({ tcp, Port }),
  cl_port:unregister_port({tcp, Port}),
  ?INFO("Close port ~p", [ Port ]),
  ok.

send(Pid, Term) ->
  gen_server:call(Pid, { send, Term }).

close(Pid, Reason) ->
  gen_server:cast(Pid, { close, Reason }).

device_update(Pid, Device) when ?IS_DEVICE(Device) ->
  gen_server:cast(Pid, { device_update, Device }).

start_link(Ref, Socket, Transport, Opts) ->
  proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init(Ref, Socket, Transport, { Module, Config }) ->
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref),
  { ok, State } = start_listener(Module, fake, Config),
  NewState = State#cl_tcp_transport{
    socket = Socket,
    transport = Transport
  },
  ok = Transport:setopts(Socket, [
    binary,
    {active, once},
    {exit_on_close, true},
    {keepalive, true}
  ]),
	gen_server:enter_loop(?MODULE, [], NewState, ?TIMEOUT).

init([]) -> {ok, undefined}.

handle_call({ send, Data }, From, State) ->
  answer_for_call(From, cl_protocol:handle_frame_out(cl_transport:module(State), Data, cl_transport:state(State), State), State);

handle_call(_, _, State) ->
  { reply, error, State, hibernate}.

handle_cast({device_update, Device}, State) ->
  try
    case cl_device:is_equal(Device, cl_transport:device(State)) of
      true -> throw({ noreply, State });
      false -> ok
    end,
    NewDevice = cl_device:merge(Device, cl_transport:device(State)),
    NewState = cl_transport:set_device(NewDevice, State),
    case cl_device:is_active(Device) of
      false -> throw(do_stop(non_active, State));
      true -> ok
    end,
    { noreply, NewState }
  catch
    throw: Reason -> Reason
  end;

handle_cast({close, Reason}, State) ->
  do_stop(Reason, State);

handle_cast(_, State) ->
  { noreply, State, hibernate}.

handle_info({tcp, _Socket, Data}, State ) ->
  NewProtocol = cl_transport:set_rest(<<(State#cl_tcp_transport.rest)/binary, Data/binary>>, State),
  Result0 = cl_protocol:handle_frame_in(cl_transport:module(NewProtocol), NewProtocol#cl_tcp_transport.rest, cl_transport:state(NewProtocol), NewProtocol),
  Result1 = answer_for_cast(Result0, NewProtocol),
  case Result1 of
    {noreply, #cl_tcp_transport{ device = Device }, _ } when Device =/= undefined ->
      calypso_traffic_hooks:receive_fire({device, Device}, Data);
    { stop, _, _ } -> ok
  end,
  inet:setopts(State#cl_tcp_transport.socket, [{active, once}]),
  Result1;

handle_info({tcp_closed,_Socket}, State) ->
  do_stop(tcp_closed, State);

handle_info({tcp_error, _, Reason}, State) ->
  do_stop({tcp_error, Reason}, State);

handle_info(timeout, State) ->
  do_stop(timeout, State);

handle_info(Msg, Protocol) ->
  answer_for_cast(cl_protocol:handle_info(cl_transport:module(Protocol), Msg, cl_transport:state(Protocol), Protocol), Protocol).

terminate(Reason, State) ->
  R = case State#cl_tcp_transport.stop_reason of
    '$undefined' -> Reason;
    Any -> Any
  end,
  cl_protocol:terminate(cl_transport:module(State), R, cl_transport:state(State), State),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_stop(Reason, State) ->
  { stop, normal, State#cl_tcp_transport{
    stop_reason = Reason
  }}.

-spec do_answer(cl_protocol:answer(), protocol()) -> { ok | { error, _ } | stop | term(), protocol()}.
do_answer({List, NewState, #cl_tcp_transport{} = NewProtocol}, _Protocol) when is_list(List) ->
  NewProt = cl_transport:set_state(NewState, NewProtocol),
  do_answer_loop(List, NewProt, []);
do_answer({Result, NewState, #cl_tcp_transport{} = NewProtocol}, _Protocol) ->
  do_answer({[Result], NewState, NewProtocol}, _Protocol).

do_answer_loop([], Protocol, Acc) -> { lists:reverse(Acc), Protocol };
do_answer_loop([Item | List], Protocol, Acc) ->
    case do_action(Item, Protocol) of
      { ok, #cl_tcp_transport{} = NewPr } ->
        do_answer_loop(List, NewPr, Acc);
      { actions, Actions,  #cl_tcp_transport{} = NewPr } when is_list(Actions) ->
        do_answer_loop(Actions ++ List, NewPr, Acc);
      { actions, Actions,  #cl_tcp_transport{} = NewPr } ->
        do_answer_loop([Actions] ++ List, NewPr, Acc);
      { stop, NewPr } ->
        do_answer_loop([], NewPr, Acc);
      { Item,  #cl_tcp_transport{} = NewPr } ->
        do_answer_loop(List, NewPr, [ Item | Acc])
    end.

do_action(ok, Protocol) ->
  { ok, Protocol };
do_action({stop, Reason}, Protocol) ->
  NewProtocol = Protocol#cl_tcp_transport{
    stop_reason = Reason
  },
  {stop, NewProtocol };
do_action({answer, Binary }, Protocol) ->
  { Result, NewProtocol0 } = cl_transport:send(Protocol, Binary),
  { actions, Result, NewProtocol0};
do_action({reply, _Reply} = Reply, Protocol) ->
  { Reply, Protocol };
do_action({upgrade, Module}, Protocol) ->
  { ok, cl_transport:upgrade(Module, Protocol) };
do_action({send, Binary}, Protocol) when is_binary(Binary) ->
  cl_transport:send(Protocol, Binary);
do_action({send, Term}, Protocol) ->
  { Reply, NewProtocol } = cl_protocol:handle_frame_out(cl_transport:module(Protocol), Term, cl_transport:state(Protocol), Protocol),
  { actions, Reply, NewProtocol };
do_action({rest, Binary}, Protocol) ->
  { ok, cl_transport:set_rest(Binary, Protocol) }.

answer_for_call(From, Result, Protocol) ->
  { Actions, NewProtocol0} = do_answer(Result, Protocol),
  NewProtocol1 = send(NewProtocol0),
  case proplists:get_value(reply, Actions, '$undefined') of
    '$undefined'-> ok;
    Reply -> gen_server:reply(From, Reply)
  end,
  case NewProtocol1#cl_tcp_transport.stop_reason of
    '$undefined' ->
      { noreply, NewProtocol1, hibernate };
    _ ->
      { stop, normal, NewProtocol1 }
  end.

answer_for_cast(Result, Protocol) ->
  { _Actions, NewProtocol0 } = do_answer(Result, Protocol),
  NewProtocol1 = send(NewProtocol0),
  case NewProtocol1#cl_tcp_transport.stop_reason of
    '$undefined' ->
      { noreply, NewProtocol1, hibernate };
    _ ->
      { stop, normal, NewProtocol1 }
  end.

send(#cl_tcp_transport{ send = Send, device = Device } = Transport) when Send =/= [] ->
  lists:foreach(fun(Binary) ->
    ok = gen_tcp:send(Transport#cl_tcp_transport.socket, Binary),
    ?INFO("Send data ~p", [ Binary ]),
    if
      Device =:= undefined -> ok;
      true ->
        calypso_traffic_hooks:send_fire({ device, Device }, Binary)
    end
  end, lists:reverse(Send)),
  cl_transport:clear_send(Transport);
 send(Transport) -> Transport.
