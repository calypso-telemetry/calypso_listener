-module(cl_upd_transport).
-author("Sergey Loguntsov").

-include_lib("calypso_core/include/logger.hrl").
-include("udp_transport.hrl").

-behaviour(cl_transport_handler).
-export([send/2, close/2, start_listener/3, stop_listener/1, device_update/2]).

start_listener(Module, fake, Config) ->
  State = #udp_transport{
    module = Module,
    socket = fake
  },
  { ok, St, NewProt } = cl_protocol:init(Module, Config, State),
  { ok, cl_transport:set_state(St, NewProt)};
start_listener(Module, Port, Options) ->
  ?INFO("Open udp port ~p for ~p", [ Port, Module ]),
  { ok, _Pid } = cl_udp_transport_sup:add_udp(Port, Module, Options),
	cl_port:register_port({ udp, Port}, Module),
  ok.

stop_listener(fake) -> ok;
stop_listener({ udp, Port}) ->
  cl_udp_transport_sup:delete_udp(Port),
  cl_port:unregister_port({udp, Port}),
  ?INFO("Close port ~p", [ Port ]),
  ok.

close(Pid, Reason) ->
  cl_udp_device:close(Pid, Reason).

device_update(Pid, Device) ->
  cl_udp_device:device_update(Pid, Device).

send(Pid, Binary) ->
  cl_udp_device:send(Pid, Binary).

