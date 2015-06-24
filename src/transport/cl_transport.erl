-module(cl_transport).
-author("Sergey Loguntsov").

-include("tcp/tcp_transport.hrl").
-include("udp/udp_transport.hrl").

-include_lib("calypso_core/include/cl_device.hrl").
-include_lib("calypso_core/include/cl_telemetry.hrl").
-include_lib("calypso_core/include/logger.hrl").

%% API
-export([
  register/2,
  send/2, get_send/1, clear_send/1,
  state/1, set_state/2,
  set_rest/2,
  module/1, upgrade/2,
  set_device_login/2, device_id/1, device/1, set_device/2, set_telemetry/2, set_data/3, change_device/2
]).

-type protocol() :: cl_tcp_transport:protocol().

register(Id, #cl_tcp_transport{}) ->
  cl_transport_handler:register(Id, cl_tcp_transport);

register(Id, #udp_transport{}) ->
  cl_transport_handler:register(Id, cl_udp_transport).

-spec send(protocol(), binary()) -> { ok |  {error, Reason :: term()}, cl_tcp_transport:protocol()}.
send(#cl_tcp_transport{ send = Send } = Transport, Binary) ->
  { ok, Transport#cl_tcp_transport{
    send = [ Binary | Send ]
  }};
send(Transport = #udp_transport{}, _) ->
  { { error, unsupported }, Transport }.

get_send(#cl_tcp_transport{ send = Send }) -> lists:reverse(Send);
get_send(#udp_transport{}) -> [].

clear_send(#cl_tcp_transport{} = Transport) ->
  Transport#cl_tcp_transport{ send = []};
clear_send(T = #udp_transport{}) -> T.

-spec state(protocol()) -> term().
state(Transport) when ?IS_TCP_TRANSPORT(Transport) -> Transport#cl_tcp_transport.state;
state(Transport) when ?IS_UDP_TRANSPORT(Transport) -> Transport#udp_transport.state.

-spec set_state(term(), protocol()) -> protocol().
set_state(State, Transport) when ?IS_TCP_TRANSPORT(Transport) ->
  Transport#cl_tcp_transport{
    state = State
  };
set_state(State, Transport) when ?IS_UDP_TRANSPORT(Transport) ->
  Transport#udp_transport{
    state = State
  }.

set_rest(Rest, Transport) when ?IS_TCP_TRANSPORT(Transport), is_binary(Rest) ->
  Transport#cl_tcp_transport{
    rest = Rest
  };
set_rest(_, Transport) when ?IS_UDP_TRANSPORT(Transport) -> Transport.

module(Protocol) when ?IS_TCP_TRANSPORT(Protocol) -> Protocol#cl_tcp_transport.module;
module(Protocol) when ?IS_UDP_TRANSPORT(Protocol) -> Protocol#udp_transport.module.

upgrade(Module, Protocol) when ?IS_TCP_TRANSPORT(Protocol), is_atom(Module) ->
  Protocol#cl_tcp_transport{ module = Module };
upgrade(Module, Protocol) when ?IS_UDP_TRANSPORT(Protocol), is_atom(Module) ->
  Protocol#udp_transport{ module = Module }.

set_device_login(Login, Protocol) when ?IS_TCP_TRANSPORT(Protocol) ->
  case calypso_db:get(device, { by_login, Login}) of
    { ok, Device = #device{} } ->
      case cl_device:is_active(Device) of
        true ->
          calypso_online_hooks:fire_online({ device, Device}),
          cl_transport:register(cl_device:id(Device), Protocol),
          { ok, Protocol#cl_tcp_transport{ device = Device}, Device};
        false ->
          { error, is_not_active }
      end;
    undefined -> undefined
  end.

device_id(Protocol) ->
  case device(Protocol) of
    undefined -> undefined;
    { ok, Sensor } -> cl_device:id(Sensor)
  end.

device(Protocol) when ?IS_TCP_TRANSPORT(Protocol) ->
  case Protocol#cl_tcp_transport.device of
    undefined -> undefined;
    Device when ?IS_DEVICE(Device) -> { ok, Device }
  end;
device(Protocol) when ?IS_UDP_TRANSPORT(Protocol) ->
  case Protocol#udp_transport.device of
    undefined -> undefined;
    Device when ?IS_DEVICE(Device) -> { ok, Device }
  end.

set_device(DeviceNew, Protocol = #cl_tcp_transport{ device = DeviceOld }) when ?IS_DEVICE(DeviceNew) ->
  cl_transport_hooks:device_change_fire(DeviceOld, DeviceNew),
  Protocol#cl_tcp_transport{ device = DeviceNew };
set_device(DeviceNew, Protocol = #udp_transport{ device = DeviceOld }) when ?IS_DEVICE(DeviceNew) ->
  cl_transport_hooks:device_change_fire(DeviceOld, DeviceNew),
  Protocol#udp_transport{ device = DeviceNew }.

set_telemetry(_, Protocol = #cl_tcp_transport{ device = undefined}) ->
  error(device_not_authorise, [ Protocol ]);
set_telemetry(Telemetry, Protocol = #cl_tcp_transport{ device = Device}) when ?IS_DEVICE(Device), ?IS_TELEMETRY(Telemetry) ->
  set_device(cl_device:telemetry(Telemetry, Device), Protocol).

set_data(Time, Data, Transport) ->
  T = cl_telemetry:new(Time, Data),
  set_telemetry(T, Transport).

change_device(Fun, Protocol) ->
  { ok, Device } = cl_transport:device(Protocol),
  NewDevice = Fun(Device),
  cl_transport:set_device(NewDevice, Protocol).