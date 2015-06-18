-module(cl_tcp).
-author("Sergey Loguntsov").

-include("tcp_transport.hrl").
-include_lib("calypso_core/include/cl_device.hrl").
-include_lib("calypso_core/include/cl_telemetry.hrl").
-include_lib("calypso_core/include/logger.hrl").

%% API
-export([
  register/1,
  send/2, get_send/1, clear_send/1,
  state/1, set_state/2,
  set_rest/2,
  module/1, upgrade/2,
  set_device_login/2, device_id/1, device/1, set_device/2, set_telemetry/2, set_data/3, change_device/2
]).

-type protocol() :: cl_tcp_transport:protocol().

register(Id) ->
  cl_transport:register(Id, cl_tcp_transport).

-spec send(protocol(), binary()) -> { ok |  {error, Reason :: term()}, cl_tcp_transport:protocol()}.
send(#tcp_transport{ send = Send } = Transport, Binary) ->
  { ok, Transport#tcp_transport{
    send = [ Binary | Send ]
  }}.

get_send(#tcp_transport{ send = Send }) -> lists:reverse(Send).

clear_send(#tcp_transport{} = Transport) ->
  Transport#tcp_transport{ send = []}.

-spec state(protocol()) -> term().
state(Transport) -> Transport#tcp_transport.state.

-spec set_state(term(), protocol()) -> protocol().
set_state(State, Transport) ->
  Transport#tcp_transport{
    state = State
  }.

set_rest(Rest, Transport) when is_binary(Rest) ->
  Transport#tcp_transport{
    rest = Rest
  }.

module(Protocol) ->
  Protocol#tcp_transport.module.

upgrade(Module, Protocol) when is_atom(Module) ->
  Protocol#tcp_transport{
    module = Module
  }.

set_device_login(Login, Protocol) ->
  case calypso_db:get(device, { by_login, Login}) of
    { ok, Device = #device{} } ->
      case cl_device:is_active(Device) of
        true ->
          calypso_online_hooks:fire_online({ device, Device}),
          cl_tcp:register(cl_device:id(Device)),
          { ok, Protocol#tcp_transport{ device = Device}, Device};
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

device(Protocol) ->
  case Protocol#tcp_transport.device of
    undefined -> undefined;
    Device when ?IS_DEVICE(Device) -> { ok, Device}
  end.

set_device(DeviceNew, Protocol = #tcp_transport{ device = DeviceOld }) when ?IS_DEVICE(DeviceNew) ->
  cl_transport_hooks:device_change_fire(DeviceOld, DeviceNew),
  Protocol#tcp_transport{ device = DeviceNew}.

set_telemetry(_, Protocol = #tcp_transport{ device = undefined}) ->
  error(device_not_authorise, [ Protocol ]);
set_telemetry(Telemetry, Protocol = #tcp_transport{ device = Device}) when ?IS_DEVICE(Device), ?IS_TELEMETRY(Telemetry) ->
  set_device(cl_device:telemetry(Telemetry, Device), Protocol).

set_data(Time, Data, Transport) ->
  T = cl_telemetry:new(Time, Data),
  set_telemetry(T, Transport).

change_device(Fun, Protocol) ->
  { ok, Device } = cl_tcp:device(Protocol),
  NewDevice = Fun(Device),
  cl_tcp:set_device(NewDevice, Protocol).