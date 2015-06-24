-module(cl_transport_handler).
-author("begemot").

-include_lib("calypso_core/include/cl_device.hrl").
-include("device_command.hrl").

%% API
-export([register/2, process/1, send/1, send/2, close/2, listen/4, device_update/2]).

-callback send(Pid, Binary) -> ok
  when
    Pid :: pid(),
    Binary :: binary().

-callback close(Pid, Reason) -> ok
  when
    Pid :: pid(),
    Reason :: term().

-callback device_update(Pid, Device) -> ok
  when
    Pid :: pid(),
    Device :: cl_device:device().

-callback start_listener(ProtocolModule, Port, Options) -> { ok, Pid } | { error, _ }
  when
    ProtocolModule :: atom(),
    Port :: integer(),
    Options :: term(),
    Pid :: pid().

-callback stop_listener(Port) -> ok
  when
    Port :: integer().

send(DCommand) when ?IS_DEVICE_COMMAND(DCommand) ->
  try
    Result = case process(cl_device_command:device_id(DCommand)) of
      undefined ->
        { error, offline };
      Process ->
        case catch send(Process, cl_device_command:command(DCommand)) of
          ok -> throw(ok);
          { error, unsupported } = Error0 -> throw(Error0);
          { error, _ } = Error0 -> Error0;
          { 'EXIT', { noproc, _ } } -> { error, offline }
        end
    end,
    case catch calypso_db:create(device_command, DCommand) of
      { ok, NewId } ->
        { saved, NewId, Result };
      { 'EXIT', { not_created , _ }} ->
        Result
    end
  catch
    throw:Reason -> Reason
  end.

send(Device, Command) when ?IS_DEVICE(Device) ->
  DCommand = cl_device_command:new(Device, Command),
  send(DCommand);

send(Process, Data) ->
  calypso_registrar:apply(Process, send, [Data]).

close(undefined, _Reason) -> ok;
close(Process, Reason) ->
  calypso_registrar:apply(Process, close, [ Reason ]).

device_update(undefined, _Reason) -> ok;
device_update(Process, Device) when ?IS_DEVICE(Device) ->
  calypso_registrar:apply(Process, device_update, [ Device ]).

listen(Module, SensorModule, Port, Config) ->
  Module:listen(SensorModule, Port, Config).

register(Id, Module) when is_atom(Module) ->
  Process = process(Id),
  Flag = case {calypso_registrar:pid(Process), self()} of
    { undefined, _ } -> true;
    {Pid, Self} when is_pid(Pid), Pid =/= Self ->
      cl_transport_handler:close(Process, double_connection),
      true;
    { Pid, Pid } ->
      false
  end,
  if
    Flag ->
      calypso_registrar:register(Module, { transport, Id});
    true ->
      ok
  end.

process(Id) ->
  calypso_registrar:process({transport, Id}).

