-module(cl_device_command).
-author("Sergey Loguntsov").

-include("device_command.hrl").
-include_lib("calypso_core/include/cl_device.hrl").
-include_lib("calypso_core/include/logger.hrl").

%% API
-export([
  init/0,
  new/2, new/3,
  id/1, device_id/1, command/1, module/1,
  comment/1,
  answer/1, set_answer/2,
  get_by_device/1
]).

-export([
  online_hook/1
]).

-type d_command() :: #device_command{}.
-export_type([ d_command/0 ]).

-callback send_comment(Command :: d_command()) -> { ok, binary()} | undefined.

init() ->
  calypso_online_hooks:online_hook(?MODULE, fun ?MODULE:online_hook/1),
  ok.

online_hook({device, Device}) ->
    spawn(fun() ->
      Commands = get_by_device(Device),
      ?INFO("Commands ~p", [ Commands ]),
      send_loop(Commands, Device),
      ok
    end),
    ok.

new(Id, Device, Command) when ?IS_DEVICE(Device) ->
  #device_command{
    id = Id,
    device_id = cl_device:id(Device),
    device_module = cl_device:module(Device),
    command = Command
  }.

new(Device, Command) when ?IS_DEVICE(Device) ->
  new(undefined, Device, Command).

id(DCommand) ->
  DCommand#device_command.id.

device_id(DCommand) ->
  DCommand#device_command.device_id.

command(DCommand) ->
  DCommand#device_command.command.

module(DCommand) ->
  DCommand#device_command.device_module.

comment(DCommand) ->
  Module = module(DCommand),
  Module:comment(command(DCommand)).

answer(DCommand) ->
  DCommand#device_command.answer.

set_answer(Answer, DCommand) ->
  DCommand#device_command{
    answer = Answer
  }.

get_by_device(Device) when ?IS_DEVICE(Device) ->
  case calypso_db:get(device_command, { by_device, Device }) of
    undefined -> [];
    { ok, List } when is_list(List) ->
      List
  end.

send_loop([], _Device) -> ok;
send_loop([{ _ , Command}|List], Device) ->
  case cl_transport:send(cl_transport:process(cl_device:id(Device)), command(Command)) of
    ok -> ok;
    { error, unsupported } -> ok
  end,
  ok = calypso_db:destroy(device_command, cl_device_command:id(Command)),
  send_loop(List, Device).
