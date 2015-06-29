-module(calypso_traffic_hooks).
-author("Sergey Loguntsov").

%% API
-export([
  connection_open_fire/2, connection_open_hook/2,
  connection_close_fire/3, connection_close_hook/2,
  send_fire/2, receive_fire/2,
  send_hook/2, receive_hook/2,
  delete_hooks/1
]).

connection_open_fire(Protocol, Device) when is_atom(Protocol) ->
  calypso_hooks:run(connection_open, { Protocol, Device }).

connection_close_fire(Protocol, Device, Reason) when is_atom(Protocol) ->
  calypso_hooks:run(connection_close, { Protocol, Device, Reason }).

send_fire(Object, Data) when is_binary(Data) ->
  calypso_hooks:run(traffic_send, {Object, Data}).

receive_fire(Object, Data) when is_binary(Data) ->
  calypso_hooks:run(traffic_receive, { Object, Data }).

connection_open_hook(Id, Fun) ->
  calypso_hooks:add(connection_open, Id, Fun).

connection_close_hook(Id, Fun) ->
  calypso_hooks:add(connection_close, Id, Fun).

send_hook(Id, Fun) ->
  calypso_hooks:add(traffic_send, Id, Fun).

receive_hook(Id, Fun) ->
  calypso_hooks:add(traffic_receive, Id, Fun).

delete_hooks(Id) ->
  calypso_hooks:delete(connection_open, Id),
  calypso_hooks:delete(connection_close, Id),
  calypso_hooks:delete(traffic_send, Id),
  calypso_hooks:delete(traffic_receive, Id),
  ok.
