-module(calypso_traffic_hooks).
-author("Sergey Loguntsov").

%% API
-export([
  send_fire/2, receive_fire/2,
  send_hook/2, receive_hook/2,
  delete_hooks/1
]).

send_fire(Object, Data) when is_binary(Data) ->
  calypso_hooks:run(traffic_send, {Object, Data}).

receive_fire(Object, Data) when is_binary(Data) ->
  calypso_hooks:run(traffic_receive, { Object, Data }).

send_hook(Id, Fun) ->
  calypso_hooks:add(traffic_send, Id, Fun).

receive_hook(Id, Fun) ->
  calypso_hooks:add(traffic_receive, Id, Fun).

delete_hooks(Id) ->
  calypso_hooks:delete(traffic_send, Id),
  calypso_hooks:delete(traffic_receive, Id),
  ok.