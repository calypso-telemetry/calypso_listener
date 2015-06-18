-module(cl_transport_hooks).
-author("Sergey Loguntsov").

-include_lib("calypso_core/include/cl_device.hrl").

%% API
-export([
  device_change_fire/2, device_change_hook/2,
  delete_hooks/1
]).

device_change_fire(DeviceOld, DeviceNew) when ?IS_DEVICE(DeviceOld), ?IS_DEVICE(DeviceNew) ->
  true = cl_device:is_same(DeviceOld, DeviceNew),
  case cl_device:is_equal(DeviceOld, DeviceNew) of
    true -> ok;
    false ->
      calypso_hooks:run(device_change, { DeviceOld, DeviceNew})
  end;
device_change_fire(_, _) -> ok.

device_change_hook(Id, Fun) ->
  calypso_hooks:add(device_change, Id, Fun).

delete_hooks(Id) ->
  calypso_hooks:delete(device_change, Id),
  ok.
