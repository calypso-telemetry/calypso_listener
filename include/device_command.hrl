-author("Sergey Loguntsov").

-record(device_command, {
  id :: binary(),
  device_id :: cl_device:id(),
  device_module :: atom(),
  command :: term(),
  answer = undefined :: term()
}).

-define(IS_DEVICE_COMMAND(DCommand), is_record(DCommand, device_command)).