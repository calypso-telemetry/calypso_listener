-author("Sergey Loguntsov").

-record(object, {
  id :: term(),
  devices :: [cl_device:id()],
  state :: term(),
  control_binding :: cl_object:control_binding(),
  telemetry_binding :: cl_object:telemetry_binding()
}).
