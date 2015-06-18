-author("Sergey Loguntsov").

-record(device, {
  protocol :: atom(), %% Module of protocol
  telemetry_bindings = undefined :: cl_binding:telemetry_binding(),
  contorol_bindings = undefined :: cl_binding:control_binding(),
  state :: term()
}).