-author("Sergey Loguntsov").

-record(cl_tcp_transport, {
  module :: module(),
  state :: term(),
  socket :: port() | fake,
  transport :: atom(),
  rest = <<>> :: binary(),
  stop_reason = '$undefined' :: term(),
  send = [] :: [binary()],
  device = undefined :: cl_device:device()
}).

-define(IS_TCP_TRANSPORT(Transport), is_record(Transport, cl_tcp_transport)).
