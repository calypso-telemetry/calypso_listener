-author("begemot").

-record(tcp_transport, {
  module :: module(),
  state :: term(),
  socket :: port() | fake,
  transport :: atom(),
  rest = <<>> :: binary(),
  event_manager :: pid(),
  stop_reason = '$undefined' :: term(),
  send = [] :: [binary()],
  device = undefined :: cl_device:device()
}).
