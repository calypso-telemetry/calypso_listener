-author("Sergey Loguntsov").

-record(udp_transport, {
  module :: module(),
  state :: term(),
  socket :: port() | fake,
  event_manager :: pid(),
  stop_reason = '$undefined' :: term(),
  send = [] :: [binary()],
  device = undefined :: cl_device:device()
}).

-define(IS_UDP_TRANSPORT(Transport), is_record(Transport, udp_transport)).
