-module(calypso_listener).

-include("logger.hrl").

%% API
-export([
  start/3, stop/1,
  register_protocol/1, unregister_protocol/1, protocols/0
]).

start(ProtocolModule, Port,Options) when is_atom(ProtocolModule) ->
  ensure_protocol(ProtocolModule),
  ok = cl_protocol:start(ProtocolModule, Port, Options),
  ok.
stop(fake) -> ok;
stop(Port = { _Transport, _ }) ->
  {ok, ProtocolModule } = cl_port:port_protocol(Port),
  cl_protocol:stop(ProtocolModule, Port),
  cl_port:unregister_port(Port),
  ok = cl_protocol:stop(ProtocolModule, Port),
  ok.

register_protocol(ProtocolApp) when is_atom(ProtocolApp) ->
  calypso_registrar:local_register({ ?MODULE, protocols }, ProtocolApp),
  ok.

unregister_protocol(ProtocolApp) when is_atom(ProtocolApp) ->
  application:stop(ProtocolApp).

protocols() ->
  [ Module || { _, Module} <- calypso_registrar:local_lookup({ ?MODULE, protocols }) ].

ensure_protocol(ProtocolModule) ->
  case lists:member(ProtocolModule, protocols()) of
    true -> ok;
    false ->
      ?CRITICAL("Protocol ~p can't register.", [ ProtocolModule ]),
      error({not_registered, ProtocolModule})
  end.

