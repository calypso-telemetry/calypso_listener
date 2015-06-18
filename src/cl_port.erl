-module(cl_port).
-author("begemot").

%% API
-export([
  register_port/2, unregister_port/1, ports/1, port_protocol/1
]).

register_port(Port = { Transport, _ }, ProtocolModule) when is_atom(ProtocolModule) ->
  [] = cl_registrar_storage:lookup({?MODULE, port, Port}),
  cl_registrar_storage:insert({?MODULE, port, Port}, ProtocolModule),
  cl_registrar_storage:insert({?MODULE, Transport, ports}, { Port, ProtocolModule }),
  ok.

unregister_port(Port = { Transport, _ }) ->
  { ok, ProtocolModule } = port_protocol(Port),
  cl_registrar_storage:delete({?MODULE, port, Port}, ProtocolModule),
  cl_registrar_storage:delete({?MODULE, Transport, ports}, { Port, ProtocolModule }),
  ok.

port_protocol(Port = { _Transport, _ }) ->
  case cl_registrar_storage:lookup({?MODULE, port, Port}) of
    [ProtocolModule] -> { ok, ProtocolModule };
    [] -> undefined
  end.

-spec ports(atom()) -> [{ Port :: pos_integer(), Module :: atom()}].
ports(Transport) ->
  cl_registrar_storage:lookup({?MODULE, Transport, ports}).
