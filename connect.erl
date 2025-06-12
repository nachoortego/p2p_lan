-module(connect).
-export([start_connection/1]).

start_connection(NodeId) ->
    KnownNodesPid ! {new, NodeId},
    % Aquí iría la lógica para iniciar la conexión
    ok.
