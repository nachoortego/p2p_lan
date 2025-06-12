-module(nodo).
-export([init/1]).

get_id(Id) ->
    receive
        {id, Pid} -> Pid ! {ok, Id}
    end,
    get_id(Id).

known_nodes(NodeList) ->
    receive
        {new, NodeId} ->
            case lists:member(NodeId, NodeList) of
                true -> known_nodes(NodeList);
                false -> known_nodes([NodeId | NodeList])
            end;
        {get, NodeId} ->
            NodeId ! {ok, NodeList},
            known_nodes(NodeList)
    end.

init(Id) ->
    io:format("Hola soy el nodo ~p~n", [Id]),
    spawn(fun() -> listen:start() end),
    KnownNodesPid = spawn(fun() -> known_nodes([]) end),
    register(knownNodes, KnownNodesPid),
    
    GetIdPid = spawn(fun() -> get_id(Id) end),
    register(getId, GetIdPid),

    cli:cli().
