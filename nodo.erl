-module(nodo).
-export([init/0]).

broadcast(Port, Message) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, true}, {broadcast, true}]),
  Address = {255, 255, 255, 255}, 
  gen_udp:send(Socket, Address, Port, Message),
  gen_udp:close(Socket).

generate_id(InitPid) ->
    Id = lists:map(
            fun(_) ->
                lists:nth(rand:uniform(62),
                    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
            end,
            lists:seq(1,4)),
    Msg = "NAME_REQUEST " ++ integer_to_list(Id) ++ "\n",
    broadcast(12346, Msg),
    receive
        {udp, _Socket, _IP, _InPort, Binary} ->
            Str = binary_to_list(Binary),
            Myname = integer_to_list(Id),
            case string:tokens(string:trim(Str), " ") of
                ["INVALID_NAME", Myname]->
                    timer:sleep(5000),
                    generate_id(InitPid);
                _ ->
                    Id
            end
    after
        3000 -> Id
    end.

hello(MyId) ->
    Msg = "HELLO " ++ integer_to_list(MyId) ++ " " ++ integer_to_list(12345) ++ "\n",
    io:format("Broadcast: ~p~n", [Msg]),
    broadcast(12332, Msg),
    timer:sleep(25000),
    hello(MyId).

get_id(Id) ->
    receive
        {id, Pid} -> Pid ! {ok, Id}
    end,
    get_id(Id).

% {
%     "idNodo": {
%         "ip": "123.123.123",
%         "puerto": "12345"
%     },
% }
known_nodes(NodeMap) ->
    receive
        {new, NodeId, Port, NodeIP} ->
            case maps:is_key(NodeId, NodeMap) of
                true -> known_nodes(NodeMap);
                false ->
                    NodeInfo = #{
                        ip => NodeIP,
                        puerto => Port
                    },
                    known_nodes(maps:put(NodeId, NodeInfo, NodeMap))
            end;
        {get, From} ->
            From ! {ok, NodeMap},
            known_nodes(NodeMap);
        {exists, NodeId, From} ->
            case maps:is_key(NodeId, NodeMap) of
                true -> From ! {exists, NodeId};
                false -> From ! {not_exists, NodeId}
            end,
            known_nodes(NodeMap)
    end.

init() ->
    MyId = generate_id(self()),
    
    GetIdPid = spawn(fun() -> get_id(MyId) end),

    register(getId, GetIdPid),
    
    getId ! {id, self()},
    
    receive
        {ok, Id123} ->
            io:format("Hola soy el nodo ~p~n", [Id123])
    end,

    spawn(fun() -> listen:start() end),
    spawn(fun() -> hello(MyId) end),
    
    KnownNodesPid = spawn(fun() -> known_nodes(#{}) end),
    register(knownNodes, KnownNodesPid),

    cli:cli().
