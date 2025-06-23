-module(nodo).
-export([init/0]).

broadcast(Port, Message) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, true}, {broadcast, true}]),
  Address = {255, 255, 255, 255}, 
  gen_udp:send(Socket, Address, Port, Message),
  gen_udp:close(Socket).

generate_id(InitPid) ->
    Id = rand:uniform(100000),
    Msg = "NAME_REQUEST " ++ integer_to_list(Id) ++ "\n",
    broadcast(12345, Msg),
    receive
        {udp, _Socket, _IP, _InPort, Binary} ->
            [Status, _] = string:tokens(Binary, " "),
            case Status of
                "INVALID_NAME" ->
                    timer:sleep(5000),
                    generate_id(InitPid);
                _ -> Id
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
    
    KnownNodesPid = spawn(fun() -> known_nodes([]) end),
    register(knownNodes, KnownNodesPid),

    cli:cli().
