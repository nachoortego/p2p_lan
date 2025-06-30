-module(nodo).
-export([init/0]).

broadcast(Port, Message) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active, true}, {broadcast, true}]),
  Address = {255, 255, 255, 255}, 
  gen_udp:send(Socket, Address, Port, Message),
  gen_udp:close(Socket).

generate_id() ->
    Id = lists:map(
            fun(_) ->
                lists:nth(rand:uniform(62),
                    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
            end,
            lists:seq(1, 4)),
    Msg = "NAME_REQUEST " ++ Id ++ "\n",
    io:format("Broadcast: ~p~n", [Msg]),

    {ok, Socket} = gen_udp:open(0, [binary, {active, true}, {broadcast, true}]),
    gen_udp:send(Socket, {255, 255, 255, 255}, 12346, Msg),

    StartTime = erlang:monotonic_time(millisecond),
    Result = wait_response(Socket, Id, StartTime),
    gen_udp:close(Socket),
    Result.

wait_response(Socket, Id, StartTime) ->
    Timeout = 3000,
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,
    Remaining = Timeout - Elapsed,
    if
        Remaining =< 0 ->
            io:format("Tiempo agotado, usando ID: ~p~n", [Id]),
            Id;
        true ->
            receive
                {udp, Socket, _IP, _Port, Binary} ->
                    Str = binary_to_list(Binary),
                    Tokens = string:tokens(string:trim(Str), " "),
                    case Tokens of
                        ["INVALID_NAME", Id] ->
                            io:format("Nombre inválido detectado: ~p~n", [Id]),
                            timer:sleep(5000),
                            generate_id(); % reinicia con otro ID
                        _ ->
                            % ignorar si es un NAME_REQUEST propio u otro mensaje
                            wait_response(Socket, Id, StartTime)
                    end
            after Remaining ->
                io:format("No se recibió INVALID_NAME, usando ID: ~p~n", [Id]),
                Id
            end
    end.

hello_loop(MyId) ->
    Msg = "HELLO " ++ MyId ++ " 12543\n",
    io:format("Broadcast: ~p~n", [Msg]),
    broadcast(12346, Msg),
    timer:sleep(25000),
    hello_loop(MyId).

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
    MyId = generate_id(),
    
    GetIdPid = spawn(fun() -> get_id(MyId) end),
    register(getId, GetIdPid),
    
    getId ! {id, self()},
    
    receive
        {ok, Id123} ->
            io:format("Hola soy el nodo ~p~n", [Id123])
    end,

    spawn(fun() -> listen:start() end),
    spawn(fun() -> hello_loop(MyId) end),
    
    KnownNodesPid = spawn(fun() -> known_nodes(#{}) end),
    register(knownNodes, KnownNodesPid),

    cli:cli().
