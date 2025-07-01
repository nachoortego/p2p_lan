-module(nodo).
-export([init/0, get_my_ip/0]).

%% Devuelve la dirección ip
get_my_ip() ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:connect(Socket, {192,168,0,1}, 12345),
    {ok, {MyIP, _}} = inet:sockname(Socket),
    gen_udp:close(Socket),
    MyIP.

%% Genera un id único para el nodo
generate_id(Socket) ->

    %% Id aleatorio de 4 caracteres ascii
    Id = lists:map(
            fun(_) ->
                lists:nth(rand:uniform(62),
                    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
            end,
            lists:seq(1, 4)),

    %% Arma y envía el mensaje NAME_REQUEST
    Msg = "NAME_REQUEST " ++ Id ++ "\n",
    gen_udp:send(Socket, {192, 168, 0, 255}, 12346, Msg),

    StartTime = erlang:monotonic_time(millisecond),
    Result = wait_response(Socket, Id, StartTime),

    Result.

%% Espera la respuesta del NAME_REQUEST
wait_response(Socket, Id, StartTime) ->
    Timeout = 10000, %% Inicializa en 10s
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,
    Remaining = Timeout - Elapsed,
    if
        Remaining =< 0 -> %% Si pasaron 10s, retorna el Id
            Id;
        true ->
            MyIP = get_my_ip(),
            case gen_udp:recv(Socket, 0, Remaining) of
                {ok, {IP, _Port, Binary}} ->
                    case IP =:= MyIP of %% Si la ip es la del nodo, se ignora el mensaje
                        true -> 
                            wait_response(Socket, Id, StartTime); % Ignora propio mensaje
                        false -> 
                            Str = binary_to_list(Binary),
                            Tokens = string:tokens(string:trim(Str), " "),
                            case Tokens of
                                ["INVALID_NAME", Id] -> %% Si el mensaje INVALID_NAME esta dirigido a su id, espera 1s y genera otro
                                    timer:sleep(1000),
                                    generate_id(Socket);
                                ["NAME_REQUEST", Id] ->
<<<<<<< HEAD
                                    listen:send_invalid_name(Socket, IP, Id),
                                    wait_response(Socket, Id, StartTime);
                                Token ->
                                    wait_response(Socket, Id, StartTime)
=======
                                    listen:send_invalid_name(Socket, IP, Id);
                                _ ->
                                    wait_response(Socket, Id, StartTime) %% Si el mensaje no esta dirigido a su id, sigue esperando
>>>>>>> 939207fa22152b1f8046ec2bd7235aacbda730ff
                            end
                    end;

                {error, timeout} -> 
                    Id
            end
    end.

%% Envia HELLO <id_nodo> <puerto_tcp> cada 15s por el canal UDP
hello_loop(Socket, MyId) ->
    Msg = "HELLO " ++ MyId ++ " 12544\n",
    gen_udp:send(Socket, {192, 168, 0, 255}, 12346, Msg),
    timer:sleep(15000),
    hello_loop(Socket, MyId).

%% Devuelve el id del nodo
get_id(Id) ->
    receive
        {id, Pid} -> Pid ! {ok, Id}
    end,
    get_id(Id).

%% Chequea que los nodos conocidos sean correctos cada 10s
check_nodes() ->
    timer:sleep(10000),
    knownNodes ! {checkNodes},
    check_nodes().

%% Lleva el registro de un map de nodos conocidos
known_nodes(NodeMap) ->
    receive
        %% Agrega un nuevo nodo conocido
        {new, NodeId, Port, NodeIP} -> 
            getId ! {id, self()},
            receive
                {ok, NodeId} -> known_nodes(NodeMap);
                {ok, _} ->
                    %% Si está actualiza sus datos, sino lo agrega
                    case maps:is_key(NodeId, NodeMap) of 
                        true ->
                            NodeInfo = maps:get(NodeId, NodeMap),
                            UpdatedNodeInfo = maps:put(last_update, erlang:monotonic_time(millisecond), NodeInfo),
                            known_nodes(maps:put(NodeId, UpdatedNodeInfo, NodeMap));
                        false ->
                            NodeInfo = #{
                                ip => NodeIP,
                                puerto => Port,
                                last_update => erlang:monotonic_time(millisecond)
                            },
                            known_nodes(maps:put(NodeId, NodeInfo, NodeMap))
                    end
            end;
        %% Actualiza los nodos que pasaron mas de 45s desde su ultima actualizacion
        {checkNodes} ->
            Now = erlang:monotonic_time(millisecond),
            Time = 45000,
            FilteredMap = maps:filter(
                fun(_Key, NodeInfo) ->
                    Now - maps:get(last_update, NodeInfo) =< Time
                end,
                NodeMap
            ),
            known_nodes(FilteredMap);
        %% Devuelve el map de nodos conocidos
        {get, From} ->
            From ! {ok, NodeMap},
            known_nodes(NodeMap);
        %% Chequea si el nodo recibido existe en el map
        {exists, NodeId, From} ->
            case maps:is_key(NodeId, NodeMap) of
                true -> From ! {exists, NodeId};
                false -> From ! {not_exists, NodeId}
            end,
            known_nodes(NodeMap);
        _ ->
            io:format("Mensaje desconocido en known_nodes: ~p~n", [NodeMap]),
            known_nodes(NodeMap)
    end.

init() ->
    %% Abre un socket UDP en el puerto 12346 y genera su id
    {ok, Socket} = gen_udp:open(12346, [binary, {broadcast, true}, {reuseaddr, true}, {active, false}]),
    MyId = generate_id(Socket),

    %% Inicializa el proceso getId
    GetIdPid = spawn(fun() -> get_id(MyId) end),
    register(getId, GetIdPid),
    
    %% Inicializa el map de nodos conocidos
    KnownNodesPid = spawn(fun() -> known_nodes(#{}) end),
    register(knownNodes, KnownNodesPid),

    %% Consulta su id y se presenta
    getId ! {id, self()},
    receive
        {ok, Id123} ->
            io:format("Hola soy el nodo ~p~n", [Id123])
    end,

    %% Luego de 1s se inicializan las tareas
    timer:sleep(1000),
    spawn(fun() -> listen:start(Socket) end), % Recibe los HELLO y los NAME_REQUEST por UDP
    spawn(fun() -> connect:start() end), % Recibe los SEARCH_REQUEST y DOWNLOAD_REQUEST por TCP
    spawn(fun() -> hello_loop(Socket, MyId) end), % Inicializa hello_loop
    spawn(fun() -> check_nodes() end), % Inicializa check_nodes

    % gen_udp:close(Socket),

    cli:cli().
