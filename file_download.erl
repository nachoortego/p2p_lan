-module(download_file).
-export([ask_for_file/2]).

% Funcion llamada por la CLI, le pregunta al nodo con ID
% 'nodeID' si tiene el archivo con nombre 'FileName'
ask_for_file(FileName, {OriginIp, OriginPort}) ->
    spawn(fun() ->
        case gen_tcp:connect(OriginIp, OriginPort, [binary, {active, false}], 5000) of
            {ok, Socket} ->
                Request = <<"DOWNLOAD_REQUEST ", FileName/binary, "\n">>,
                gen_tcp:send(Socket, Request),
                receive_answer(Socket, FileName),
                gen_tcp:close(Socket);
            {error, Reason} ->
                io:format("ERROR en ask_for_file. No se pudo conectar a ~p:~p, ~p~n", [OriginIp, OriginPort, Reason])
        end
    end),
    ok.


% El nodo recibe una respuesta sobre el archivo que pidió, 
% y actua según corresponda.
receive_answer(Socket, FileName) ->
    case gen_tcp:recv(Socket, 1, 5000) of
        % ERROR, pasaron 5seg y no se encontró nada
        {error, Reason} ->
            io:format("ERROR en receive_answer: ~p~n", [Reason]),
            ok;
        
        % NOTFOUND, el nodo origen no encontró el archivo pedido
        {ok, <<112>>} ->
            io:format("Archivo ~s no encontrado,~n", [FileName]),
            ok;
        
        % OK, el nodo origen tiene el archivo y lo mandó
        {ok, <<101>>} ->
            %% Primero guardamos el tamaño del archivo
            case gen_tcp:recv(Socket, 4, 5000) of
                {error, Reason} ->
                    io:format("ERROR en receive_answer al leer ~s: ~p~n", [FileName, Reason]),
                    ok;

                {ok, <<FileSize:32/big>>} ->
                    if
                        % Si el archivo pesa <4MB, se manda todo de una
                        FileSize < 4 * 1024 * 1024 ->  % 4 MB = 4194304 bytes
                            case gen_tcp:recv(Socket, FileSize, 5000) of
                                {error, Reason} ->
                                    io:format("ERROR en receive_answer al leer ~s: ~p~n", [FileName, Reason]),
                                    ok;        
                                {ok, Bin} ->
                                    save_single_file(FileName, Bin),
                                    ok;              
                        % Si el archivo pesa >=4MB, se manda de a chunks
                        true ->
                            case gen_tcp:recv(Socket, 4, 5000) of
                                {error, Reason} ->
                                    io:format("ERROR en receive_answer al leer ~s: ~p~n", [FileName, Reason]),
                                    ok;        
                                {ok, ChunkSize:32/big} ->
                                    save_chunks(Socket, FileName, FileSize, ChunkSize),
                                    ok; 
                    end
    end.


%% Arma el file con el binario dado en la carpeta "Descargas"
save_single_file(FileName, Bin) ->
    Dir = "Descargas/",
    FullPath = filename:join(Dir, FileName),

    case file:open(FullPath, [write, binary]) of
        {ok, Fd} ->
            ok = file:write(Fd, Bin),
            ok = file:close(Fd),
            io:format("Archivo ~s guardado correctamente en ~s~n", [FileName, Dir]);

        {error, Reason} ->
            io:format("ERROR en save_single_file al guardar ~s: ~p~n", [FileName, Reason])
    end.


%% Recibe los chunks, y los unifica en un solo file
save_chunks(Socket, FileName, FileSize, ChunkSize) ->
    Dir = "Descargas/",
    FullPath = filename:join(Dir, FileName),

    % Intenta abrir el archivo para escritura binaria
    case file:open(FullPath, [write, binary]) of
        % Si se abrío correctamente, empieza a recibir los chunks
        {ok, Fd} ->
            receive_chunks(Socket, Fd, ChunkSize),
            file:close(Fd),
            io:format("Archivo ~s descargado correctamente en ~s~n", [FileName, Dir]);
        {error, Reason} ->
            io:format("ERROR en file:open al abrir archivo ~s: ~p~n", [FileName, Reason])
    end.


receive_chunks(Socket, Fd, ChunkSize) ->
    % Intenta recibir 5 bytes: 1 byte de tipo (debe ser 111), 2 bytes que indican el índice, y 2 bytes que indican el tamaño real del chunk
    case gen_tcp:recv(Socket, 5, 5000) of
        {ok, <<111, ChunkIndex:16/big, ChunkLen:16/big>>} ->
            % Recibe los ChunkLen bytes de datos binarios del chunk
            case gen_tcp:recv(Socket, ChunkLen, 5000) of
                {ok, ChunkData} ->
                    ok = file:write(Fd, ChunkData),
                    % Si el chunk recibido es más chico que ChunkSize, es el último y ya terminó. Si no, sigue recibiendo.
                    case ChunkLen < ChunkSize of
                        true ->
                            ok;
                        false ->
                            receive_chunks(Socket, Fd, ChunkSize)
                    end;
                {error, Reason} ->
                    io:format("ERROR en receive_chunks en el chunk ~p: ~p~n", [ChunkIndex, Reason]),
                    ok
            end;
        {error, Reason} ->
            io:format("ERROR en receive_chunks: ~p~n", [Reason]),
            ok
    end.