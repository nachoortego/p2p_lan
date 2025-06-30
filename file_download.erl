-module(file_download).
-export([ask_for_file/2]).

% Funcion llamada por la CLI, le pregunta al nodo con ID 'nodeID' si tiene el 
% archivo con nombre 'FileName'
ask_for_file(FileName, {OriginIp, OriginPort}) ->
    spawn(fun() ->
        case gen_tcp:connect(OriginIp, OriginPort, [binary, {active, false}], 5000) of
            {ok, Socket} ->
                FileNameBin = list_to_binary(FileName),
                Request = <<"DOWNLOAD_REQUEST ", FileNameBin/binary, "\n">>,
                gen_tcp:send(Socket, Request),
                receive_answer(Socket, FileName),
                gen_tcp:close(Socket);
            {error, Reason} ->
                io:format("ERROR en ask_for_file. No se pudo conectar a ~p:~p, ~p~n", [OriginIp, OriginPort, Reason])
        end
    end),
    ok.

% El nodo recibe una respuesta sobre el archivo que pidió, y actua según corresponda.
receive_answer(Socket, FileName) ->
    case gen_tcp:recv(Socket, 1, 20000) of
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
                                    ok
                            end;             
                        % Si el archivo pesa >=4MB, se manda de a chunks
                        true ->
                            case gen_tcp:recv(Socket, 4, 5000) of
                                {error, Reason} ->
                                    io:format("ERROR en receive_answer al leer ~s: ~p~n", [FileName, Reason]),
                                    ok;        
                                {ok, Bin} ->
                                    <<ChunkSize:32/big>> = Bin,
                                    save_chunks(Socket, FileName, FileSize, ChunkSize),
                                    ok
                            end
                    end
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


%% Recibe los chunks y los unifica en un file que guarda en la carpeta "Descargas"
save_chunks(Socket, FileName, FileSize, ChunkSize) ->
    Dir = "Descargas/",
    FullPath = filename:join(Dir, FileName),

    % Intenta abrir el archivo para escritura binaria
    case file:open(FullPath, [write, binary]) of
        % Si se abrío correctamente, empieza a recibir los chunks
        {ok, Fd} ->
            receive_chunks(Socket, Fd, ChunkSize, FileSize, 0),
            file:close(Fd),
            io:format("Archivo ~s descargado correctamente en ~s~n", [FileName, Dir]);
        {error, Reason} ->
            io:format("ERROR en file:open al abrir archivo ~s: ~p~n", [FileName, Reason])
    end.

receive_chunks(Socket, Fd, ChunkSize, FileSize, BytesReceived) ->
    % Intenta recibir 7 bytes: 1 byte que indica el tipo de mensaje (debe ser 111, 
    % tipo chunk), 2 bytes que indican el índice, y 4 bytes que indican el tamaño 
    % real del chunk.
    case gen_tcp:recv(Socket, 7, 5000) of
        {ok, <<111, ChunkIndex:16/big, ChunkRealSize:32/big>>} ->
            % Recibe los ChunkRealSize bytes de datos binarios del chunk
            case gen_tcp:recv(Socket, ChunkRealSize, 5000) of
                {ok, ChunkData} ->
                    ok = file:write(Fd, ChunkData),
                    NewBytesReceived = BytesReceived + ChunkRealSize,
                    %io:format("Chunk ~p recibido (~p bytes acumulados)~n", [ChunkIndex, NewBytesReceived]),
                    % Si ya se termino de recibir todo, listo. Si no, se vuelve a 
                    % llamar a si misma y sigue recibiendo lo que falte.
                    case NewBytesReceived >= FileSize of
                        true ->
                            ok;
                        false ->
                            receive_chunks(Socket, Fd, ChunkSize, FileSize, NewBytesReceived)
                    end;
                {error, Reason} ->
                    io:format("ERROR en receive_chunks en el chunk ~p: ~p~n", [ChunkIndex, Reason]),
                    ok
            end;
        {error, Reason} ->
            io:format("ERROR en receive_chunks: ~p~n", [Reason]),
            ok;
        {ok, Msg} ->
            io:format("ERROR en receive_chunks: mensaje inesperado recibido ~p~n", [Msg]),
            ok
    end.