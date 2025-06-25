-module(download_file).
-export([ask_for_file/2]).

% Funcion llamada por la CLI, le pregunta al nodo con ID
% 'nodeID' si tiene el archivo con nombre 'FileName'
ask_for_file(FileName, NodeID) ->
    % ...
    receive_answer(Socket, FileName),
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
                                    receive_and_save_chunks(Socket, FileName, FileSize, ChunkSize),
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
receive_and_save_chunks(Socket, FileName, FileSize, ChunkSize) ->
    ok.