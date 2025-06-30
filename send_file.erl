-module(send_file).
-include_lib("kernel/include/file.hrl").
-export([get_file_size/1, send_entire_file/3, send_file_in_chunks/3, send_file/2]).

% Obtenemos el tamano del archivo
get_file_size(FilePath) ->
    case file:read_file_info(FilePath) of
        {ok, Info} -> {ok, Info#file_info.size};
        Error -> Error
    end.

% Enviar el archivo entero
send_entire_file(Socket, FilePath, Size) ->
    file:read_file(FilePath),
    {ok, Bin} = file:read_file(FilePath),
    ok = gen_tcp:send(Socket, <<101, (Size):32/big>>),
    gen_tcp:send(Socket, Bin).

% Enviar el archivo en chunks
send_file_in_chunks(Socket, FilePath, Size) ->
    {ok, FD} = file:open(FilePath, [read, binary]),
    ChunkSize = 1048576,  % 1MB
    ok = gen_tcp:send(Socket, <<101, Size:32/big, ChunkSize:32/big>>),
    send_chunks(Socket, FD, 0, Size, ChunkSize),
    file:close(FD).

send_chunks(Socket, FD, Index, Remaining, ChunkSize) when Remaining > 0 ->
    ThisChunkSize = min(ChunkSize, Remaining),
    {ok, Bin} = file:pread(FD, Index * ChunkSize, ThisChunkSize),
    gen_tcp:send(Socket, <<111, Index:16/big, ThisChunkSize:16/big>>),
    gen_tcp:send(Socket, Bin),
    send_chunks(Socket, FD, Index + 1, Remaining - ThisChunkSize, ChunkSize);
send_chunks(_, _, _, _, _) ->
    ok.

% Funcion para el envio del archivo
send_file(Socket, FileName) ->
    Dir = "Descargas/",
    FilePath = filename:join(Dir, FileName),
    io:format("Enviando archivo: ~s~n", [FilePath]),
    case get_file_size(FilePath) of
        {ok, Size} when Size =< 4 * 1024 * 1024 ->
            io:format("Archivo chico, mandando entero (~p bytes)~n", [Size]),
            send_entire_file(Socket, FilePath, Size);
        {ok, Size} ->
            io:format("Archivo grande, mandando en chunks (~p bytes)~n", [Size]),
            send_file_in_chunks(Socket, FilePath, Size);
        Error ->
            io:format("Archivo no encontrado o error: ~p~n", [Error]),
            gen_tcp:send(Socket, <<112>>)
    end.

