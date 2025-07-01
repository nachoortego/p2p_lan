-module(listar_archivos).
-export([listar/1]).
-include_lib("kernel/include/file.hrl").

listar(Directorio) ->
    case file:list_dir(Directorio) of
        {ok, Archivos} -> 
            lists:filter(fun(Archivo) -> es_archivo(Directorio, Archivo) end, Archivos);
        {error, _} -> 
            {error, "No se pudo acceder al directorio"}
    end.

es_archivo(Directorio, Archivo) ->
    case file:read_file_info(filename:join(Directorio, Archivo)) of
        {ok, #file_info{type = regular}} -> true;
        _ -> false
    end.
