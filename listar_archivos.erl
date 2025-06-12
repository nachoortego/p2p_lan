-module(listar_archivos).
-export([listar/1]).

listar(Directorio) ->
    case file:list_dir(Directorio) of
        {ok, Archivos} -> 
            lists:filter(fun(Archivo) -> es_archivo(Directorio, Archivo) end, Archivos);
        {error, _} -> 
            {error, "No se pudo acceder al directorio"}
    end.

es_archivo(Directorio, Archivo) ->
    case file:consult(Directorio ++ "/" ++ Archivo) of
        {ok, _} -> true; 
        {error, _} -> false
    end.