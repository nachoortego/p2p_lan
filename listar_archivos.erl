-module(listar_archivos).
-export([listar/1]).

-include_lib("kernel/include/file.hrl").

%% Lista los nombres de archivos en el directorio dado.
listar(Directorio) ->
    case file:list_dir(Directorio) of
        {ok, Archivos} -> 
            lists:filter(fun(Archivo) -> es_archivo(Directorio, Archivo) end, Archivos);
        {error, _} -> 
            {error, "No se pudo acceder al directorio"}
    end.

%% Verifica si una entrada del directorio es un archivo regular.
es_archivo(Directorio, Archivo) ->
    Path = filename:join(Directorio, Archivo),
    case file:read_file_info(Path) of
        {ok, Info} ->
            case Info#file_info.type of
                regular -> true;
                _ -> false
            end;
        _ -> false
    end.