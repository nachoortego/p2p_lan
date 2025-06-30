Sistema P2P Simple para Compartir Archivos en LAN
=================================================

Descripción General
-------------------

Este proyecto es una implementación en Erlang de un sistema de archivos distribuido P2P para una red LAN, desarrollado como parte del Trabajo Práctico Final de Sistemas Operativos I (FCEIA - UNR).

Cada nodo en la red actúa como cliente y servidor, permitiendo compartir y descargar archivos directamente entre pares. El sistema implementa descubrimiento de nodos, asignación de identificadores únicos, búsqueda distribuida y transferencia robusta de archivos usando TCP y UDP.

Instrucciones de Uso
--------------------

Requisitos:
- Erlang instalado (`erlc`, `erl`)
- Puerto UDP 12346 abierto (para mensajes de descubrimiento)
- Puerto TCP configurable por nodo

Compilación:
```bash
    make
```

Ejecución:
```bash
    make run
```
Esto lanza el nodo en modo sin consola (`-noshell`) ejecutando `nodo:init/0`.

Carpetas esperadas:
- `Compartidos/`: archivos compartidos por este nodo.
- `Descargas/`: archivos descargados desde otros nodos.

CLI – Comandos Disponibles
--------------------------

| Comando               | Descripción                                                                 |
|-----------------------|-----------------------------------------------------------------------------|
| id_nodo              | Muestra el identificador del nodo.                                          |
| listar_descargas     | Muestra los archivos descargados.                                           |
| listar_compartidos   | Muestra los archivos disponibles para compartir.                            |
| nodos_conocidos      | Lista los nodos descubiertos por la red con su IP y puerto TCP.             |
| pedir_archivo        | Realiza un broadcast para buscar un archivo por nombre e ID de nodo.        |
| salir                | Finaliza el nodo.                                                            |
| help                 | Muestra la lista de comandos disponibles.                                    |

Comunicación Entre Nodos
-------------------------

Mensajes UDP:
- `HELLO <id_nodo> <puerto_tcp>`
- `NAME_REQUEST <id_nodo>`
- `INVALID_NAME <id_nodo>`

Mensajes TCP:
- `SEARCH_REQUEST <id_origen> <nombre_archivo>`
- `SEARCH_RESPONSE <id_origen> <archivo> <tamaño>`
- `DOWNLOAD_REQUEST <archivo>`
- `CHUNKED FILE (con códigos 101 y 111, y datos binarios)`

Estructura del Proyecto
-----------------------

- `nodo.erl`: Módulo principal, inicia todos los procesos.
- `listen.erl`: Servidor TCP concurrente para aceptar conexiones entrantes.
- `connect.erl`: Cliente TCP para búsquedas y descargas.
- `cli.erl`: Proceso interactivo para la línea de comandos.
- `request.erl`: Manejador de peticiones.
- `listar_archivos.erl`: Utilidades para gestionar archivos locales.
- `udp_broadcast.erl`: Maneja la lógica de envío y recepción de mensajes UDP.

Diseño de Concurrencia
----------------------

Erlang permite un modelo de concurrencia basado en actores (lightweight processes), donde:
- Cada componente del nodo (CLI, servidor TCP, discovery UDP, etc.) es un proceso independiente.
- Se usan `spawn`, `receive`, y paso de mensajes para sincronización.
- No se requieren locks ni condiciones, evitando deadlocks.

Robustez y Manejo de Errores
----------------------------

- Timeouts: Se implementan `after` para evitar bloqueos indefinidos.
- Reintentos: Se reintentan IDs en caso de conflicto.
- Desconexión: Se eliminan nodos inactivos si no se recibe HELLO en 45s.
- Transferencia Segura: Manejo de chunks para archivos grandes (>4MB).


Compilación y Pruebas
---------------------

Puede probarse en al menos dos terminales diferentes con:
```bash
    make run
```

Y en cada CLI asignar un ID distinto para los nodos, luego verificar nodos conocidos, búsqueda y descarga.

Autores
-------

- Grepachok Sofia
- Lopez Agustín
- Ortego Ignacio
- Pereyra Guillermo

Licencia
--------

Uso académico. Proyecto para Sistemas Operativos I, FCEIA - UNR.
