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

Compilación y ejecución:
```bash
make
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
| descargar            | Descarga un archivo dado el nombre del archivo y el nodo que lo tiene       |
| buscar               | Busca un archivo en la red dado su nombre                                   |
| salir                | Finaliza el nodo.                                                            |
| help                 | Muestra la lista de comandos disponibles.                                    |

Autores
-------

- Grepachok Sofia
- Lopez Agustín
- Ortego Ignacio
- Pereyra Guillermo

Licencia
--------

Uso académico. Proyecto para Sistemas Operativos I, FCEIA - UNR.
