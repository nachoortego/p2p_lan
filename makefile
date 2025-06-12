# all: nodo
# 	erl -s nodo init 1

nodo: listar_archivos listen connect cli
	erlc nodo.erl

listar_archivos: listar_archivos.erl
	erlc listar_archivos.erl

listen: listen.erl
	erlc listen.erl

connect: connect.erl
	erlc connect.erl

cli: cli.erl
	erlc cli.erl

clean:
	rm -f *.beam