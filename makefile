# all: nodo
# 	erl -s nodo init 1

nodo: listar_archivos sv
	erlc nodo.erl

listar_archivos: listar_archivos.erl
	erlc listar_archivos.erl

sv: sv.erl
	erlc sv.erl

clean:
	rm -f *.beam