# all: nodo
# 	erl -s nodo init 1

run: nodo
	erl -noshell -s nodo init -s init stop

nodo: listar_archivos listen connect cli file_download send_file
	erlc nodo.erl

listar_archivos: listar_archivos.erl
	erlc listar_archivos.erl

listen: listen.erl
	erlc listen.erl

connect: connect.erl
	erlc connect.erl

cli: cli.erl
	erlc cli.erl

file_download: file_download.erl
	erlc file_download.erl

send_file: send_file.erl
	erlc send_file.erl

clean:
	rm -f *.beam