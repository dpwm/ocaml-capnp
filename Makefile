test:
	dune runtest -f

default:
	dune build @install

install: default
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
