default:
	dune build @install

install: default
	dune install

clean:
	dune clean
