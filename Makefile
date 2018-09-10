testbc:
	dune build src/tests/fuzzer/ropebuffer/main.bc

test:
	dune runtest

default:
	dune build @install

install: default
	dune install

clean:
	dune clean
