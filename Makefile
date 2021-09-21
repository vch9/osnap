all: build

build:
	@dune build

test:
	@dune runtest

coverage:
	@dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html
	firefox _coverage/index.html

clean:
	@dune clean
	rm -rf _coverage
