# Current version number of dkmeta.
VERSION = 0.1

# Compile with "make Q=" to display the commands that are run.
Q = @

.PHONY: all
all: bin binary

.PHONY: binary
binary: dkmeta.native

%.native:
	@ln -fs _build/install/default/bin/$* $@

.PHONY: bin
bin:
	@dune build

.PHONY: doc
doc:
	@dune build @doc

.PHONY: clean
clean:
	@dune clean

.PHONY: install
install: all
	@dune install
