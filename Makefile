# Current version number of Dedukti.
VERSION = devel

# Compile with "make Q=" to display the commands that are run.
Q = @

all: dkmeta.native

#### Compilation of the dedukti suite ########################################

.PHONY: commands
commands: dkmeta.native

dkmeta.native: $(wildcard src/*.ml src/*.mli)
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -package dedukti -use-ocamlfind src/dkmeta.native

clean:
	$(Q)ocamlbuild -quiet -clean

distclean: clean
	$(Q)find -name "*~" -exec rm {} \;
	$(Q)rm -f kernel/version.ml
	$(Q)rm -f META
