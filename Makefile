# Current version number of dkmeta.
VERSION = 0.1

# Compile with "make Q=" to display the commands that are run.
Q = @

all: dkmeta.native dkmeta

#### Compilation of the kernel library #######################################

DKMETA_MLI := $(wildcard src/*.mli)
DKMETA_ML  := $(DKMETA_MLI:.mli=.ml)

.PHONY: dkmeta
dkmeta: _build/src/dkmeta.cma _build/src/dkmeta.cmxa

dkmeta/version.ml: GNUmakefile
	@echo "[GEN] $@ ($(VERSION))"
	$(Q)echo 'let version = "$(VERSION)"' > $@

_build/src/dkmeta.cma: $(DKMETA_MLI) $(DKMETA_ML)
	@echo "[BYT] $@"
	$(Q)ocamlbuild -quiet -package dedukti -use-ocamlfind src/dkmeta.cma

_build/src/dkmeta.cmxa: $(DKMETA_MLI) $(DKMETA_ML)
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -package dedukti -use-ocamlfind src/dkmeta.cmxa

#### Compilation of dkmeta ########################################

.PHONY:
commands: dkmeta.native

dkmeta.native: $(wildcard src/*.ml src/*.mli)
	@echo "[OPT] $@"
	$(Q)ocamlbuild -quiet -package dedukti -use-ocamlfind src/dkmeta.native

clean:
	$(Q)ocamlbuild -quiet -clean

distclean: clean
	$(Q)find -name "*~" -exec rm {} \;
	$(Q)rm -f META

META: Makefile
	@echo "[GEN] $@"
	@echo 'name = "dkmeta"'                                                 > META
	@echo 'version = "$(VERSION)"'                                         >> META
	@echo 'description = "A wrapper around the rewrite engine of Dedukti"' >> META
	@echo 'requires = "dedukti"'                                           >> META
	@echo 'archive(byte) = "dkmeta.cma"'                                   >> META
	@echo 'archive(native) = "dkmeta.cmxa"'                                >> META

BINDIR = $(dir $(shell which ocaml))

.PHONY: uninstall
uninstall:
	@ocamlfind remove dkmeta
	@rm -f $(BINDIR)/dkmeta

.PHONY: install
install: META uninstall all
	@ocamlfind install dkmeta META \
		$(wildcard _build/src/*.mli) $(wildcard _build/src/*.cmi) \
		$(wildcard _build/src/*.cmx) $(wildcard _build/src/*.o) \
		_build/src/dkmeta.cma  \
		_build/src/dkmeta.cmxa \
		_build/src/dkmeta.a
	install -m 755 -d $(BINDIR)
	install -m 755 -p dkmeta.native  $(BINDIR)dkmeta
