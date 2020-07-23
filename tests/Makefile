DIR=product

all: product

.PHONY: product
product: $(DIR)/result.dko

%.dko: %.dk
	dkcheck -e -I $(DIR) $<

$(DIR)/result.dk: $(DIR)/file.dk $(DIR)/meta.dk $(DIR)/l.dko
	cd $(DIR) && ../../dkmeta.native -I $(DIR) -m meta.dk --quoting prod file.dk > result.dk

.PHONY: clean
clean:
	cd $(DIR) && rm -f result.dk result.dko l.dko
