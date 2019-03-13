all: product

.PHONY: product
product: l.dko

%.dko: %.dk
	dkcheck -e $<
