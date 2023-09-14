package := orloca.es
version := 4.10
R := $(wildcard R/*.R)

# User targets
0: $(package).log
	@echo Test done
default: $(package)
	R --interactive --no-save -e 'library("orloca.es"); help.start()'
$(package): ~/R_LIBS/$(package)
	@echo Package updated
check: $(package).Rcheck
	@echo Check done
tests: examples.out tests.out
	@echo Test done

# Other targets
$(package).log: $(package) makefile
	date >$(package).log
	echo "library('orloca.es')" >.Rprofile
	R --interactive --no-save
	rm .Rprofile
	reset
$(package).Rcheck: $(package)_$(version).tar.gz
	R CMD check --as-cran $(package)_$(version).tar.gz
$(package)_$(version).tar.gz: $(R) $(Rmd) $(html) DESCRIPTION
	R CMD build --no-build-vignettes .
~/R_LIBS/$(package): $(package)_$(version).tar.gz
	R CMD INSTALL $(package)_$(version).tar.gz
examples.out: ~/R_LIBS/$(package)
	R -e "devtools::run_examples()" | tee examples.out
