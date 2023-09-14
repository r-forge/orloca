package := orloca
version := 5.5
R := $(wildcard R/*.R)
Rmd := $(wildcard vignettes/*.Rmd)
test_FILES := $(wildcard tests/testthat/*.R)

html := $(patsubst vignettes/%.Rmd,inst/doc/%.html,$(Rmd))

# User targets
0: $(package).log
	@echo Test done
default: $(package)
	R --interactive --no-save -e 'library("orloca"); help.start()'
$(package): ~/R_LIBS/$(package)
	@echo Package updated
check: $(package).Rcheck
	@echo Check done
tests: examples.out tests.out
	@echo Test done
vignettes: $(R) $(Rmd)
	R -e "devtools::build_vignettes('.', clean = FALSE, install = FALSE)"

# Other targets
$(package).log: $(package) makefile
	date >$(package).log
	echo "library('orloca')" >.Rprofile
	R --interactive --no-save
	rm .Rprofile
	reset
$(package).Rcheck: $(package)_$(version).tar.gz
	R CMD check --as-cran $(package)_$(version).tar.gz
$(package)_$(version).tar.gz: $(R) $(Rmd) $(html) ChangeLog DESCRIPTION inst/po/es/LC_MESSAGES/R-$(package).mo po/R-$(package)-es.po po/R-$(package).pot 
	R CMD build --no-build-vignettes .
~/R_LIBS/$(package): $(package)_$(version).tar.gz
	R CMD INSTALL $(package)_$(version).tar.gz
DESCRIPTION: $(R)
	R -e "roxygen2::roxygenize('.')"
inst/po/es/LC_MESSAGES/R-$(package).mo: po/R-$(package)-es.po
	msgfmt -c -o inst/po/es/LC_MESSAGES/R-$(package).mo po/R-$(package)-es.po
po/R-$(package)-es.po: $(R) po/R-$(package).pot
	poedit po/R-$(package)-es.po
po/R-$(package).pot: $(R)
	# This target builds the pot file and update any R-$(package)-lang.po file under po dir
	R -e "library('tools'); update_pkg_po('.')"
tests.out: $(test_FILES) ~/R_LIBS/$(package)
	R -e "devtools::test()" | tee tests.out
examples.out: ~/R_LIBS/$(package)
	R -e "devtools::run_examples()" | tee examples.out
doc/%.html: $(R) $(Rmd)
	make vignettes
inst/doc/%.html: doc/%.html
	cp --force $< $@
