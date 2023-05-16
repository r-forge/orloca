package := orloca
version := 5.3
R := $(wildcard R/*.R)
Rmd := $(wildcard vignettes/*.Rmd)
test_FILES := $(wildcard tests/testthat/*.R)

html := $(patsubst vignettes/%.Rmd,inst/doc/%.html,$(Rmd))

inst/po/es/LC_MESSAGES/R-$(package).mo: po/R-$(package)-es.po
	msgfmt -c -o inst/po/es/LC_MESSAGES/R-$(package).mo po/R-$(package)-es.po
$(package):
	make ~/R_LIBS/$(package)
$(package).Rcheck: $(package)_$(version).tar.gz
	R CMD check --as-cran $(package)_$(version).tar.gz
$(package)_$(version).tar.gz: $(R) $(Rmd) $(html) ChangeLog DESCRIPTION inst/po/es/LC_MESSAGES/R-$(package).mo po/R-$(package)-es.po po/R-$(package).pot 
	R -e "library('roxygen2'); roxygenize('.')"
	R CMD build --no-build-vignettes .
po/R-$(package)-es.po: $(R) po/R-$(package).pot
	poedit po/R-$(package)-es.po
po/R-$(package).pot: $(R)
	# This target builds the pot file and update any R-$(package)-lang.po file under po dir
	R -e "library('tools'); update_pkg_po('.')"
	touch po/R-$(package).pot

~/R_LIBS/$(package): $(package)_$(version).tar.gz
	R CMD INSTALL $(package)_$(version).tar.gz
tests.out: $(test_FILES) ~/R_LIBS/$(package)
	R -e "devtools::test()" | tee tests.out
examples.out: ~/R_LIBS/$(package)
	R -e "devtools::run_examples()" | tee examples.out
tests: examples.out tests.out
	@echo Test done

inst/doc/%.html: $(R) $(Rmd)
	make vignettes
vignettes: $(R) $(Rmd)
	R -e "devtools::build_vignettes('.', clean = FALSE, install = FALSE)"
	cp --force doc/* inst/doc
	##rmdir doc
