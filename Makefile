SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make >=4.0)
endif
.RECIPEPREFIX = >

PKGNM := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC := $(shell basename `pwd`)
RSCRIPT = Rscript --no-init-file
export _R_CHECK_SYSTEM_CLOCK_ = false
export _R_CHECK_FUTURE_FILE_TIMESTAMPS_ = false

all: dev_deps sentinels/check clean
.PHONY: all

dev_deps:
> ${RSCRIPT} -e "stopifnot(requireNamespace('DBI', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('devtools', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('knitr', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('pkgdown', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('pool', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('rmarkdown', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('RPostgres', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('tinytest', quietly = TRUE))";
.PHONY: dev_deps

sentinels/check: sentinels/pkgdown $(shell find inst/tinytest -type f)
> cd ..;\
> R CMD check $(PKGNM)_$(PKGVERS).tar.gz;\
> cd $(PKGSRC);\
> mkdir -p $(@D);\
> touch $@

sentinels/pkgdown: sentinels/build README.md LICENSE _pkgdown.yml \
  $(shell find pkgdown -type f)
> ${RSCRIPT} -e "options(yaml.eval.expr = TRUE); pkgdown::build_site()";\
> rm -rf docs/reference/Rplot001.png docs/deps/bootstrap-*/font*;\
> sed -i 's/@import url("font.css");//g' \
>   docs/deps/bootstrap-*/bootstrap.min.css;\
> rm -rf docs/reference/Rplot001.png docs/dev/deps/bootstrap-*/font*;\
> sed -i 's/@import url("font.css");//g' \
>   docs/dev/deps/bootstrap-*/bootstrap.min.css;\
> sed -i 's|dev/dev|dev|g' docs/dev/search.json docs/dev/sitemap.xml;\
> mkdir -p $(@D);\
> touch $@

sentinels/build: sentinels/doc
> cd ..;\
> R CMD build $(PKGSRC);\
> R CMD INSTALL $(PKGNM)_$(PKGVERS).tar.gz;\
> cd $(PKGSRC);\
> mkdir -p $(@D);\
> touch $@

sentinels/doc: $(shell find R -type f) DESCRIPTION
> ${RSCRIPT} -e "devtools::document()";\
> mkdir -p $(@D);\
> touch $@

clean:
> cd ..;\
> $(RM) -r $(PKGNM).Rcheck $(PKGNM)_$(PKGVERS).tar.gz
.PHONY: clean
