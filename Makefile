## varpred package re-factor

## https://cygubicko.github.io/varpred/index.html 

current: target
-include target.mk

vim_session:
	bash -cl "vmt"


######################################################################

Sources += $(wildcard *.md .*.yml)
Sources += $(wildcard vignettes/*.md)
Sources += $(wildcard vignettes/*.Rmd)
Sources += $(wildcard *.R R/*.R)
Sources += $(wildcard man/*.Rd) NAMESPACE DESCRIPTION
Sources += $(wildcard man/figures/*)
Sources += $(wildcard docs/*)
Sources += $(wildcard docs/articles/*)

Ignore += README.html
Ignore += *.md.args
Ignore += vignettes/.gitignore
Ignore += vignettes/*.pdf
Ignore += .gitignore
Ignore += varpred_*.*.tar.gz

######################################################################

## We don't need to preserve yml as defined in $(knitmd)
makemd = echo "library(rmarkdown); render(\"$^\", \"md_document\")" | R --slave
makepdf = echo "library(rmarkdown); render(\"$^\", \"pdf_document\")" | R --slave

autopipeR = defined

Sources += README.md 
Sources += README.Rmd 
README.md: README.Rmd
	$(makemd)

vignettes/varpred_intro.pdf: vignettes/vapred_intro.Rmd
	$(makepdf)

######################################################################

## Package main functions
varpred.Rout: R/varpred.R
helperfuns.Rout: R/helperfuns.R
supported.Rout: R/supported.R
plotsfuns.Rout: R/plotsfuns.R
methodfuns.Rout: R/methodfuns.R
pkgsExport.Rout: R/pkgsExport.R

######################################################################

install:
	make vignettes/varpred_intro.pdf
	make update-doc && make build-package && make install-tarball && make README.md
	make pkg-site

pkg-site:
	echo "pkgdown::build_site()" | R --slave

build-package:
	R CMD build .

install-tarball:
	R CMD INSTALL varpred_1.0.1.*

check-package:
	echo "devtools::check('.')" | R --slave

update-doc:
	echo "devtools::document('.')" | R --slave

######################################################################

### Makestuff

Sources += Makefile

## Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/chains.mk
-include makestuff/texi.mk
-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
