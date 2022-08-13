## varpred package re-factor

current: target
-include target.mk

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.md)
Sources += $(wildcard vignettes/*.md)
Sources += $(wildcard *.R R/*.R)
Sources += $(wildcard man/*.Rd) NAMESPACE DESCRIPTION
Sources += $(wildcard docs/*)

Sources += README.md 

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
	make update-doc && make build-package && make install-tarball && make pkg-site

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

-include makestuff/texi.mk
-include makestuff/pipeR.mk
-include makestuff/chains.mk

-include makestuff/git.mk
-include makestuff/visual.mk
