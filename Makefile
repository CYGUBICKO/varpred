## varpred package re-factor

current: target
-include target.mk

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.md)
Sources += $(wildcard *.R R/*.R)
Sources += $(wildcard man/*.Rd) NAMESPACE DESCRIPTION

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
