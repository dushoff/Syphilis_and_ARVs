# Syphilis_and_ARVs
### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: base.plots.Rout 

##################################################################

# make files

Sources = Makefile .gitignore README.md stuff.mk LICENSE.md 
include stuff.mk
# include $(ms)/perl.def

##################################################################

## Content

Sources += $(wildcard *.md *.R)

base.sim.Rout: base.parms.Rout functions.Rout model.Rout sim.R
%.sim.Rout: %.parms.Rout functions.Rout model.Rout sim.R
	$(run-R)

base.plots.Rout: base.sim.Rout plots.R
%.plots.Rout: %.sim.Rout plots.R
	$(run-R)

######################################################################

### Makestuff

## Change this name to download a new version of the makestuff directory
# Makefile: start.makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
# -include $(ms)/oldlatex.mk
