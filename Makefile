# Syphilis_and_ARVs
### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: lancet_plots.Rout 

##################################################################

# make files

Sources = Makefile .gitignore README.md stuff.mk LICENSE.md 
include stuff.mk
# include $(ms)/perl.def

##################################################################

## Content

Sources += $(wildcard *.md *.R *.range.txt)

base.sim.Rout: base.parms.Rout functions.Rout model.Rout sim.R
%.sim.Rout: %.parms.Rout functions.Rout model.Rout sim.R
	$(run-R)

base.plots.Rout: base.sim.Rout plots.R
%.plots.Rout: %.sim.Rout plots.R
	$(run-R)

lancet_sims.Rout: base.parms.Rout model.Rout functions.Rout simFuns.Rout lancet_sims.R

lancet_plots.Rout: lancet_sims.Rout lancet_plots.R

######################################################################

### Makestuff

## Change this name to download a new version of the makestuff directory
# Makefile: start.makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
# -include $(ms)/oldlatex.mk
