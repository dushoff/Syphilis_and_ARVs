# Syphilis_and_ARVs
### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: paper_gg.eps 

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

Sources += legend.txt

paper_sims.Rout: base.parms.Rout model.Rout functions.Rout simFuns.Rout paper_sims.R

paper_plots.Rout: paper_sims.Rout paper_plots.R

# Current paper figure has facets

# Make a pdf and eps using R commands
paper_facet.Rout: paper_plots.Rout paper_facet.R
paper_facet.eps: paper_facet.Rout ;

# Make an eps using pdftops
paper_facet.Rout.eps: paper_facet.Rout.pdf Makefile
	pdftops -eps -r 1200 $< $@

# Use imagemagick
paper_facet.Rout.tiff: paper_facet.Rout.pdf Makefile
	convert -density 600 -trim $< -quality 100 $@

# Use ggsave
paper_gg.Rout: paper_facet.Rout gg.R
	$(run-R)
paper_gg.eps: paper_gg.Rout ;

fancy = paper_facet.Rout.pdf paper_facet.Rout.eps paper_facet.eps paper_facet.Rout.tiff paper_gg.eps

pushfancy: $(fancy:%=%.gp)

######################################################################

### Makestuff

## Change this name to download a new version of the makestuff directory
# Makefile: start.makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
# -include $(ms)/oldlatex.mk
