#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# a commom worksheet for sprof
00sprofworksheet.R is the authoritative source for any current mess.
Build instructions and correction notes are placed here.
Eventually, they will bubble down to other files.

###  To Do
complete cycle
- read in & first info
- massage // delete top & last
- write to file
- read&pass to proftools
- dots
##
plot_nodes(rpo)
plot_stacks(rpo)
plot_profiles(rpo)



##
revise strategy: use <Rprof> replacement for controls
###
cscore <- rank(rpo$nodes$total.time,ties.method="random")
###
#!/bin/sh
cd ~/projects/rforge/sintro/pkg/sprof/

lapply(sprofRegressionExpl$stacks$nodes, function(x) {x[-(1:level)]}

svn propset svn:keywords "Date Author Id Revision HeadURL"  *.R
svn propset svn:keywords "Date Author Id Revision HeadURL" pkg/R/*.R
svn propset svn:keywords "Date Author Id Revision HeadURL" work/*.R
svn propset svn:keywords "Date Author Id Revision HeadURL" pkg/man/*.Rd
export _R_CHECK_TIMINGS_=0
export _R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_=TRUE

cd ~/projects/rforge/sintro/pkg/
R CMD CHECK sprof  --no-multiarch  --timings
cd ~/projects/rforge/sintro/pkg/
R CMD BUILD --compact-vignettes=gs+qpdf sprof --no-multiarch --md5

rm sprof.pdf; R CMD Rd2pdf -o sprof.pdf  --internals --no-clean --title="sprof internal" sprof
#### R cmds for ad hoc construction
setwd("/Users/gs/projects/rforge/sintro/pkg/sprof/")

remove.packages("sprof")
install.packages("/Users/gs/projects/rforge/sintro/pkg/sprof_0.0-4.tar.gz", repos=NULL, type="source")

##
vignette <- function(){ 
options(width=72); setwd("/Users/gs/projects/rforge/sintro/pkg/sprof/work/vignettes/" )
Sweave(file= "sprofiling.Rnw", output="sprofiling.tex", keep.source=TRUE)
# debug=TRUE, eps=FALSE, pdf=TRUE, keep.source=TRUE
}
##
rn <- rstacks$nodes
data.matrix(rn)
data.frame(rn1)
rnm <- as.matrix(rn)

##
sources <- function(){} 

setwd('~/projects/rforge/sintro/pkg/sprof/')
file.edit('~/projects/rforge/sintro/pkg/sprof/R/readProf.R', chdir = TRUE)

source('~/projects/rforge/sintro/pkg/sprof/R/print.sprof.R', chdir = TRUE)
source('~/projects/rforge/sintro/pkg/sprof/R/plot.sprof.R', chdir = TRUE)
source('~/projects/rforge/sintro/pkg/sprof/R/readRprof.R', chdir = TRUE)
source('~/projects/rforge/sintro/pkg/sprof/R/rrle.R', chdir = TRUE)
source('~/projects/rforge/sintro/pkg/sprof/R/sampleRprof.R', chdir = TRUE)
source('~/projects/rforge/sintro/pkg/sprof/R/summary.sprof.R', chdir = TRUE)
source('~/projects/rforge/sintro/pkg/sprof/R/profiles_matrix.R', chdir = TRUE)
source('~/projects/rforge/sintro/pkg/sprof/R/stacks_matrix.R', chdir = TRUE)

	cd /Users/gs/projects/rforge/sprof/pkg/inst/doc
	mv sprofR.pdf sprofR_temp.pdf
	qpdf sprofR_temp.pdf sprofR.pdf
	rm sprofR_temp.pdf
    cd /Users/gs/projects/rforge/sprof


# install package
R CMD INSTALL sprof

#### end of shell commands
source('/Users/sw/R/R-devel/src/library/graphics/R/plot.R', chdir = TRUE)
source('/Users/sw/R/R-devel/src/library/stats/R/plot.lm.R', chdir = TRUE)
file.show('/Users/sw/R/R-devel/src/library/stats/R/plot.lm.R')


#####
file.edit('~/projects/rforge/sintro/pkg/sprof/vignettes_temp/sprofiling.Rnw')
% options(width=72); 
% setwd("/Users/gs/projects/rforge/sintro/pkg/spwork/vignettes/" )
% Sweave(file= "sprofiling.Rnw", output="sprofiling.tex", keep.source=TRUE)
