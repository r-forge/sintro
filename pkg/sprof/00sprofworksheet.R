#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# a commom worksheet for sprof
00sprofworksheet.R is the authoritative source for any current mess.
Build instructions and correction notes are placed here.
Eventually, they will bubble down to other files.

 \code{\link[igraph]{get.adjacency}}
 
###  To Do
- sync colors & attributes
- add consistent edge weights
complete cycle
- read in & first info
- massage // delete top & last
##
revise strategy: use <Rprof Ctrl> replacement for controls
##
plot_nodes(rpo)
plot_stacks(rpo)
plot_profiles(rpo)

###
promptClass(clName, filename = NULL, type = "class",
            keywords = "classes", where = topenv(parent.frame()),
            generatorName = clName)

promptMethods(f, filename = NULL, methods)

prompt(object, filename = NULL, name = NULL, ...)
promptData(object, filename = NULL, name = NULL)

## Default S3 method:
prompt(object, filename = NULL, name = NULL,
       force.function = FALSE, ...)

## S3 method for class 'data.frame'
prompt(object, filename = NULL, name = NULL, ...)


###
cscore <- rank(rpo$nodes$total.time,ties.method="random")
###
#!/bin/sh
cd ~/projects/rforge/sintro/pkg/sprof/

lapply(sprofRegressionExpl$stacks$nodes, function(x) {x[-(1:level)]}

svn propset svn:keywords "Date Author Id Revision HeadURL" sprof/man/*.Rd
svn propset svn:keywords "Date Author Id Revision HeadURL" sprof/R/*.R
svn propset svn:keywords "Date Author Id Revision HeadURL" sprof/vignettes/*.Rnw
xexport _R_CHECK_TIMINGS_=0
export _R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_=TRUE
# svn propset svn:keywords "Date Author Id Revision HeadURL" sprof/work/*.R

cd ~/projects/rforge/sintro/pkg/
R CMD CHECK sprof  --no-multiarch  --timings
cd ~/projects/rforge/sintro/pkg/
R CMD BUILD --compact-vignettes=gs+qpdf sprof --no-multiarch --md5
svn log -r 166:HEAD -v > ChangeLog0
cat sprof/ChangeLog >> ChangeLog0
##  check here !!!
mv ChangeLog0  sprof/ChangeLog
#$Revision$

rm sprof_internal.pdf; R CMD Rd2pdf -o sprof_internal.pdf  --internals --no-clean --title="sprof internal" sprof
#### R cmds for ad hoc construction
setwd("/Users/gs/projects/rforge/sintro/pkg/sprof/")

remove.packages("sprof")
install.packages("/Users/gs/projects/rforge/sintro/pkg/sprof_0.0-5.tar.gz", repos=NULL, type="source")

##
vignette <- function(){ 
options(width=72); setwd("/Users/gs/projects/rforge/sintro/pkg/sprof/work/vignettes/" )
Sweave(file= "../../vignettes/sprofiling.Rnw", output="sprofiling.tex", keep.source=TRUE)
# debug=TRUE, eps=FALSE, pdf=TRUE, keep.source=TRUE
}
##

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
