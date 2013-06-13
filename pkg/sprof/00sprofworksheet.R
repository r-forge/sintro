#$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/bertin/work/00sprofworksheet.R $
#$Id: 00bertinworksheet.R 87 2013-05-16 14:39:42Z gsawitzki $
#$Revision: 87 $
#$Date: 2013-05-16 16:39:42 +0200 (Thu, 16 May 2013) $
#$Author: gsawitzki $

# a commom worksheet for sprof

#!/bin/sh
cd ~/projects/rforge/sintro/pkg/sprof/

svn propset svn:keywords "Date Author Id Revision HeadURL"  *.R
svn propset svn:keywords "Date Author Id Revision HeadURL" pkg/R/*.R
svn propset svn:keywords "Date Author Id Revision HeadURL" work/*.R
svn propset svn:keywords "Date Author Id Revision HeadURL" pkg/man/*.Rd
export _R_CHECK_TIMINGS_=0
export _R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_=TRUE

R CMD CHECK sprof  --no-multiarch  --timings
R CMD BUILD --compact-vignettes=gs+qpdf sprof --no-multiarch --md5

rm sprof.pdf; R CMD Rd2pdf -o sprof.pdf  --internals --no-clean --title="sprof internal" sprof
 
setwd('~/projects/rforge/sintro/pkg/sprof/')
file.edit('~/projects/rforge/sintro/pkg/sprof/R/readProf.R', chdir = TRUE)

source('~/projects/rforge/sintro/pkg/sprof/R/print.sprof.R', chdir = TRUE)
source('~/projects/rforge/sintro/pkg/sprof/R/plot.sprof.R', chdir = TRUE)
source('~/projects/rforge/sintro/pkg/sprof/R/readProf.R', chdir = TRUE)
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

p <- profiles_matrix(rpo)

pp <- function(x) {xr <- rev(rpo$nodes[x]); paste(xr[!is.na(xr)], collapse = ' ')}
px<- apply(p,2,pp)

rev(rpo$nodes[p[,1]])
rpo$nodes[p[,1]]
paste(yy[!is.na(yy)], collapse = ' ')

re_profilessource <- function(x)

# install package
R CMD INSTALL sprof

#### end of shell commands

#### R cmds for ad hoc construction
setwd("/Users/gs/projects/rforge/sintro/pkg/sprof/")
install.packages(pkgs=install.packages("/Users/gs/projects/rforge/sintro/pkg/sprof_0.0-2.tar.gz", repos=NULL, type="source")

#####
file.edit('~/projects/rforge/sintro/pkg/sprof/vignettes_temp/sprofiling.Rnw')
% options(width=72); 
% setwd("/Users/gs/projects/rforge/sintro/pkg/spwork/vignettes/" )
% Sweave(file= "sprofiling.Rnw", output="sprofiling.tex", keep.source=TRUE)
