#$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/bertin/work/00sprofworksheet.R $
#$Id: 00bertinworksheet.R 87 2013-05-16 14:39:42Z gsawitzki $
#$Revision: 87 $
#$Date: 2013-05-16 16:39:42 +0200 (Thu, 16 May 2013) $
#$Author: gsawitzki $

# a commom worksheet for sprof

#!/bin/sh

svn propset svn:keywords "Date Author Id Revision HeadURL"  *.R
svn propset svn:keywords "Date Author Id Revision HeadURL" pkg/R/*.R
svn propset svn:keywords "Date Author Id Revision HeadURL" work/*.R
svn propset svn:keywords "Date Author Id Revision HeadURL" pkg/man/*.Rd
export _R_CHECK_TIMINGS_=0
export _R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_=TRUE

cd ~/Documents/lectures/src/insider/profile/sprof/
R CMD CHECK pkg  --no-multiarch  --timings
R CMD BUILD --compact-vignettes pkg --no-multiarch
R CMD BUILD pkg

rm sprof.pdf; R CMD Rd2pdf -o sprof.pdf  --internals --title="sprof" pkg
R CMD Rd2pdf -o sprof.pdf  --internals --no-clean --title="sprof" pkg

	cd /Users/gs/projects/rforge/sprof/pkg/inst/doc
	mv sprofR.pdf sprofR_temp.pdf
	qpdf sprofR_temp.pdf sprofR.pdf
	rm sprofR_temp.pdf
    cd /Users/gs/projects/rforge/sprof


# install package
R CMD INSTALL sprof

#### end of shell commands

#### R cmds for ad hoc construction
setwd("/Users/gs/projects/rforge/sprof/")
install.packages("/Users/gs/projects/rforge/sprof/sprof_0.1-76.tar.gz", repos=NULL, type="source")

#####

