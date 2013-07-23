#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# a commom worksheet for sprof
00sprofworksheet.R is the authoritative source for any current mess.
Build instructions and correction notes are placed here.
Eventually, they will bubble down to other files.
#####

todo <- function(){}
reduce str, summary output

weighted nodes for graphviz

barplot_s

nodei wants to modify sprof argument

##source('~/Documents/lectures/src/insider/profile_pkgs/profr_0.2/R/parse.r', chdir = TRUE)
nodepackage <- function(node)
{where <- getAnywhere(node)$where
	if (length(where)==0) return("<not found>") else{
	where <- strsplit(where,':')
	if (is.null(where)) return("<??>") else
		if (where[[1]][1] == "package") return(where[[1]][2]) else
		{print(where); return("nn")}
		}
}

#add similar for name space.

> .function_sources <- function(df) {
+   fs <- sapply(levels(df$f), function(x) do.call(getAnywhere, list(x))$where[1])
+   
+   packaged <- grep("package", fs)
+   names <- sapply(strsplit(fs[packaged], ":"), "[", 2)
+   
+   fs[-packaged] <- NA
+   fs[packaged] <- names
+   unname(fs[as.character(df$f)])
+ }
> .function_sources("help")
Error in df$f : $ operator is invalid for atomic vectors

Enter a frame number, or 0 to exit   

1: .function_sources("help")
2: #2: sapply(levels(df$f), function(x) do.call(getAnywhere, list(x))$
3: lapply(X = X, FUN = FUN, ...)
4: levels(df$f)

Selection: 0
> fx <- function(x) do.call(getAnywhere, list(x))
> fx("help")
##
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
                                                                                                                                                      prompt <- function(name){
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
}

###
cscore <- rank(rpo$nodes$total.time,ties.method="random")
###
#!/bin/sh
cd ~/projects/rforge/sintro/pkg/sprof/

lapply(sprofRegressionExpl$stacks$nodes, function(x) {x[-(1:level)]}

svn propset svn:keywords "Date Author Id Revision HeadURL Name" sprof/man/*.Rd
svn propset svn:keywords "Date Author Id Revision HeadURL  Name" sprof/R/*.R
svn propset svn:keywords "Date Author Id Revision HeadURL Name" sprof/vignettes/*.Rnw
svn propset svn:keywords "Date Author Id Revision HeadURL Name"  sprof/tests/*.R
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

pdf <- function(){
cd ~/projects/rforge/sintro/pkg/
rm sprof_internal.pdf
R CMD Rd2pdf -o sprof_internal.pdf  --internals --no-clean --title="sprof internal" sprof
}
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
