#$HeadURL$
#$Id$
#$Date$
#$Author$
#$Revision$
#\encoding{utf8}
# setwd("")
# source('~/Documents/lectures/src/insider/profile/sprof/pkg/R/nodescloud.R', chdir = TRUE)
#! To Do
#!

## cf. source('~/Documents/lectures/src/insider/profile_pkgs/profr_0.2/R/parse.r', chdir = TRUE)
nodepackage<- function(x)
{
nodepackage0 <- function(node)
{where <- getAnywhere(node)$where
	if (length(where)==0) return("<not found>") else{
	where1 <- strsplit(where,':')
	if (is.null(where1)) return("<??>") else
		if (where1[[1]][1] == "package") return(where1[[1]][2]) else
		if (where1[[1]][1] == "namespace") return(where1[[1]][2]) else
		if ((length(where1)>1) && (where1[[2]][1] == "namespace")) return(where1[[2]][2]) else
		{warning(paste(node,"not found. got:",where1)); return("nn")}
		}
}
x <- as.character(x)
xpackage <- rep("nn",length(x))
for (i in (1:length(x))) xpackage[i]<-nodepackage0(x[[i]])
xpackage
}
