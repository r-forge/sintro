pkgname <- "sprof"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "sprof-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('sprof')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("readProf")
### * readProf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readProf
### Title: Read Rprof Output files and Stack Logs
### Aliases: readProf
### Keywords: utilities

### ** Examples

## Not run: 
##D ## Rprof() is not available on all platforms
##D profinterval <- 0.001
##D simruns <- 100
##D 
##D n <- 10000
##D x <- runif(n)
##D y0 <- 2+ 3 * x
##D 
##D sinknull <- textConnection(NULL, "w"); sink(sinknull)
##D Rprof(tmp <- tempfile(), interval = profinterval)
##D for (i in 1:simruns) {y <- y0 +  rnorm(n); xxx<- summary(lm(y~x))}
##D Rprof()
##D 
##D Rprof_out <- readProf(tmp)
##D 
##D unlink(tmp)
##D sink(); close(sinknull)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readProf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
