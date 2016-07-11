# adapted, and modified from MASS
# $Date$
# $Rev$
# $Header$


parcoord <- function (x, col = 1, lty = 1, 
	var.label = FALSE, 
	group,
	perm, negate, 
	rep=FALSE, ...) 
{
 negmark<- '-'
 if (!missing(group)) {
	 if  (col==1) {col <- rainbow(levels(group))[group]}} #! to improve
  if (!missing(negate)) 
  	{x[negate] <- - x[negate]; 
	colnames(x)[negate] <- paste(negmark,colnames(x)[negate], sep='') }
   if (!missing(perm)) x <- x[,perm]
   if (rep) x <- cbind(x,x)
    rx <- apply(x, 2L, range, na.rm = TRUE)
    x <- apply(x, 2L, function(x) (x - min(x, na.rm = TRUE))/(max(x, 
        na.rm = TRUE) - min(x, na.rm = TRUE)))
    matplot(1L:ncol(x), t(x), type = "l", col = col, lty = lty, 
        xlab = "", ylab = "", axes = FALSE, ...)
    axis(1, at = 1L:ncol(x), labels = colnames(x))
    for (i in 1L:ncol(x)) {
        lines(c(i, i), c(0, 1), col = "grey70")
        if (var.label) 
            text(c(i, i), c(0, 1), labels = format(rx[, i], digits = 3), 
                xpd = NA, offset = 0.3, pos = c(1, 3), cex = 0.7)
    }
    invisible()
}
