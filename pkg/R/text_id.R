	text_id <- function(x, y, ind, labels.id = rownames(x), adj.x = TRUE, 
          cex.id = 0.75, label.pos = c(4, 2),...) {
          x <- x[ind];y <- y[ind];labels.id <- labels.id[ind]
	    labpos <- 
                if(adj.x) label.pos[1 + as.numeric(x > mean(range(x)))] else 3
	    text(x, y, labels.id, cex = cex.id, xpd = TRUE, 
		 pos = labpos, offset = 0.25,...)
	}
