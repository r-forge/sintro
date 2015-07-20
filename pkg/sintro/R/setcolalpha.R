# file  ~/sintro/R/setcolalpha.R 
# $Date:$
# $Rev:$
# $ID:$
# $URL:$

#! improve control over input formats

setcolalpha <- function(col, alpha='#40') {
	ifelse(nchar(col)==7, col <- paste(col, substr(alpha,2,3), sep='') ,
		ifelse (nchar(col)==9, {substr(col,8,9) <- substr(alpha,2,3)} ,		
			stop('setcolapha: col must be 7 or 9 character code')
	)
	)
		col
} #col
