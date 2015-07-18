# file  ~/sintro/R/setcolalpha.R 

setcolalpha <- function(col, alpha='#40') {
	ifelse(nchar(col)==7, col <- paste(col, substr(alpha,2,3), sep='') ,
	ifelse (nchar(col)==9, {substr(col,8,2) <- substr(alpha,2,3)} ,		stop('setcolapha: col must be 7 or 9 character code')
	)
	)
		col
} #col
