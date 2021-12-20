# TODO: Add comment
# 
# Author: schueta6
###############################################################################

library(tools)

# Function encloses value in assignments "text=" and "col=" in 
# '\"', i.e. "text=blue" will be "text=\"blue\"" which is required
# in a do.call() command.

MarkAsStringInList <- function(x) {
	x <- as.character(x)
	if(!grepl("list\\(", x))
		return(x)
	else {
		if(grepl("text=", x)) {
			# mark text as string
			tstart 	<- regexpr("text=", x)
			end0	<- nchar(x)
			end    	<- unlist(gregexpr(",", x))
			dif		<- end-tstart
			neg 	<- which(dif < 0)
			if(length(neg) > 0) {
				dif <- dif[-neg]
				end <- end[-neg]
			}
			min	  	<- which(dif  == min(dif))
			x		<- paste0(	substr(x, 1, tstart+4), 
					"\"", 
					if(length(dif) == 0)
						paste0(substr(x, tstart+5, end0-1), "\")")
					else {
						paste0(
								substr(x, tstart+5, end[min]-1), 
								"\",", 
								substr(x, end[min]+1, end0) )
					}, collapse="")
		}
		if(grepl("col=", x)) {		
			# mark col as string
			tstart 	<- regexpr("col=", x)
			end0	<- nchar(x)
			end    	<- unlist(gregexpr(",", x))
			dif		<- end-tstart
			neg 	<- which(dif < 0)
			if(length(neg) > 0) {
				dif <- dif[-neg]
				end <- end[-neg]
			}
			min	  	<- which(dif  == min(dif))
			x		<- paste0(	substr(x, 1, tstart+3), 
								"\"", 
								if(length(dif) == 0)
									paste0(substr(x, tstart+4, end0-1), "\")")
								else {
									paste0(
										substr(x, tstart+4, end[min]-1), 
										"\",", 
										substr(x, end[min]+1, end0) )
								}, collapse="")
		}		
	}	
	return(x)
}

###############################################################################
# read plot parameterizeation, generate reference plots and derive MD5 hash-sum
# from the pngs

parms <- read.csv("./Ref_Plot_Parms.csv")
if("X" %in% colnames(parms))
	parms <- parms[,-which(colnames(parms) == "X")]
md5s  <- NULL

for(i in 1:nrow(parms)) {
	args <- as.list(parms[i,])
	idx  <- which(sapply(args, function(x) x %in% c("", NA)))
	if(length(args) > 0)
		args <- args[-idx]
	# factor variables into characer strings
	args <- lapply(args, function(x){
				if(is.factor(x)) 
					return(as.character(x))
				else
					return(x)
			})
	# "NULL" into NULL
	args <- lapply(args, function(x){
				if(x == "NULL") 
					return(NULL)
				else
					return(x)
			})
	# character strings with substring "list(" to be evaluated
	args <- lapply(args, function(x){
				if(is.character(x) && grepl("*.*\\(", x)) 
					return(eval(parse(text=MarkAsStringInList(x))))
				else
					return(x)
			})
	args$x <- fit.all.models

	if(file.exists("RefPlot.png"))
		try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
	png(filename="RefPlot.png")	
	do.call("plot", args=args)	
	dev.off()	
	md5 	<- md5sum("RefPlot.png")
	md5s	<- c(md5s, md5)
}

parms$MD5 <- md5s

write.csv2(parms, file="./Ref_Plot_Parms_with_MD5sum.csv")


###############################################################################
md5s <- NULL
# combination 1
if(file.exists("RefPlot.png"))
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
set.seed(1)
png(filename="RefPlot.png")	
precisionPlot(fit.all.models, model.no=1, cutoff=10)
dev.off()
md5 	<- md5sum("RefPlot.png")
md5s	<- c(md5s, md5)

# combination 2
if(file.exists("RefPlot.png"))
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
set.seed(2)
png(filename="RefPlot.png")	
precisionPlot(fit.all.models, model.no=2, cutoff=15, prob=c(.1, .9), BG="wheat")
dev.off()
md5 	<- md5sum("RefPlot.png")
md5s	<- c(md5s, md5)

# combination 3
if(file.exists("RefPlot.png"))
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
set.seed(3)
png(filename="RefPlot.png")	
precisionPlot(fit.all.models, model.no=3, cutoff=15, prob=c(.25, .75), BG="white", Title=list(text="This is the title"))
dev.off()
md5 	<- md5sum("RefPlot.png")
md5s	<- c(md5s, md5)

# combination 4
if(file.exists("RefPlot.png"))
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
set.seed(4)
png(filename="RefPlot.png")	
precisionPlot(fit.all.models, model.no=4, cutoff=15, prob=c(.25, .75), BG="white", alpha=0.25, alpha2=.8)
dev.off()
md5 	<- md5sum("RefPlot.png")
md5s	<- c(md5s, md5)

# combination 5
if(file.exists("RefPlot.png"))
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
set.seed(5)
png(filename="RefPlot.png")	
precisionPlot(fit.all.models, model.no=6, cutoff=15, prob=c(.25, .75), BG="white", alpha=0.25, alpha2=.8)
dev.off()
md5 	<- md5sum("RefPlot.png")
md5s	<- c(md5s, md5)

# combination 6
if(file.exists("RefPlot.png"))
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
set.seed(6)
png(filename="RefPlot.png")	
precisionPlot(fit.all.models, model.no=7, cutoff=15, prob=c(.01, .99), BG="lightblue", Xlabel=list(text="This is the X-axis label"), Ylabel=list(text="This is the Y-axis label"))
dev.off()
md5 	<- md5sum("RefPlot.png")
md5s	<- c(md5s, md5)

# combination 7
if(file.exists("RefPlot.png"))
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
set.seed(7)
png(filename="RefPlot.png")	
precisionPlot(	fit.all.models, model.no=8, cutoff=c(10, 15), prob=c(.1, .9), BG="lightgray", 
				HRLine=list(lwd=2, col="magenta", lty=2), Cutoff=list(col="blue", lty=4), alpha2=.5)
dev.off()
md5 	<- md5sum("RefPlot.png")
md5s	<- c(md5s, md5)

write.csv(cbind(Combination=1:7, MD5=md5s), file="MD5_Reference_precisionPlot.csv")