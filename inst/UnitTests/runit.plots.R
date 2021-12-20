# TODO: Unit-tests for plotting functions. MD5 hashes of png-files will be compared
#		to those of reference png-files. This avoids storing reference graphics-files
#		thus, reducing required memory for storage.
# 
# Author: schueta6
###############################################################################

library(tools)

cat("\n\n###############################################################################\n\n")
cat("Executing unit-tests defined in runit.plots.R")

# for computing MD5 hash-sum
library(tools)

no <- 1

if(!"fit.all.models" %in% ls() || class(fit.all.models) != "VFP") {
	library(VCA)
	data(MultiLotReproResults)		# MLR
	fit.all.models  <- fit.vfp(MultiLotReproResults, model.no=1:9)
}

###############################################################################
# auxiliary function

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
### comprehensive testing of function plot.VFP

#* **target plot.VFP
#* **riskid RA02
#* **funid  Fun102
#* **desc Comprehensive testing 24 parameter combinations of function plot.VFP

TF001.plot.VFP <- function() {
	parms 	<- read.csv2("./data/Ref_Plot_Parms_with_MD5sum.csv")
	MD5ref	<- parms$MD5
	parms$MD5 <- NULL
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
		
		try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
		
		# create plot with specified set of arguments
		png(filename="RefPlot.png")	
		do.call("plot", args=args)	
		dev.off()	
		# compute MD5 hash-sum
		test.md5 <- md5sum("RefPlot.png")
		# perform check against reference MD5-value
		checkIdentical(as.character(MD5ref[i]), as.character(test.md5))
	}
}


###############################################################################

#* **target deriveCx
#* **riskid RA06
#* **funid  Fun202
#* **desc Four tests of finding Cx-concentrations

TF002.deriveCx <- function() {
	internal.fun <- function(obj, model.no=NULL, Mean, perc=5) {
		tmp <- predict(obj, model.no=model.no, newdata=Mean, type="sd")
		q	<- qnorm(mean=Mean, sd=tmp$Fitted, p=(100-perc)/100)
		q
	}
	### model 8
	# C95 at cutoff 10
	tmp1 <- deriveCx(fit.all.models, model.no=8, cutoff=10, start=8, tol=1e-15)
	co1	 <- internal.fun(fit.all.models, model.no=8, Mean=tmp1["Mean"], perc=5)
	checkEquals(co1, 10, tolerance=1e-12)
	# C5 at cutoff 10
	tmp2 <- deriveCx(fit.all.models, model.no=8, cutoff=10, start=11, Cx=.95, tol=1e-15)
	co2	 <- internal.fun(fit.all.models, model.no=8, Mean=tmp2["Mean"], perc=95)
	checkEquals(co2, 10, tolerance=1e-12)
	
	### model 6
	# C95 at cutoff 10
	tmp1 <- deriveCx(fit.all.models, model.no=6, cutoff=10, start=9, tol=1e-15)
	co1	 <- internal.fun(fit.all.models, model.no=6, Mean=tmp1["Mean"], perc=5)
	checkEquals(co1, 10, tolerance=1e-12)
	# C5 at cutoff 10
	tmp2 <- deriveCx(fit.all.models, model.no=6, cutoff=10, start=11, Cx=.95, tol=1e-15)
	co2	 <- internal.fun(fit.all.models, model.no=6, Mean=tmp2["Mean"], perc=95)
	checkEquals(co2, 10, tolerance=1e-12)
}


###############################################################################
#* **target precisionPlot
#* **riskid RA05
#* **funid  Fun203
#* **desc extensive testing of function precisionPlot

TF003.precisionPlot <- function() {
	
	md5s <- read.csv("./data/MD5_Reference_precisionPlot.csv")

	# combination 1
	if(file.exists("RefPlot.png"))
		try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
	set.seed(1)
	png(filename="RefPlot.png")	
	precisionPlot(fit.all.models, model.no=1, cutoff=10)
	dev.off()
	md5 	<- md5sum("RefPlot.png")
	checkIdentical(as.character(md5), as.character(md5s[1, "MD5"]))
	
	# combination 2
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
	set.seed(2)
	png(filename="RefPlot.png")	
	precisionPlot(fit.all.models, model.no=2, cutoff=15, prob=c(.1, .9), BG="wheat")
	dev.off()
	md5 	<- md5sum("RefPlot.png")
	checkIdentical(as.character(md5), as.character(md5s[2, "MD5"]))
	
	# combination 3
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
	set.seed(3)
	png(filename="RefPlot.png")	
	precisionPlot(fit.all.models, model.no=3, cutoff=15, prob=c(.25, .75), BG="white", Title=list(text="This is the title"))
	dev.off()
	md5 	<- md5sum("RefPlot.png")
	checkIdentical(as.character(md5), as.character(md5s[3, "MD5"]))
	
	# combination 4
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
	set.seed(4)
	png(filename="RefPlot.png")	
	precisionPlot(fit.all.models, model.no=4, cutoff=15, prob=c(.25, .75), BG="white", alpha=0.25, alpha2=.8)
	dev.off()
	md5 	<- md5sum("RefPlot.png")
	checkIdentical(as.character(md5), as.character(md5s[4, "MD5"]))
	
	# combination 5
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
	set.seed(5)
	png(filename="RefPlot.png")	
	precisionPlot(fit.all.models, model.no=6, cutoff=15, prob=c(.25, .75), BG="white", alpha=0.25, alpha2=.8)
	dev.off()
	md5 	<- md5sum("RefPlot.png")
	checkIdentical(as.character(md5), as.character(md5s[5, "MD5"]))
	
	# combination 6
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
	set.seed(6)
	png(filename="RefPlot.png")	
	precisionPlot(fit.all.models, model.no=7, cutoff=15, prob=c(.01, .99), BG="lightblue", Xlabel=list(text="This is the X-axis label"), Ylabel=list(text="This is the Y-axis label"))
	dev.off()
	md5 	<- md5sum("RefPlot.png")
	checkIdentical(as.character(md5), as.character(md5s[6, "MD5"]))
	
	# combination 7
	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
	set.seed(7)
	png(filename="RefPlot.png")	
	precisionPlot(	fit.all.models, model.no=8, cutoff=c(10, 15), prob=c(.1, .9), BG="lightgray", 
			HRLine=list(lwd=2, col="magenta", lty=2), Cutoff=list(col="blue", lty=4), alpha2=.5)
	dev.off()
	md5 	<- md5sum("RefPlot.png")
	checkIdentical(as.character(md5), as.character(md5s[7, "MD5"]))

	try(unlink("RefPlot.png", force=TRUE), silent=TRUE)
}
