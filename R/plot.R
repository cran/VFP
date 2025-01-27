# TODO: Add comment
# 
# Author: schueta6
###############################################################################




#' Plot VFP-Ojbects.
#' 
#' Function takes an object of class 'VFP' and plots a fitted variance-function
#' either on the original variance-scale ('type="vc"') or on the CV-scale ("cv").
#' The corresponding 100x(1-alpha)\% confidencen interval around the variance-function
#' can be plotted either as lines ('ci.type="lines') or as per default as CI-band.
#' 
#' @param x				(VFP) object as returned by function 'fit_vfp'
#' @param model.no		(integer) specifying which model to plot, must be one of the fitted models
#' @param type			(character) either "vc" to generate a plot on the original
#' 						variance-scale or "cv" to plot it on the coefficient of variation
#' 						scale, i.e. CV = 100*sqrt(VC)/Mean or "log" to plot after a variance 
#' 						stabilizing transformation. The latter will not work if 'Prediction' is specified!. 
#' @param add			(logical) TRUE = the current (im)precision profile is added to an existing plot,
#' 						FALSE = a new plot will be generated
#' @param alpha			(numeric) value specifying the 100x(1-alpha)\% confidencen interval
#' 						to be plotted around the function
#' @param ci.type		(character) either "band" to plto the CI as polygon, or "lines" to
#' 						plot the CI-bounds as two separate line using color 'ci.col' 
#' @param dispersion	(numeric) NULL = the dispersion parameter will be estimated,
#' 						numeric value = the dispersion parameter will be used as specified
#' @param browse		(logical) TRUE = if multiple models were fitted, all will be displayed one after
#' 						the other in increasing order of their respective AIC, mouse-klick on the plot
#' 						triggers switch to the next model
#' @param BG			(character) string specifying a background color
#' @param ci.col		(character) string specifying a color used for the CI-region 
#' @param Title			(list) passed to function \code{\link{mtext}} controlling content and style of
#' 						the main title
#' @param Xlabel		(list) passed to function \code{\link{mtext}} controlling the labeling
#' 						of the X-axis
#' @param Ylabel		(list) passed to function \code{\link{mtext}} controlling the labeling
#' 						of the Y-axis
#' @param Points		(list) passed to function \code{\link{points}} controlling how 
#' 						data used to fit a variance function shall be plotted
#' @param Line			(list) passed to function \code{\link{lines}} controlling the visual appearance
#' 						of the line representing the fitted variance-function 
#' @param Grid			(list) passed to function \code{\link{add_grid}} controlling the appearance
#' 						of a grid, set to NULL to omit
#' @param Crit			(list) passed to function \code{\link{legend}} per default used to present
#' 						the optimality criterion for choosing the best fitting model, per default this
#' 						is AIC and additionally residual sum of squares (RSS) is shown for the plotted
#' 						model in the upper-right corner
#' @param xlim			(numeric) vector of length two specifying plot-limits in X-direction, if NULL
#' 						these will be automatically determined
#' @param ylim			(numeric) vector of length two specifying plot-limits in Y-direction, if NULL
#' 						these will be automatically determined
#' @param Prediction	(list) with elements 'y' specifying values on VC-, SD- or CV-scale depending on
#' 						'type' for which predictions on the X-axis are requested or 'x' specifying mean-values
#' 						on the X-axis for which predictions on the Y-axis are requested; furthermore,
#' 						all graphical parameters accepted by function \code{\link{lines}} indicating predictions;
#' 						NULL to omit (default);
#' 						additionally to arguments accepted by function 'lines', one can specify parameters
#' 						'x.line' and 'y.line' used to indicated values at the respective axis in margin-lines see
#' 						\code{\link{mtext}} for details; 'cex' and 'font' for specifying how these values are plotted.
#' 						The same color as lines is used	per default but can be changed setting 'text.col'.
#' 						Can also be (numeric) vector (not a list) which results in using default graphical settings. 
#' 						See also parameter Pred.CI.
#' @param Pred.CI		(list) with all parameters accepted by function \code{\link{rect}}; if not NULL,
#' 						the 100x(1-alpha)\% CI of predicted values will be added as a semi-transparent rectangle
#' 						per default (see examples).
#' @param Model			(logical) TRUE = plots the fitted model as subtitle below the main title, FALSE = omits this
#' @param CI.method		(character) one of "t", "normal", "chisq" specifying which CI-method to use for
#' 						deriving confidence intervals 									
#' @param use.log		(logical) TRUE = X- and Y-axis will be log-transformed
#' @param Npred			(integer) specifying the number of data points used to plot the fitted model, the larger the 
#' 						smoother (maybe slower if too large)
#' @param ...			additional parameters passed forward
#' 
#' @return (matrix) of predictions at user-specified X- or Y-coordinates is invisibly return in case Prediction is not NULL
#' 
#' @method plot VFP
#' 
#' @author Andre Schuetzemeister \email{andre.schuetzeneister@@roche.com}
#' 
#' @seealso \code{\link{fit_vfp}}, \code{\link{predict.VFP}}, \code{\link{predict_mean}}
#' 
#' @examples 
#' \donttest{
#' library(VCA)
#' data(VCAdata1)
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by="sample")
#' mat <- get_mat(lst)		# automatically selects "total"
#' mat
#' 
#' res <- fit_vfp(model.no=1:9, Data=mat)
#' plot(res)
#' plot(res, type="cv")
#' plot(res, type="cv", ci.type="lines", ci.col="red",
#' 		Grid=list(col="wheat"), Points=list(pch=2, lwd=2, col="black"))
#' 
#' # same for repeatability
#' mat.err <- get_mat(lst, "error")
#' res.err <- fit_vfp(1:9, Data=mat.err)
#' plot(res.err)
#' 
#' # add predictions to plot, e.g. functional sensitivity
#' plot(res.err, type="cv", xlim=c(0, 4), Prediction=10)
#' 
#' # variability at X-values are of interest
#' plot(res.err, type="cv", xlim=c(0, 4), Prediction=list(x=0.5))
#' 
#' # one can specify X- and Y-values in the "Prediction" list-argument
#' plot(res.err, type="cv", xlim=c(0, 4), 
#' 		Prediction=list(x=c(0.25, 0.5), y=15))
#' 
#' #######################################################################
#' # another example using CA19_9 data from CLSI EP05-A3
#' 
#' data(CA19_9)
#' 
#' # fit reproducibility model to data
#' fits.CA19_9 <- anovaVCA(result~site/day, CA19_9, by="sample")
#' 
#' # fit within-laboratory-model treating site as fixed effect
#' fits.ip.CA19_9 <- anovaMM(result~site/(day), CA19_9, by="sample")
#' 
#' # the variability "between-site" is not part of "total"
#' fits.ip.CA19_9[[1]]
#' fits.CA19_9[[1]]
#' 
#' # extract repeatability
#' rep.CA19_9 <- get_mat(fits.CA19_9, "error")
#' 
#' # extract reproducibility
#' repro.CA19_9 <- get_mat(fits.CA19_9, "total")
#' 
#' # extract intermediate-precision (within-lab)
#' ip.CA19_9 <- get_mat(fits.ip.CA19_9, "total")
#' 
#' # fit model (a+bX)^C (model 8) to all three matrices
#' 
#' mod8.repro 	<- fit_vfp(repro.CA19_9, 8)
#' mod8.ip		<- fit_vfp(ip.CA19_9, 8)
#' mod8.rep		<- fit_vfp(rep.CA19_9, 8)
#' 
#' # plot reproducibility precision profile first
#' # leave enough space in right margin for a legend
#' plot(mod8.repro, mar=c(5.1, 7, 4.1, 15), 
#' 		type="cv", ci.type="none", Model=FALSE,
#' 		Line=list(col="blue", lwd=3), 
#' 		Points=list(pch=15, col="blue", cex=1.5),  
#' 		xlim=c(10, 450), ylim=c(0,10),
#' 		Xlabel=list(text="CA19-9, kU/L (LogScale) - 3 Patient Pools, 3 QC Materials",
#' 				cex=1.5), Title=NULL,
#' 		Ylabel=list(text="% CV", cex=1.5),
#' 		Grid=NULL, Crit=NULL, log="x")
#' 
#' # add intermediate precision profile
#' plot	(mod8.ip, type="cv", add=TRUE, ci.type="none",
#' 		Points=list(pch=16, col="deepskyblue", cex=1.5),
#' 		Line=list(col="deepskyblue", lwd=3), log="x")
#' 
#' # add repeatability precision profile
#' plot(mod8.rep, type="cv", add=TRUE, ci.type="none",
#' 		Points=list(pch=17, col="darkorchid3", cex=1.5),
#' 		Line=list(col="darkorchid3", lwd=3), log="x")
#' 
#' # add legend to right margin
#' legend_rm( x="center", pch=15:17, col=c("blue", "deepskyblue", "darkorchid3"),
#' 		cex=1.5, legend=c("Reproducibility", "Within-Lab Precision", "Repeatability"),
#' 		box.lty=0)
#' 
#' # repeatability precision profile with some beautifications
#' plot(mod8.rep, BG="darkgray", 
#' 		Points=list(pch=17, cex=1.5, col="blue"), Line=list(col="blue"),
#' 		Grid=list(x=seq(0, 400, 50), y=seq(0, 100, 10), col="white"),
#' 		Xlabel=list(cex=1.5, text="CA19-9 [U/mL]", col="blue"),
#' 		Ylabel=list(cex=1.5, text="Repeatability on Variance-Scale", col="blue"),
#' 		Crit=list(text.col="white", text.font=2, cex=1.25))
#' }

plot.VFP <- function(	x, model.no=NULL, type=c("vc", "sd", "cv"), add=FALSE,
		alpha=.05, ci.col="gray90", ci.type=c("band", "lines", "none"), 
		dispersion=NULL, browse=FALSE, BG="white", Title=list(),
		Xlabel=list(), Ylabel=list(), Line=list(), 
		Points=list(), Grid=list(), Crit=list(),
		ylim=NULL, xlim=NULL, Prediction=NULL, Pred.CI=NULL, Model=TRUE,
		CI.method=c("chisq", "t", "normal"), use.log=FALSE,
		Npred=200, ...)
{
	call 	<- match.call()
	obj 	<- x
	type 	<- match.arg(type[1], choices=c("vc", "cv", "sd"))
	ci.type <- match.arg(ci.type[1], choices=c("band", "lines", "none"))
	stopifnot(alpha > 0 && alpha < 1)
	
	#### CI-method one of "t", "normal", "chisq" (default)
	CI.method	<- match.arg(CI.method[1], choices=c("t", "normal", "chisq"))
	
	if(is.null(xlim))
	{
		xlim <- range(obj$Data[,"Mean"])
		xlim[1] <- xlim[1] * 0.75
		xlim[2] <- xlim[2] * 1.05
	}
	else
	{
#		xlim <- eval(call$xlim)
		stopifnot(is.numeric(xlim))
		stopifnot(length(xlim) == 2)
		if(xlim[1] <= 0)
			xlim[1] <- min(obj$Models[[1]]$data[,"Mean"], na.rm=TRUE)/10
	}
	
	old.par <- par(mgp=c(3, .75, 0))
	on.exit(suppressWarnings(par(ask=FALSE)))
	
	aic 	<- sort(obj$AIC)
	num 	<- which(obj$AIC== aic[1])
	models 	<- sub("Model_", "", names(obj$RSS))
	
	if(!is.null(model.no))				
	{	
		if(!model.no %in% models)
			stop("Specified model ", paste0("'", model.no, "'")," is not among fitted models: ", paste(models, collapse=", "),"!")
		else
			num <- which(models == model.no)
	}
	
	# browse through all fitted models
	if(browse && is.null(model.no))
	{
		dev		 <- obj$Deviance[names(aic)]
		rss		 <- obj$RSS[names(aic)]
		tmp.args <- as.list(call)[-1]
		
		for(i in 1:length(aic))
		{
			mod <- as.numeric(sub("Model_", "", names(aic)[i]))
			tmp.args$model.no <- mod
			par(ask=TRUE)
			tmp <- try(do.call(plot.VFP, tmp.args), silent=TRUE)
			if(is(tmp, "try-error"))
				warning(paste0("Model '", mod, "' could not be plotted due to an error!\n\nError:\n\t", attr(tmp, "condition")$message,"\n\n"))
			mtext( 	side=1, at=par("usr")[1], line=3, font=6,
					text=paste(switch(i, "1"="", "2"="2nd", "3"="3rd", "4"="4th", 
									"5"="5th", "6"="6th", "7"="7th", "8"="8th", "9"="9th", "10"="10th"),
							"best-fitting model"))
			cat(paste0(	"(",i,") ",names(aic)[i], ifelse(names(aic)[i] == "Model_10", "\t", "\t\t"),	
							format(paste0("AIC: ", signif2(aic[i])), width=15),
							format(paste0("Deviance: ", signif2(dev[i])), width=20),
							format(paste0("RSS: ", signif2(rss[i])), width=15),
							"\t\t"))
		}
		cat("\n\n")
		
		return()
	}	
	
	stopifnot(is.null(dispersion) || (is.numeric(dispersion) && dispersion > 0))
	
	if(is.null(dispersion))
	{
		dispersion <- 1
	}
	
	Main 	<- switch(	type, 
			"vc"="Variance", 
			"cv"="CV",
			"sd"="SD")
	Main 	<- paste0("Precision Profile on ", Main, "-Scale")
	Data 	<- obj$Models[[num]]$data
	if(is.null(Data))
		Data <- obj$Data											# for EP17 model
	xval 	<- seq(xlim[1], xlim[2], length.out=Npred)				# function is drawn by using 100 points
	
	if(use.log)
		xval <- log(xval)
	
	# call predict.VFP here
	preds	<- predict(	obj, model.no=model.no, newdata=xval, dispersion=dispersion, type=type, 
			CI.method=CI.method, use.log=use.log, alpha=alpha)
	
	Np <- nrow(preds)				
	
	# sanity check for UCL values
	prdc <- preds$UCL[1:(Np-1)]
	succ <- preds$UCL[2:Np]
	prop <- prdc / succ
	ind  <- which(prop > 1e3)
	if(length(ind) > 0)
	{
		preds <- preds[-ind,,drop=F]
		xval  <- preds$Mean
	}
	
	if(all(is.na(preds$SE)))
		message("Confidence-region for current model could not be estimated!")
	
	# full freedom for user to specify points
	Points.default <- list(col="blue", pch=16, cex=1)
	Points.default[names(Points)] <- Points	
	Points <- Points.default
	Points$x <- Data[,"Mean"]
	
	Xlabel.default <- list(side=1, line=3, font=1, cex=1.25)
	if(is.character(Xlabel))
		Xlabel <- list(text=Xlabel)
	Xlabel.default[names(Xlabel)] <- Xlabel
	Xlabel <- Xlabel.default
	
	Ylabel.default <- list(side=2, line=4, font=1, cex=1.25)
	if(is.character(Ylabel))
		Ylabel <- list(text=Ylabel)
	Ylabel.default[names(Ylabel)] <- Ylabel
	Ylabel <- Ylabel.default	
	
	Grid.default <- list(col="gray70", lty=2)
	Grid.default[names(Grid)] <- Grid
	if(!is.null(Grid))
		Grid <- Grid.default
	
	y 	<- preds$Fitted	#fit				# refers to variance-scale
	se	<- preds$SE		#se	<- preds$se.fit
	
	#Points$y <- Data[in.xlim, "VC"]
	Points$y <- Data[, "VC"]
	if(is.null(Ylabel$text))
	{
		Ylabel$text <- switch(type,
				"vc"=ifelse(use.log, "log( Variance )", "Variance"),
				"cv"=ifelse(use.log, "log( %CV )","Coefficient of Variation [%]"),
				"sd"=ifelse(use.log, "log( SD )","Standard Deviation"))
	}
	
	if(is.null(Xlabel$text))
		Xlabel$text <- 	ifelse(use.log, "log( Mean )", "Mean")
	
	CIupper 	<- preds$UCL
	CIlower 	<- preds$LCL
	
	if(type %in% c("sd", "cv"))
	{
		Points$y	<- sqrt(Data[,"VC"])
		if(type == "cv")
			Points$y	<- 100 * Points$y / Data[,"Mean"]
	}
	
	if(use.log)
	{
		Points$y 	<- log(Points$y)
		Points$x 	<- log(Points$x)
	}
	
	if(is.null(ylim))
	{
		if(use.log)
		{
			ylim=c(min(c(CIlower,min(Points$y)), na.rm=TRUE), max(c(CIupper, Points$y), na.rm=TRUE))
		}
		else
		{
			ylim <- c(0, max(c(CIupper, Points$y), na.rm=TRUE))
		}
	}
	
	nc <- max(nchar(pretty(ylim)))
	
	if(is.null(call$mar) && !add)
		par(mar=c(5, max(nc-1, 6), 4,1))
	else
		par(mar=eval(call$mar))
	
	if(!add)					# generate new plot
	{
		if(is.null(call$axes))
		{
			plot(	xval, y, type="n", xlab="", ylab="", main="", ylim=ylim,
					las=1, axes=FALSE, ...)
		}
		else
		{
			plot(	xval, y, type="n", xlab="", ylab="", main="", ylim=ylim,
					las=1, ...)
		}
		
		if(is.null(call$axes) || isTRUE(call$axes) )
		{
			Xax <- axis(1)
			Yax <- axis(2, las=1)
			
			if(is.null(Grid$x))
				Grid$x <- Xax
		}
		
		if(!is.null(Title))
		{
			Title.def <- list(text=Main, line=1.75, cex=1.5)
			if(is.character(Title))
				Title <- list(text=Title)
			Title.def[names(Title)] <- Title
			Title <- Title.def
			Title$side=3
			do.call("mtext", Title)
		}
		USR <- par("usr")
		rect(USR[1], USR[3], USR[2], USR[4], col=BG, border=NA)
		
		do.call("mtext", Xlabel)
		do.call("mtext", Ylabel)
	}
	
	if(ci.type == "band")
	{
		CImat <- cbind(xval, CIlower, CIupper)
		CImat <- na.omit(CImat)
		polygon(x=c(CImat[,1], rev(CImat[,1])),
				y=c(CImat[,3], rev(CImat[,2])),
				col=ci.col, border=NA)
	}
	else if(ci.type == "lines")
	{
		lines(xval, CIupper, lty=2, col=ci.col)
		lines(xval, CIlower, lty=2, col=ci.col)
	}
	
	if(!is.null(Grid) && !add)
		do.call("add_grid", Grid)
	
	predMean <- NULL
	
	# add predicted mean for specified Y-value(s) or read off fitted value at given mean 
	if(!is.null(Prediction))
	{
		if(!is.numeric(Prediction) && is.null(Prediction$y) && is.null(Prediction$x))
			warning("Neither Y- nor X-value(s) specified, i.e. no prediction(s) requested!")
		else
		{
			if(is.numeric(Prediction))						# numeric values interpreted as Y-values (standard predict.VFP can used otherwise)
				Prediction <- list(y=Prediction)
			
			Type <- switch(	type, 
					vc = "VC",
					sd = "SD",
					cv = "CV"
			)
			
			if(is.numeric(Prediction$y))
			{
				predMean <- predict_mean(x, model.no=model.no, newdata=Prediction$y, type=type)
				predMean <- predMean[,c("Mean", Type, "LCL", "UCL")]
				predMean$Type <- 1
			}
			if(is.numeric(Prediction$x))
			{
				predVar <- predict(x, model.no=model.no, newdata=Prediction$x, type=type)
				predVar <- eval(parse(text=paste0("cbind(", ifelse(type=="log", "Log.Mean", "Mean"), "=Prediction$x,", Type, "=predVar$Fitted, LCL=predVar$LCL, UCL=predVar$UCL, Type=2)")))
				
				if(is.null(predMean))
					predMean <- predVar					# no y-values was specified
				else
					predMean <- rbind(predMean, predVar)
			}
			
			# confidence interval for predictions
			if(!is.null(Pred.CI))
			{
				Pred.CI.def 	<- list(col=as_rgb("blue", .15), border=NA)
				Pred.CI.def[names(Pred.CI)] <- Pred.CI
				Pred.CI			<- Pred.CI.def
			}
			
			Prediction.def 	<- list(lty=2, lwd=2, col="blue", x.line=1.5, y.line=2, digits=2, 
					cex=1, font=1)
			Prediction.def[names(Prediction)] <- Prediction
			Prediction 		<- Prediction.def
			pred.col		<- ifelse(is.null(Prediction$text.col), Prediction$col, Prediction$text.col)
			x.line			<- Prediction$x.line
			y.line			<- Prediction$y.line
			digits			<- Prediction$digits
			pred.cex		<- Prediction$cex
			pred.font		<- Prediction$font
			
			USR 	<- par("usr")
			Largs	<- list(lty=Prediction$lty, lwd=Prediction$lwd, col=Prediction$col)
			
			for(i in 1:nrow(predMean))
			{
				if(!is.null(Pred.CI))
				{
					predArgs 			<- Pred.CI
					predArgs$xleft 		<- ifelse(predMean[i,"Type"]==2, USR[1], predMean[i,"LCL"])
					predArgs$ybottom 	<- ifelse(predMean[i,"Type"]==2, predMean[i,"LCL"], USR[3])
					predArgs$xright 	<- ifelse(predMean[i,"Type"]==2, predMean[i, ifelse(type=="log", "Log.Mean", "Mean")], predMean[i,"UCL"])
					predArgs$ytop 		<- ifelse(predMean[i,"Type"]==2, predMean[i,"UCL"], predMean[i, Type])
					
					do.call("rect", predArgs)
				}
				tmpArgs 	<- Largs
				tmpArgs$x	<- c(USR[1], rep(predMean[i, 1], 2))
				tmpArgs$y	<- c(rep(predMean[i, 2], 2), USR[3])
				do.call("lines", tmpArgs)
				
				mtext(	side=1, text=round(unlist(predMean[i,1]), digits=digits), at=predMean[i,1], 
						col=pred.col, line=x.line, cex=pred.cex, font=pred.font)
				
				mtext(	side=2, text=round(unlist(predMean[i,2]), digits=digits), at=predMean[i,2], 
						col=pred.col, line=y.line, cex=pred.cex, font=pred.font, las=1) 
			}
		}
	}
	
	# will be done if add is TRUE or FALSE
	
	Line.def <- list(col="black", lwd=1, lty=1)
	Line.def[names(Line)] <- Line
	Line   <- Line.def
	Line$x <- xval
	Line$y <- y
	do.call("lines", Line)
	do.call("points", Points)
	
	if(!add)
	{
		if(Model)
		{
			# model number and formula; normalize X-coord from normalized inner region coords to user coords
			mtext(	side=3, line=0.45, adj=1, at=grconvertX(0.5, from="npc", to="user"), 
					text=paste0(sub("_", " ", names(x$AIC)[num]),":   "), cex=1.1, font=3)
			mtext(	side=3, line=0.25, adj=0, at=grconvertX(0.5, from="npc", to="user"),
					text=parse(text=obj$Formulas[num]))
		}
		# use 4 significant digits or for large values (> 4 digits) integer-representation
		tmpAIC <- signif2(x$AIC[num])
		tmpRSS <- signif2(x$RSS[num])
		
		if(!is.null(Crit))
		{
			Crit.default <- list(	x=ifelse(type=="cv", "topright", "topleft"), 
					box.lty=0, bg=par("bg"),
					legend=parse(text=c(paste("AIC ==", tmpAIC),
									paste("RSS[VC] ==", paste(tmpRSS, " (unweighted)")))))
			Crit.default[names(Crit)] <- Crit
			Crit <- Crit.default
			do.call("legend", Crit)
		}
		
		box()
	}
	
	res <- NULL
	
	if(!is.null(predMean))
	{
		tLCL 	<- paste0("LCL.", Type)
		tUCL 	<- paste0("UCL.", Type)
		pred.X 	<- predMean[which(predMean[,"Type"]==1),,drop=FALSE]
		pred.Y 	<- predMean[which(predMean[,"Type"]==2),,drop=FALSE]
		
		if(nrow(pred.X) > 0)
		{
			res.X  	<- pred.X[,1:2,drop=FALSE]
			res.X	<- cbind(res.X, LCL.Mean=as.numeric(pred.X[,"LCL"]))
			res.X	<- cbind(res.X, UCL.Mean=as.numeric(pred.X[,"UCL"]))
			res.X 	<- cbind(res.X, LCL.Y=NA, UCL.Y=NA)			
			res 	<- res.X
		}
		if(nrow(pred.Y) > 0)
		{
			res.Y 	<- pred.Y[,1:2,drop=FALSE]
			res.Y	<- cbind(res.Y, LCL.Mean=rep(NA, nrow(res.Y)))
			res.Y	<- cbind(res.Y, UCL.Mean=rep(NA, nrow(res.Y)))
			res.Y 	<- cbind(res.Y, pred.Y[,c("LCL", "UCL"),drop=FALSE])
			
			colnames(res.Y)[5:6] <- c("LCL.Y", "UCL.Y")
			
			if(!is.null(res))
				res <- rbind(res, res.Y)
			else
				res <- res.Y
		}
		colnames(res)[5:6] <- c(tLCL, tUCL)
	}
	
	#par(old.par)
	invisible(res)
}



#' Precision Performance Plot of Qualitative Tests.
#' 
#' This function visualizes what is described in the CLSI EP12 guideline
#' for qualitative test with internal continuous response (ICR). The hit rate,
#' i.e. the number of measurements deemed to have a certain condition. 
#' The C5 and C95 concentrations will be derived per default by this function
#' but it can be set to any set of hit rates.
#' The histograms representing normal distribution of imprecisions at specific
#' concentrations will be scaled to nicely fit into the plot, i.e. the area under
#' the plot will not be equal to 1.
#' 
#' @param vfp			(VFP) object modeling imprecision over the measuring range
#' @param model.no		(integer) specifying the VFP-model to used
#' @param cutoff		(numeric) specifying one or two cutoff(s), the latter will 
#' 						implicitly define an equivical zone with implications on how
#' 						'prob' will be interpreted (see 'prob' for details)
#' @param prob			(numeric) values 0 < x < 1 specifying coverage probability of
#' 						an respecitive normal distribution at cutoff, in case of two 
#' 						cutoffs all elements of 'prob' < 0.5 will be evaluated in 
#' 						regard to cutoff 1, and all 'prob' > 0.5 in regard to cutoff 2
#' @param col			(character) strings specifying colors of the different distributions,
#' 						which will be plotted semi-transparent using 'alpha1' for specifying
#' 						the level of transparency (1=opaque, 0=fully transparent)
#' @param Cutoff		(list) specifying all parameters of the \code{\link{abline}} function.
#' 						Vertical lines representing one or two cutoffs can be specified, the
#' 						color will be re-used for a label in the upper margin. Set to NULL
#' 						to omit.
#' @param Title			(list) specifying all parameters applicable in function
#' 						\code{\link{mtext}} for specifying a main title of the plot
#' @param Xlabel		(list) specifying all parameters applicable in function
#' 						\code{\link{mtext}} for specifying the X-axis label of the plot
#' @param Ylabel		(list) specifying all parameters applicable in function
#' 						\code{\link{mtext}} for specifying the Y-axis label of the plot
#' @param HRLine		(list) specifying all parameters applicable in \code{\link{lines}}
#' 						of the line representing the hit rate developing from 0\% to 100\% 
#' @param Legend		(logical) TRUE = a legend is added to the plot
#' @param nclass 		(integer) number of classes in the histograms representing normal
#' 						distributions of imprecision at Cx-concentrations, number<10 will lead
#' 						to automatically determining appropriate numbers per histogram (default)
#' @param BG			(character) string specifying a background color
#' @param digits		(integer) number of significant digits used to indicated concentrations Cx
#' @param alpha			(numeric) value 0<=x<=1 specifying the level of transparency of histograms
#' @param alpha2		(numeric) similar to 'alpha' referring to the coverage probability, i.e. 
#' 						setting it to a value < 0 will highlight coverage probabilities in histograms
#' @param xlim			(numeric) plotting limits in X-direction
#' @param col.grid		(character) string specifying a color name to be used for the grid providing
#' 						orientation in X- and Y-direction
#' @param Nrand			(integer) specifying the number of data points simulated to represent a normal
#' 						distribution
#' 
#' @aliases precisionPlot
#' 
#' @examples 
#' \dontrun{
#' # perform variance component analysis
#' library(VCA)
#' data(VCAdata1)
#' # perform VCA-anaylsis
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by="sample")
#' # transform list of VCA-objects into required matrix
#' mat <- get_mat(lst)		# automatically selects "total"
#' mat
#' # fit all models batch-wise, the best fitting will be used automatically
#' res <- fit_vfp(model.no=1:9, Data=mat)
#' # plot hit and visualize imprecision usign default settings
#' precision_plot(res, cutoff=20)
#' # without normal distribution at cutoff do
#' precision_plot(res, cutoff=20, prob=c(.05, .95), col=c("blue", "red"))
#' # highlight the proportion > cutoff (hit rate) more 
#' precision_plot(res, cutoff=20, prob=c(.05, .95), col=c("blue", "red"), alpha2=.5)
#' # plot with legend
#' precision_plot(res, cutoff=20, prob=c(.05, .95), col=c("blue", "red"), alpha2=.5, Legend=TRUE)
#' # use different probabilities and colors
#' precision_plot(res, cutoff=20, prob=c(.05, .95), col="black", alpha2=.3)
#' 
#' # now using two cutoffs, i.e. with equivocal zone
#' precision_plot(	res, cutoff=c(17, 19), prob=c(.05, .95), col=c("mediumblue", "red3"), 
#' 					alpha2=.5, HRLine=list(col=c("mediumblue", "red3")))
#' }
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}

precision_plot <- function(	vfp, model.no=NULL, cutoff, prob=c(.05, .5, .95), col=c("blue", "black", "red"), 
		Cutoff=list(), Title=list(), Xlabel=list(), Ylabel=list(), HRLine=list(),
		Legend=FALSE, nclass=-1,  BG="gray90", digits=3, alpha=.15, alpha2=0, 
		xlim=NULL, col.grid="white", Nrand=1e6)
{
	
	stopifnot(is(vfp, "VFP"))
	stopifnot(is.numeric(cutoff))
	Nco <- length(cutoff)
	if(!Nco %in% 1:2)
		stop("One or two numeric cutoff(s) must be specified!")
	col <- rev(col)					# reverse order to match expectation
	if(length(prob) > length(col))											
		col <- rep(col, ceiling(length(prob)/length(col)))
	
	p 		<- c(.005, seq(.01, .12, .01), seq(.15, .85, .05), seq(.88, .99, .01), .995)
	p		<- unique(sort(c(p, prob)))
	
	if(Nco == 1)
		res1	<- derive_cx(vfp, model.no=model.no, cutoff=cutoff, Cx=p)
	else
	{
		res1	<- derive_cx(vfp, model.no=model.no, cutoff=cutoff[1], Cx=p)
		res2	<- derive_cx(vfp, model.no=model.no, cutoff=cutoff[2], Cx=p)
	}
	# use actual probabilities, in case that mean would < 0 C0.01 cannot be reached, indicate actual Cx-value
	res1 <- cbind(res1, Actual.p.rounded=round(res1[,"Actual.p"]*100, 2))
	
	prob	<- sort(prob, decreasing=FALSE)
	indMin	<- which(p == prob[1])
	indMax	<- which(p == prob[length(prob)])
	prob	<- sort(prob, decreasing=TRUE)
	
	if(is.null(xlim))
	{
		xlim	<- res1[indMin, "Mean"]-4*res1[indMin, "SD"]
		xlim	<- c(xlim, 	if(Nco == 1)
							res1[indMax, "Mean"]+4*res1[indMax, "SD"]
						else
							res2[indMax, "Mean"]+4*res2[indMax, "SD"])
	}
	ylim	<- c(0, 1)
	
	old.par <- par(mar=c(5,6,5,ifelse(!Legend, 4, 14)))
	
	plot(xlim, ylim, axes=FALSE, xlab="", ylab="", main="", type="n", xaxs="i", yaxs="i")
	USR <- par("usr")
	PLT <- par("plt")
	
	rect(xlim[1], ylim[1], xlim[2], ylim[2], col=BG, border="white")
	add_grid(x=pretty(xlim, 20), y=seq(0,1,.05), col=col.grid)
	axis(1, at=pretty(xlim, 10), mgp=c(3, .75, 0), col.ticks="gray60")
	axis(2, las=1, mgp=c(3, .75, 0), col.ticks="gray60")
	
	Means 	<- NULL
	scl		<- res1[which(p == tail(prob, 1)),]
	scl		<- dnorm(scl["Mean"], mean=scl["Mean"], sd=scl["SD"])
	p.idx	<- NULL
	for(i in 1:length(prob))
	{
		tmpInd  <- which(p == prob[i])
		p.idx 	<- c(p.idx, tmpInd)				# 
		if(Nco == 1)
			tmp.res <- res1
		else {
			if(prob[i] < 0.5)
				tmp.res <- res1
			else
				tmp.res <- res2
		}
		tmpData <- rnorm(Nrand, tmp.res[tmpInd, "Mean"], tmp.res[tmpInd, "SD"])
		Means 	<- c(Means, tmp.res[tmpInd, "Mean"])
		
		tmpNclass <- nclass
		
		if(nclass<=10)			# automatically determine number of classes in the histogram
			tmpNclass <- ceiling(	150 * 
							(qnorm(.999, res1[tmpInd, "Mean"], res1[tmpInd, "SD"]) -
								qnorm(.001, res1[tmpInd, "Mean"], res1[tmpInd, "SD"]))	/
							(USR[2] - USR[1]))
		
		h 	<- hist(tmpData, plot=FALSE, n=tmpNclass )
		h$density <- h$density / (scl * 1.5)			# scale normal distribution to half the height of the plot
		
		plot(	h, las=1, xlim=xlim, ylim=ylim, col=as_rgb(col[i], alpha), 
				border="white",add=TRUE, freq=FALSE)
		if(Nco == 1)	
			idx <- which(h$breaks >= cutoff[1])
		else {
			if(prob[i] < 0.5)
				idx <- which(h$breaks >= cutoff[1])
			else
				idx <- which(h$breaks >= cutoff[2])
		}		
		
		for(j in idx)
			rect(h$breaks[j], 0, h$breaks[j+1], h$density[j], col=as_rgb(col[i], alpha2), border="white")
	}
	
	abline(v=Means, h=prob, col="gray60", lty=2, lwd=2)
	#mtext(side=3, at=Means, line=0, col="gray60", text=paste0("C", res1[c(indMax, indMin), "Actual.p.rounded"]))#paste0("C", 100*prob))
	mtext(side=3, at=Means, line=0, col="gray60", text=paste0("C", res1[p.idx, "Actual.p.rounded"]))
	mtext(side=4, at=prob,  line=.2, col="gray60", text=paste0(100*prob, "%"), las=1)
	mtext(side=1, at=Means, line=1.5, col="gray60", text=signif(Means, digits=digits))
	
	Cutoff.def <- list(col="magenta", lty=3, lwd=3)
	Cutoff.def[names(Cutoff)] <- Cutoff
	Cutoff <- Cutoff.def
	Cutoff$v <- cutoff
	do.call("abline", Cutoff)
	if(!is.null(Cutoff))
	{
		if(Nco == 1)
			mtext(side=3, line=.75, at=cutoff, text="Cutoff", col=Cutoff$col)
		else
		{
			mtext(side=3, line=.75, at=cutoff[1], text=expression("Cutoff"[1]), col=Cutoff$col)
			mtext(side=3, line=.75, at=cutoff[2], text=expression("Cutoff"[2]), col=Cutoff$col)
		}
	}
	
	Title.def <- list(font=4, cex=1.75, line=2, text="Hit Rate Plot Based On Precision Profile")
	Title.def[names(Title)] <- Title
	Title <- Title.def
	Title$side <- 3
	
	Xlabel.def <- list(font=3, cex=1.5, line=3, text="Internal Continuous Response (ICR)")
	Xlabel.def[names(Xlabel)] <- Xlabel
	Xlabel <- Xlabel.def
	Xlabel$side <- 1
	
	Ylabel.def <- list(font=3, cex=1.5, line=3, text="Hit Rate / Probability [%]")
	Ylabel.def[names(Ylabel)] <- Ylabel
	Ylabel <- Ylabel.def
	Ylabel$side <- 2
	
	do.call("mtext", Title)
	do.call("mtext", Xlabel)
	do.call("mtext", Ylabel)
	
	HRLine.def <- list(lwd=3, col="gray40", lty=1)
	HRLine.def[names(HRLine)] <- HRLine
	HRLine <- HRLine.def
	HRLine$x <- c(xlim[1], res1[,"Mean"], xlim[2])
	HRLine$y <- c(0, p, 1)
	do.call("lines", HRLine)
	if(Nco == 2)
	{
		HRLine$x <- c(xlim[1], res2[,"Mean"], xlim[2])
		if(length(HRLine$col) > 1)
			HRLine$col <- HRLine$col[2]
		do.call("lines", HRLine)
	}
	if(Legend)
	{
		Fill 	<- as_rgb(col[1:length(col)], alpha)
		if(alpha2 > 0)
			Fill <- c(Fill, as_rgb(col[1:length(col)], ifelse(alpha+alpha2>1, 1, alpha+alpha2)))
		
		Legend 	<- c(	paste0("~N(C", formatC(100*prob, format="s", width=3, flag="-"), " ; SD) < Cutoff"),
				paste0("~N(C", formatC(100*prob, format="s", width=3, flag="-"), " ; SD) > Cutoff") )
		LTY 	<- c(rep(0, length(Fill)), HRLine$lty)
		COL		<- HRLine$col
		LWD		<- HRLine$lwd
		Legend	<- c(Legend, paste0("Hitrate", ifelse(Nco==2,1,"")))
		if(Nco == 2)
			Legend	<- c(Legend, "Hitrate2")
		
		Fill 	<- c(Fill, NA)
		
		if(!is.null(Cutoff))
		{
			LTY 	<- c(LTY, Cutoff$lty)
			COL 	<- c(COL, Cutoff$col)
			LWD 	<- c(LWD, Cutoff$lwd)
			Legend 	<- c(Legend, "Cutoff")
			Fill 	<- c(Fill, NA)
		}
		
		
		legend_rm(	x="center", fill=Fill, legend=Legend, y.intersp=2, 
				lty=LTY, lwd=LWD, col=COL, bg=BG, 
				border=c(	rep("white", length(which(!is.na(Fill)))), 
						rep(BG, length(which(!is.na(Fill))))) )
	}
	if(Nco == 1)
		res <- res1
	else
		res <- list(cutoff1=res1, cutoff2=res2)
	
	box(col="black")
	
	par(old.par)
	
	invisible(res)
}



#' Determine C5 and C95 or any Concentration Cx.
#' 
#' This function makes use of a precision profile. The concentration is sought
#' at which 100 * 'Cx'\% of the measurements lie above 'cutoff' theoretically
#' as each X-value corresponds to a normal distribution with mean=X and SD as
#' read off the precision profile. In case of e.g. "C5" exactly 5\% will be
#' above cutoff, whereas for "C95" 95\% will  be larger than cutoff. This follows
#' the CLSI EP12 guideline whenever an internal continuous result (ICR) is 
#' available and measurement imprecision can be assumed to be normally distributed. 
#' The CLSI EP12 recommends to base derivation of C5 and C95 on the results of 
#' intermediate precision analyses using multiple samples. This includes between-day 
#' and between-run as additional variance components besides repeatability.
#' 
#' @param vfp		(VFP) object representing a precision profile to be used
#' @param model.no	(integer) specifying which model to use, if NULL the "best"
#' 					model will be used automatically
#' @param start		(numeric) start concentration, e.g. for C5 smaller than r,
#' 					for C95 larger than R
#' @param cutoff	(numeric) the cutoff of to be used
#' @param Cx		(numeric) the probability, e.g. for C5 use 0.05 and for 
#' 					C95 use 0.95
#' @param tol		(numeric) tolerance value determining when the iterative
#' 					bisections search can terminate, i.e. when the difference
#' 					becomes smalle enough
#' @param plot		(logical) TRUE = plot the result
#' 
#' @return (numeric) Mean and SD of concentration Cx
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @aliases deriveCx
#' 
#' @examples 
#' \dontrun{
#' # perform variance component analysis
#' library(VCA)
#' data(VCAdata1)
#' # perform VCA-anaylsis
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by="sample")
#' # transform list of VCA-objects into required matrix
#' mat <- get_mat(lst)		# automatically selects "total"
#' mat
#' # fit all models batch-wise
#' res <- fit_vfp(model.no=1:9, Data=mat)
#' # now search for the C5 concentration
#' derive_cx(res, start=15, cutoff=20, Cx=0.05, plot=TRUE)
#' derive_cx(res, start=25, cutoff=20, Cx=0.95, plot=TRUE)
#' derive_cx(res, start=25, cutoff=20, Cx=0.25, plot=TRUE)
#' derive_cx(res, start=25, cutoff=20, Cx=0.75, plot=TRUE)
#' 
#' #
#' p <- c(seq(.01, .12, .01), seq(.15, .85, .05), seq(.88, .99, .01))
#' system.time(x <- derive_cx(res, Cx=p, cutoff=20))
#' }

derive_cx <- function(vfp, model.no=NULL, start=NULL, cutoff=NULL, Cx=.05, tol=1e-6, plot=FALSE)
{
	stopifnot(is(vfp, "VFP"))
	stopifnot(is.numeric(cutoff))
	if(is.null(start))
		start <- cutoff
	stopifnot(is.numeric(start))
	if(length(Cx) > 1)
	{
		res <- t(sapply(Cx, derive_cx, vfp=vfp, model.no=model.no, start=start, cutoff=cutoff))
		res <- cbind(Cx=Cx, res)
		return(res)
	}
	stopifnot(Cx > 0 && Cx < 1)
	Prob <- 1 - Cx
	Mean <- start
	
	iter <- 1
	
	while(1)
	{
		SD 		<- predict(vfp, model.no=model.no, newdata=Mean, type="sd")$Fitted
		tmpQ 	<- qnorm(p=Prob, mean=Mean, sd=SD)
		Diff 	<- tmpQ - cutoff
		
		if(abs(Diff) < tol)			# tolerable difference to cutoff
		{
			if(plot)
			{
				h <- hist(rnorm(1e5, mean=Mean, sd=SD), nclass=50, main="", freq=FALSE, las=1, col="gray80",
						border="white", 
						xlab=paste0("Normal Distribution ~N(", round(Mean, 2), ",", round(SD,4),")"),
						font.lab=3, cex.lab=1.5)
				
				idx <- which(h$breaks >= cutoff)
				for(i in idx)
					rect(h$breaks[i], 0, h$breaks[i+1], h$density[i], col="gray50", border="white")
				mtext(side=3, line=1, text=bquote(C[.(100*Cx)] == .(round(Mean, 2))), at=Mean, cex=1.25)
				abline(v=cutoff, col="blue", lwd=3)
				abline(v=qnorm(p=Prob, mean=Mean, sd=SD), col="red", lwd=3, lty=2)	
				abline(v=Mean, lwd=3, lty=3)
				legend( "right", col=c("black", "blue", "red"), lty=c(3, 1,2), lwd=c(3,3,3),
						legend=c(as.expression(bquote(C[.(100*Cx)])), "Cutoff", paste0(100*Prob, "% Quantile")), box.lty=0)
				legend( "topright", fill=c("gray80", "gray50"), border="white", 
						legend=c("< Cutoff          ", "> Cutoff          "), box.lty=0)
			}
			
			res <- c(Mean=Mean, SD=SD, Actual.p=pnorm(cutoff, Mean, SD, lower.tail=FALSE))
			
			return(res)
		}
		
		if(Diff < 0)
			Mean <- Mean + abs(Diff)/2
		else
			Mean <- Mean - abs(Diff)/2
		
		# negative concentration not defined
		if(Mean < 0)
			Mean <- 0
		
		iter <- iter + 1
		if(iter == 500)
		{
			res <- c(Mean=Mean, SD=SD, Actual.p=pnorm(cutoff, Mean, SD, lower.tail=FALSE))
			#warning(paste0("Max iterations (",iter,") reached without convergence!"))
			return(res)
		}
	}	
}

