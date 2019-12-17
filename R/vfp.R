# TODO: Add comment
# 
# Author: schueta6
###############################################################################


#' Internal Function Model 2.
#' @param x		(numeric) parameter

powfun2simple <- function(x) 
{
	list(
			predictors=list(beta1 = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( predictors[1],"*",variables[1],"^2")
			}
	)
}
class(powfun2simple) <- "nonlin"

#' Internal Function Model 3.
#' @param x		(numeric) parameter

powfun3 <- function(x)
{
	cutval 	<- 0.95
	xmax	<- max(x)
	
	list(
			predictors=list(beta1 = 1, beta2 = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( "exp(",predictors[1],")*(1+(exp(",predictors[2],")-",cutval,")*",variables[1],"/",xmax,")")
			}
	)
}
class(powfun3) <- "nonlin"

#' Internal Function Model 3.
#' @param x		(numeric) parameter

powfun3simple <- function(x)
{
	cutval <- 0.95
	list(
			predictors=list(beta1 = 1, beta2 = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( predictors[1],"+",predictors[2],"*",variables[1],"^2")
			}
	)
}
class(powfun3simple) <- "nonlin"



#' Internal Function Model 4.
#' @param x			(numeric) parameter 1
#' @param potenz	(numeric) parameter 2

powfun4 <- function(x, potenz)
{
	cutval 	<- 0.95
	xmax	<- max(x)
	list(
			predictors=list(beta1 = 1, beta2 = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( "exp(",predictors[1],")*(1+(exp(",predictors[2],")-",cutval,")*",variables[1],"/",xmax,")^",potenz)
			}
	)
}
class(powfun4) <- "nonlin"


#' Internal Function Model 4.
#' @param x			(numeric) parameter 1
#' @param potenz	(numeric) parameter 2

powfun4simple <- function(x, potenz)
{
	list(
			predictors=list(beta1 = 1, beta2 = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( "(",predictors[1],"+",predictors[2],"*",variables[1],")^",potenz)
			}
	)
}
class(powfun4simple) <- "nonlin"


#' Internal Function Model 5.
#' @param x			(numeric) parameter 1

powfun5 <- function(x)
{
	cutval 	<- 0.95
	xmax	<- max(x)	
	
	list(
			predictors=list(beta1 = 1, beta2 = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( "exp(",predictors[1],")*(1+(exp(",predictors[2],")-",cutval,")*",variables[1],"/",xmax,")")
			}
	)
}
class(powfun5) <- "nonlin"

#' Internal Function Model 5.
#' @param x		(numeric) parameter 1
#' @param K		(numeric) parameter 2

powfun5simple <- function(x,K)
{
	list(
			predictors=list(beta1 = 1, beta2 = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( predictors[1],"+",predictors[2],"*",variables[1],"^", K)
			}
	)
}
class(powfun5simple) <- "nonlin"


#' Internal Function Model 6.
#' @param x			(numeric) parameter 1

powfun6 <- function(x)
{
	list(
			predictors=list(beta1 = 1, beta2 = 1,beta3=1, pot = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( "exp(",predictors[1],")*(1+(atan(",predictors[2],")/pi-0.5)*",variables[1],
						"*exp(",predictors[3],")*(1+exp(",predictors[4],")-(",variables[1],"*exp(",predictors[3],
						"))^exp(",predictors[4],"))/exp(",predictors[4],"))")
			}
	)
}
class(powfun6) <- "nonlin"

#' Internal Function Model 6.
#' @param x			(numeric) parameter 1

powfun6simple <- function(x)
{
	list(
			predictors=list(beta1 = 1, beta2 = 1,beta3=1, pot = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( predictors[1],"+",predictors[2],"*",variables[1],"+",predictors[3],"*",variables[1],"^",predictors[4])
			}
	)
}
class(powfun6simple) <- "nonlin"


#' Internal Function Model 7.
#' @param x			(numeric) parameter 1

powfun7 <- function(x)
{
	cutval <- 0.95
	xmax	<- max(x)
	
	list(
			predictors=list(beta = 1, beta2=1, pot = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( "exp(",predictors[1],")*(1+(exp(",predictors[2],")-",cutval,")*(",variables[1],"/",xmax,")^((0.1+10*exp(",predictors[3],"))/(1+exp(",predictors[3],"))))")
			}
	)
}
class(powfun7) <- "nonlin"


#' Internal Function Model 7.
#' @param x			(numeric) parameter 1

powfun7simple <- function(x)
{
	list(
			predictors=list(beta = 1, beta2=1, pot = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( predictors[1],"+",predictors[2],"*",variables[1],"^",predictors[3])
			}
	)
}
class(powfun7simple) <- "nonlin"


#' Internal Function Model 8.
#' @param x			(numeric) parameter 1
#' @param C			(numeric) parameter 2
#' @param signJ		(numeric) parameter 3

powfun8 <- function(x, C,signJ)
{
	cutval <- 0.95
	
	list(
			predictors=list(beta1 = 1, beta2 = 1, beta3 = 1), 
			variables=list(substitute(x/C)),
			term=function(predictors,variables){
				paste( "exp(",predictors[1],")*(1+(exp(",predictors[2],")-",cutval,")*",variables[1],")^(",signJ,"*(0.1+10*exp(",predictors[3],"))/(1+exp(",predictors[3],")))")
			}
	)
}
class(powfun8) <- "nonlin"


#' Internal Function Model 8.
#' @param x			(numeric) parameter 1

powfun8simple <- function(x)
{
	list(
			predictors=list(beta1 = 1, beta2 = 1, pot = 1), 
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( "(",predictors[1],"+",predictors[2],"*",variables[1],")^",predictors[3],sep=" ")
			}
	)
}
class(powfun8simple) <- "nonlin"

#' Internal Function Model 9.
#' @param x			(numeric) parameter 1

powfun9simple <- function(x)
{
	list(
			predictors=list(beta1 = 1,  pot = 1),
			variables=list(substitute(x)),
			term=function(predictors,variables){
				paste( predictors[1],"*",variables[1],"^",predictors[2],sep=" ")
			}
	)
}
class(powfun9simple) <- "nonlin"


#' Condition-Handling Without Losing Information.
#' 
#' Function is intented to wrap expressions provided and catching
#' all potentially useful information generated by the wrapped expression, i.e.
#' errors, warnings, and messages.
#' 
#' @param expr		(expression) for which exception handling should be provided
#' @param file		(character) string specifying a file to which all captured output
#' 					shall be written
#' 
#' @return 	(list) with element "result", "status" (0 = no warnings, no errors), 1 = warnings
#' 			were caught, 2 = errors were caught no result generated, "warnings", "errors", 
#' 			"messages"
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' @examples 
#' conditionHandler(warning("This is a warning!"))
#' f <- function(expr){warning("This a warning!"); eval(expr)}
#' conditionHandler(f(1/2))
#' conditionHandler(stop("This is an error!"))
#' conditionHandler(1/"a")

conditionHandler <- function(expr,  file=NULL) 
{
	Warnings <- Errors <- Messages <- NULL
	status   <- 0
	# handle warnings
	WHandler <- function(w) 
	{
		status <<- 1
		Warnings <<- c(Warnings, list(w))
		invokeRestart("muffleWarning")		
	}
	# handle messages
	MHandler <- function(m)
	{
		Messages <<- c(Messages, list(m))
		invokeRestart("muffleMessage")		
	}
	# handle errors
	if(!is.null(file) && file.exists(file))
	{
		capture.output(
				result <- try(withCallingHandlers(expr, warning = WHandler, message=MHandler), silent=TRUE),
				file=file, append=TRUE)
	}
	else
	{
		result <- try(withCallingHandlers(expr, warning = WHandler, message=MHandler), silent=TRUE)
	}
	
	if(class(result[[1]]) == "try-error") # || class(result) == "try-error")
	{		
		Errors <- attr(result, "condition")$message
		result <- NA
		status <- 2
	}
	res <- list(result = result, status=status, warnings = Warnings, errors = Errors, messages = Messages)
	
	res
} 




#' Estimate (Im)Precision-Profiles Modeling the Relationship 'Var ~ Mean'.
#' 
#' This function fits one out of ten or any subset of ten non-linear functions at once. This is done on 
#' precision data consisting of information about the variance, concentration at which these variance
#' was observed and the respective degrees of freedom. Provided data must contain at least three column with
#' these information. There are following variance-functions covered: \cr
#' \itemize{
#'	 \item{constant variance}{ \eqn{sigma^2}}
#'	 \item{constant CV}{\eqn{ sigma^2=beta_1*u^2 }}
#'	 \item{mixed constant, proportional variance}{ \eqn{sigma^2=beta_1+beta*u^2}}
#'	 \item{constrained power model, constant exponent}{ \eqn{sigma^2=(beta_1+beta_2*u)^K}}
#'	 \item{alternative constrained power model}{ \eqn{sigma^2=beta_1+beta_2*u^K}}
#'	 \item{alternative unconstrained power model for VF's with a minimum}{ \eqn{sigma^2=beta_1+beta_2*u+beta_3*u^J}}
#'	 \item{alternative unconstrained power model}{ \eqn{sigma^2=beta_1+beta_2*u^J}}
#'	 \item{unconstrained power model (default model of Sadler)}{ \eqn{sigma^2=(beta_1 + beta_2 * u)^J}}
#' 	 \item{CLSI EP17 similar model}{ \eqn{sigma^2=beta_1 * u^J}}
#'	 \item{Exact CLSI EP17 model (fit by linear regression on logarithmic scale)}{ \eqn{cv=beta_1 * u^J}}
#' }
#' Fitting all ten models is somehow redundant if constant 'C' is chosen to be equal to 2, since models 3 and 5 are
#' equivalent and these are constrained versions of model 7 where the exponent is also estimated. The latter also applies to model
#' 4 which is a constrained version of model 8. Nevertheless, since computation time is not critical here for typical
#' precision-profiles (of in-vitro diagnostics precision experiments) we chose to offer batch-processing as well.
#' During computation, all models are internally reparameterized so as to guarantee that the variance function is positive in the
#' range 'u' from 0 to 'u_max'. In models 7 and 8, 'J' is restricted to 0.1<J<10 to avoid the appearance of sharp hooks. 
#' Occasionally, these restrictions may lead to a failure of convergence. This is then a sign that the model parameters
#' are on the boundary and that the model fits the data very badly. This should not be taken as reason for concern. 
#' It occurs frequently for model 6 when the variance function has no minimum, which is normally the case. 
#' 
#' Functions \code{\link{predict.VFP}} and \code{\link{predictMean}} are useful to make inference based on a fitted model.
#' It is possible to derive concentrations at which a predefined variability is reached, which is sometimes referred to as
#' "functional sensitivity" and/or "limit of quantitation" (LoQ). Funtion \code{\link{predictMean}} returns the fitted value
#' at which a user-defined variance ("vc"), SD or CV is reached with its corresponding 100(1-alpha)\% CI derived from the CI of
#' the fitted model. The plotting method for objects of class 'VFP' can automatically add this information to a plot using 
#' arguments 'Prediction' and 'Pred.CI' (see \code{\link{plot.VFP}} for details. Function \code{\link{predict.VFP}} makes 
#' predictions for specified mean-values based on fitted models.  
#' 
#' @param Data			(data.frame, matrix) containing mean-values, estimated variances and degrees of freedom for each sample
#' @param model.no		(integer) in 1:10, may be any combination of these integer-values specifying models 1 to 9 which
#' 						are consequently fitted to the data
#' @param K				(numeric) value defining the constant used in models 4 and 5
#' @param startvals		(numeric) vector of start values for the optimization algorithm, only used if a single model
#' 						is specified by the user, otherwise, start values will be sought automatically
#' @param quiet			(logical) TRUE = all output messages (warnings, errors) and regular console output is suppressed and can
#' 						be found in elements "stderr" and "stdout" of the returned object
#' @param col.mean		(character) string specifying a column/variable in 'Data' containing mean-values
#' @param col.var		(character) string specifying a column/variable in 'Data' containing the variances
#' @param col.df		(character) string specifying a column/variable in 'Data' containing the degrees of freedom
#' @param col.sd		(character) string (optional) specifying a column/variable in 'Data' containing the SD-values,
#' 						used for model 10 to derive more precice CV-value compared to derivation from 'col.var' in case
#' 						'col.cv' is not specified directly
#' @param col.cv		(character) string (optional) specifying a column/variable in 'Data' containing the CV-values,
#' 						if missing, first 'col.sd' is checked, if missing 'col.var' used to derive per-sample CV-values
#' @param minVC			(numeric) value assigned to variances being equal to zero, defaults to NA, which results in removing
#' 						these observations; could also be set to the smallest possible positive double (\code{.Machine$double.eps}) 
#' @param ...			additional parameters passed forward, e.g. 'vc' of function \code{\link{getMat.VCA}} for selecting a 
#' 						specific variance component in case of 'Data' being a list of 'VCA'-objects (see examples)
#' 
#' @return (object) of class 'VFP' with elements:\cr
#' \item{Models}{objects of class 'gnm' representing the fitted model}
#' \item{RSS}{residual sum of squares for each fitted model}
#' \item{AIC}{the Akaike information criterion for each fitted model}
#' \item{Deviance}{the deviance for each fitted model}
#' \item{Formulas}{formula(s) as expression for each fitted model}
#' \item{Mu.Func}{function(s) to transform mean value to re-parameterized scale}
#' \item{Mu}{list of transformed mean values}
#' \item{Data}{the original data set provided to fit model(s)}
#' \item{K}{constant as specified by the user; must be >0}
#' \item{startvals}{start values as specified by the user}
#' \item{errors}{messages of all errors caught}
#' \item{output}{if 'quiet=TRUE' all output that was redirected to a file}
#' \item{warning}{messages of all warnings caught}
#' \item{notes}{all other notes caught during execution}
#' 
#' @author 	Florian Dufey \email{florian.dufey@@roche.com}, 
#' 			Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @seealso \code{\link{plot.VFP}}, \code{\link{predict.VFP}}, \code{\link{predictMean}}
#' 
#' @examples 
#' \donttest{
#' # load VCA-package and data
#' library(VCA)
#' data(VCAdata1)
#' # perform VCA-anaylsis
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by="sample")
#' # transform list of VCA-objects into required matrix
#' mat <- getMat.VCA(lst)		# automatically selects "total"
#' mat
#' 
#' # fit all 9 models batch-wise
#' res <- fit.vfp(model.no=1:10, Data=mat)
#' 
#' # if 'mat' is not required for later usage, following works
#' # equally well
#' res2 <- fit.vfp(lst, 1:10)
#' 
#' # plot best-fitting model
#' plot(res)
#' plot(res, type="cv")
#' plot(res, type="cv", ci.type="lines", ci.col="red",
#' 		Grid=list(col="wheat"), Points=list(pch=2, lwd=2, col="black"))
#' 
#' # now derive concentation at which a specific reproducibility-
#' # imprecision of 10\% is reached and add this to the plot
#' pred <- plot(res, type="cv", ci.type="band", 
#' 				ci.col=as.rgb("red", .25), Grid=list(col="orange"), 
#' 				Points=list(pch=2, lwd=2, col="black"),
#' 				Prediction=list(y=10, col="red"), Pred.CI=TRUE)
#' 
#' # (invisibly) returned object contains all relevant information
#' pred
#' 
#' # same for repeatability
#' mat.err <- getMat.VCA(lst, "error")
#' res.err <- fit.vfp(1:10, Data=mat.err)
#' 
#' # without extracting 'mat.err'
#' res.err2 <- fit.vfp(lst, 1:10, vc="error")
#' 
#' plot(res.err)
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
#' rep.CA19_9 <- getMat.VCA(fits.CA19_9, "error")
#' 
#' # extract reproducibility
#' repro.CA19_9 <- getMat.VCA(fits.CA19_9, "total")
#' 
#' # extract intermediate-precision (within-lab)
#' ip.CA19_9 <- getMat.VCA(fits.ip.CA19_9, "total")
#' 
#' # fit model (a+bX)^C (model 8) to all three matrices
#' 
#' mod8.repro 	<- fit.vfp(repro.CA19_9, 8)
#' mod8.ip		<- fit.vfp(ip.CA19_9, 8)
#' mod8.rep		<- fit.vfp(rep.CA19_9, 8)
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
#' plot(	mod8.rep, type="cv", add=TRUE, ci.type="none",
#' 		Points=list(pch=17, col="darkorchid3", cex=1.5),
#' 		Line=list(col="darkorchid3", lwd=3), log="x")
#' 
#' # add legend to right margin
#' legend.rm( x="center", pch=15:17, col=c("blue", "deepskyblue", "darkorchid3"),
#' 		cex=1.5, legend=c("Reproducibility", "Within-Lab Precision", "Repeatability"),
#' 		box.lty=0)
#' #' }


fit.vfp <- function(Data, model.no = 7, K=2, startvals=NULL, quiet=T, 
		col.mean="Mean", col.var="VC", col.df="DF", 
		col.sd=NULL, col.cv=NULL, minVC=NA, ...) #.Machine$double.eps, ...)
{
	
	Call <- match.call()
	stopifnot(model.no %in% 1:10)
	stopifnot(K>0)
	stopifnot(is.data.frame(Data) || is.matrix(Data) || class(Data) == "list" && all(sapply(Data, class) == "VCA"))
	if(class(Data) == "list" && all(sapply(Data, class) == "VCA"))
		Data <- getMat.VCA(Data, ...)
	if(is.matrix(Data))
		Data <- as.data.frame(Data)
	
	cn	<- colnames(Data)
	stopifnot(all(c(col.mean, col.var, col.df) %in% cn))
	
	stopifnot(is.numeric(Data[,col.mean]))
	stopifnot(is.numeric(Data[,col.var]))
	stopifnot(is.numeric(Data[,col.df]))
	stopifnot(is.null(col.sd) || is.character(col.sd) && is.numeric(Data[,col.sd]))
	stopifnot(is.null(col.cv) || is.character(col.cv) && is.numeric(Data[,col.cv]))
	
	Data[,col.mean]	<- as.numeric(Data[,col.mean])
	Data[,col.var]  <- as.numeric(Data[,col.var]) 
	Data[,col.df]   <- as.numeric(Data[,col.df])  
	
	ind.NegVC <- which(Data[,col.var] <= 0)
	
	if(length(ind.NegVC) > 0)
	{
		message("Variance(s) <= 0 detected! This/these will be set to 'minVC'!")
		Data[ind.NegVC,col.var] <- minVC
	}
	
	if(!is.null(col.sd))
	{
		Data[,col.sd] <- as.numeric(Data[,col.sd])
		colnames(Data)[which(cn == col.sd)] <- "SD"
	}
	if(!is.null(col.cv))
	{
		Data[,col.cv] <- as.numeric(Data[,col.cv])
		colnames(Data)[which(cn == col.cv)] <- "CV"	
	}
	
	if(quiet)
	{
		file.create("./stdout.log")
	}
	
	errors <- warnings <- messages <- notes <- NULL
	
	skip5 <- FALSE
	if(all(c(3,5) %in% model.no) && K==2)
	{
		skip5 <- TRUE								# models 3 and 5 are identical in case K=2
		if(!quiet)
			message("Model 5 will not be fitted because it is identical to model 3 with 'K=2'!")
		notes <- c(notes, "Model 5 will not be fitted because it is identical to model 3 with 'K=2'!")
	}
	
	if(length(model.no) > 1)
		startvals <- NULL
	
	if(col.mean != "Mean")
	{
		if("Mean" %in% cn)
		{
			Data <- Data[,-which(colnames(Data) == "Mean")]
			cn	 <- colnames(Data)
		}
		colnames(Data)[which(cn == col.mean)] <- "Mean"
	}
	if(col.var != "VC")
	{
		if("VC" %in% cn)
		{
			Data <- Data[,-which(colnames(Data) == "VC")]
			cn	 <- colnames(Data)
		}
		colnames(Data)[which(cn == col.var)] <- "VC"
	}
	if(col.df != "DF")
	{
		if("DF" %in% cn)
		{
			Data <- Data[,-which(colnames(Data) == "DF")]
			cn	 <- colnames(Data)
		}
		colnames(Data)[which(cn == col.df)] <- "DF"
	}
	
	ind.NA <- which(apply(Data[c("Mean", "VC", "DF")], 1, function(x) any(is.na(x))))
	if(length(ind.NA) > 0)
	{
		Data <- Data[-ind.NA,,drop=FALSE]
		message(paste(length(ind.NA), "observation(s) with at least one NA value were removed!"))
	}
	
	Data 		<- subset(Data,"Mean">0)
	Data		<- subset(Data,"VC">0)
	pweights 	<- Data[,"DF"]/2 										# prior weight parameters
	Mean 		<- Data[,"Mean"]
	RSS 		<- Deviance <- AIC <- numeric(10)
	
	Formulas	<- c( 	"sigma^{2}==beta[1]",									# model 1
			"sigma^{2}==beta[1]*u^{2}",								# model 2
			"sigma^{2}==beta[1]+beta[2]*u^2",						# model 3
			paste("sigma^{2}==(beta[1]+beta[2]*u)^{",K,"}"),		# model 4, replace K by acutal value of K
			paste("sigma^{2}==beta[1]+beta[2]*u^{",K,"}"),			# model 5,                - " -
			"sigma^{2}==beta[1]+beta[2]*u+beta[3]*u^{J}",			# model 6
			"sigma^{2}==beta[1]+beta[2]*u^{J}",						# model 7
			"sigma^{2}==(beta[1]+beta[2]*u)^{J}",					# model 8
			"sigma^{2}==beta[1]*u^{J}",								# model 9
			"cv==beta[1]*u^{J}")									# model 10 (true CLSI EP17 model)
	
	names(RSS) 	<- names(Deviance) <- names(AIC) <- paste0("Model_", 1:10)
	
	res.gnm1 	<- res.gnm2 <- res.gnm3 <- res.gnm4 <- res.gnm5 <- res.gnm6 <- res.gnm7 <- res.gnm8 <- res.gnm9 <- res.mod10 <- NULL
	mittel		<- Mu <- mu.fun <- vector("list", length=9)	
	eps			<- 1.0e-07
#	cutval		<- 0.95 #determines lower boundary in models, values between 0 and 1 are possible
	# find a crude analytical model for the calculation of the start weights
	imin=0
	for(i in 0:2){
		if (i>0) {mindevalt<-mindev}
		mindev <- sum((Data[,"DF"]*(1-weighted.mean(Data[,"VC"]/Data[,"Mean"]^i,pweights)*Data[,"Mean"]^i/Data[,"VC"])^2))
		if (i>0){
			if(mindev<mindevalt){imin=i}
		}
	}
	startVC    <- weighted.mean(Data[,"VC"]/Data[,"Mean"]^imin,pweights)*Data[,"Mean"]^imin+0.05*weighted.mean(Data[,"VC"],pweights)
	
	
	
	################################################################################################################################
	
	if(1 %in% model.no)
	{
		cat("\nModel 1 ")
		my.form1 	<- VC ~ 1					 				#constant VC
		startvals   <- log(weighted.mean(Data[,"VC"],pweights)) #exact solution
		tmp.res 	<- conditionHandler(gnm(formula = my.form1, family = Gamma(link = "log"), data = Data, weights = pweights,
						start=startvals, trace=TRUE), file="./stdout.log")
		if(tmp.res$status != 2)
		{
			coeffs 	<- bt.coef(tmp.res$result, model=1)#
			my.form1simple 	<-  VC ~ 1
			tmp.res <- conditionHandler(gnm(formula = my.form1simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start=coeffs, trace=TRUE,iterMax=1), file="./stdout.log")
			res.gnm1 	<- tmp.res$result
			RSS[1] 		<- sum((res.gnm1$y-res.gnm1$fitted.values)^2)
			AIC[1]		<- res.gnm1$aic
			Deviance[1] <- res.gnm1$deviance
			if(tmp.res$status == 1)
				warnings <- c(warnings, paste0("Model 1 (Warnings): ", tmp.res$warnings))
			if(!is.null(tmp.res$messages))
				messages <- c(messages, paste0("Model 1 (Messages): ", tmp.res$messages))
			
			tmp.msg <- " ... finished."
		}
		else
		{
			errors 	<- c(errors, paste0("Model 1 (Errors): ", tmp.res$errors))
			tmp.msg <- " ... could not be fitted due to errors."	
		}
		
		startvals   <- NULL
		cat(tmp.msg)
	}
	
	if(2 %in% model.no)
	{
		cat("\nModel 2 ")
		#mu.fun[[2]]	<- function(x) 2*log(x)
		mu 			<- Mu[[2]] <- 2*log(Data[,"Mean"])
		my.form2 	<- VC ~ offset(mu)  # constant CV
		startvals   <- log(weighted.mean(Data[,"VC"]/Data[,"Mean"]^2,pweights)) #exact solution
		tmp.res 	<- conditionHandler(gnm(formula = my.form2, family = Gamma(link = "log"), data = Data, weights = pweights,
						start=startvals, trace=TRUE), file="./stdout.log")
		if(tmp.res$status != 2)
		{
			coeffs 	<- bt.coef(tmp.res$result, model=2)#
			mu		<- Data[,"Mean"]^2
			my.form2simple 	<-  VC ~ powfun2simple(Mean)-1
			
			tmp.res <- conditionHandler(gnm(formula = my.form2simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start=coeffs, trace=TRUE,iterMax=0), file="./stdout.log")
			res.gnm2 	<- tmp.res$result
			RSS[2] 		<- sum((res.gnm2$y-res.gnm2$fitted.values)^2)
			AIC[2]		<- res.gnm2$aic
			Deviance[2] <- res.gnm2$deviance
			if(tmp.res$status == 1)
				warnings <- c(warnings, paste0("Model 2 (Warnings): ", tmp.res$warnings))
			if(!is.null(tmp.res$messages))
				messages <- c(messages, paste0("Model 2 (Messages): ", tmp.res$messages))
			
			tmp.msg <- " ... finished."
		}
		else
		{
			errors 	<- c(errors, paste0("Model 2 (Errors): ", tmp.res$errors))
			tmp.msg <- " ... could not be fitted due to errors."	
		}
		
		startvals   <- NULL
		cat(tmp.msg)
	}
	
	# models 3 4, and 5 are covered by models and, therefore, do not need to be fitted separately if the more
	# general models optimizing the exponent are fitted per default
	
	if(3 %in% model.no)
	{
		cat("\nModel 3 ")
		mu 				<- Mu[[3]] <- Data[, "Mean"]^2
		mumax			<- max(mu)
		startweights 	<- pweights/startVC^2
		
		if(is.null(startvals))
		{
			st <- lm(VC ~ mu, data=Data,weights=startweights)
			startvals <- t.coef(st$coefficients, Maxi=mumax,model=3)
		}
		else
		{
			startvals <- t.coef(startvals,Maxi=mumax,model=3)
		}
		my.form3	<- VC~ -1 + powfun3(mu) #quadratic model
		tmp.res 	<- conditionHandler(gnm(formula = my.form3, family = Gamma(link = "identity"), data = Data, weights = pweights,
						start=startvals, trace=TRUE), file="./stdout.log")
		
		if(tmp.res$status != 2)
		{
			coeffs 			<- bt.coef(tmp.res$result, model=3)
			my.form3simple 	<- VC ~ powfun3simple(Mean)-1
			tmp.res 		<- conditionHandler(gnm(formula = my.form3simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start=coeffs, trace=TRUE,iterMax=0), file="./stdout.log")
			if(tmp.res$status != 2)
			{				
				res.gnm3 		<- tmp.res$result
				RSS[3] 			<- sum((res.gnm3$y-res.gnm3$fitted.values)^2)
				AIC[3]			<- res.gnm3$aic
				Deviance[3] 	<- res.gnm3$deviance
				if(tmp.res$status == 1)
					warnings 	<- c(warnings, paste0("Model 3 (Warnings): ", tmp.res$warnings))
				if(!is.null(tmp.res$messages))
					messages <- c(messages, paste0("Model 3 (Messages): ", tmp.res$messages))
				
				tmp.msg <- " ... finished."
			}
			else
			{
				res.gnm3 	<- NULL
				errors 		<- c(errors, paste0("Model 3 (Errors): ", tmp.res$errors, "\nError occurred in call to 'gnm' with final parameter-estimates."))
				tmp.msg 	<- " ... could not be fitted due to errors."
			}
		}
		else
		{
			errors 	<- c(errors, paste0("Model 3 (Errors): ", tmp.res$errors))
			tmp.msg <- " ... could not be fitted due to errors."
		}
		
		startvals   <- NULL
		cat(tmp.msg)
	}
	
	if(4 %in% model.no)
	{
		cat("\nModel 4 ")
		mu			<- Mu[[4]] <- Data[,"Mean"]
		mumax=max(mu)
		startweights <- pweights/startVC^(2/K)
		VCrootK     <- Data[,"VC"]^(1/K)
		my.form4 	<- VC~-1+powfun4(mu, K) 	#fixed power model 
		if (is.null(startvals)) 
		{
			st <- lm(VCrootK ~ mu, data=Data,weights=startweights)
			startvals <- t.coef(st$coefficients,K=K, Maxi=mumax,model=4)
		}
		else
		{
			startvals <- t.coef(startvals, K=K, Maxi=mumax, model=4)
		}
		
		tmp.res <- conditionHandler(gnm(formula = my.form4, family = Gamma(link = "identity"), data = Data, weights = pweights,
						start=startvals, trace=TRUE), file="./stdout.log")
		
		if(tmp.res$status != 2)
		{
			coeffs 	<- bt.coef(tmp.res$result,K=K, model=4)#
			my.form4simple 	<- VC~-1+powfun4simple(Mean, K) #fixed power model 
			tmp.res <- conditionHandler(gnm(formula = my.form4simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start=coeffs, trace=TRUE,iterMax=0), file="./stdout.log")
			
			if(tmp.res$status != 2)
			{		
				res.gnm4	<- tmp.res$result
				RSS[4] 		<- sum((res.gnm4$y-res.gnm4$fitted.values)^2)
				AIC[4]		<- res.gnm4$aic
				Deviance[4] <- res.gnm4$deviance
				if(tmp.res$status == 1)
					warnings 	<- c(warnings, paste0("Model 4 (Warnings): ", tmp.res$warnings))
				if(!is.null(tmp.res$messages))
					messages <- c(messages, paste0("Model 4 (Messages): ", tmp.res$messages))
				
				tmp.msg <- " ... finished."
			}
			else
			{
				errors 	<- c(errors, paste0("Model 4 (Errors): ", tmp.res$errors, "\nError occurred in call to 'gnm' with final parameter-estimates."))
				tmp.msg <- " ... could not be fitted due to errors."
			}
		}
		else
		{
			res.gnm4 	<- NULL
			errors 		<- c(errors, paste0("Model 4 (Errors): ", tmp.res$errors))
			tmp.msg 	<- " ... could not be fitted due to errors."
		}
		
		startvals 	<- NULL				# re-set start values
		cat(tmp.msg)
	}
	
	if(5 %in% model.no){
		cat("\nModel 5 ")
		if(skip5)
			cat(" ... skipped!")
		else
		{
			mu 				<- Mu[[5]] <- Data[, "Mean"]^K
			mumax			<- max(mu)
			startweights 	<- pweights/startVC^2
			
			if (is.null(startvals)) 
			{
				st <- lm(VC ~ mu, data=Data,weights=startweights)
				startvals	<- t.coef(st$coefficients,Maxi=mumax, model=5)
			}
			else
			{
				startvals	<-t.coef(startvals,Maxi=mumax,Model=5)
			}
			
			my.form5 	<- VC~-1+powfun5(mu)		#const model, requires a fixed power on input
			tmp.res 	<- conditionHandler(gnm(formula = my.form5, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start=startvals, trace=TRUE), file="./stdout.log")	
			if(tmp.res$status != 2)
			{
				coeffs 	<- bt.coef(tmp.res$result,K=K, model=5)
				my.form5simple <- VC~powfun5simple(Mean,K)-1
				tmp.res <- conditionHandler(gnm(formula = my.form5simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
								start=coeffs, trace=TRUE,iterMax=0), file="./stdout.log")
				
				if(tmp.res$status != 2)
				{
					res.gnm5 <- tmp.res$result	
					RSS[5] 		<- sum((res.gnm5$y-res.gnm5$fitted.values)^2)
					AIC[5]		<- res.gnm5$aic
					Deviance[5] <- res.gnm5$deviance
					if(tmp.res$status == 1)
						warnings 	<- c(warnings, paste0("Model 5 (Warnings): ", tmp.res$warnings))
					if(!is.null(tmp.res$messages))
						messages <- c(messages, paste0("Model 5 (Messages): ", tmp.res$messages))
					
					tmp.msg <- " ... finished."
				}
				else
				{
					errors 	<- c(errors, paste0("Model 5 (Errors): ", tmp.res$errors, "\nError occurred in call to 'gnm' with final parameter-estimates."))
					tmp.msg <- " ... could not be fitted due to errors.\n"	
				}
			}
			else
			{
				res.gnm5 	<- NULL
				errors 		<- c(errors, paste0("Model 5 (Errors): ", tmp.res$errors))
				tmp.msg 	<- " ... could not be fitted due to errors.\n"	
			}
			
			startvals   <- NULL
			cat(tmp.msg)
		}
	}
	
	if(6 %in% model.no)
	{
		cat("\nModel 6 ")
		mu				<- Mu[[6]] <- Data[,"Mean"]
		my.form6 		<- VC~-1+ powfun6(mu) #linear + variable power model 
		results			<- vector("list", 5)
		par.ind 		<- 4
		res.ind			<- 1
		res.gnm6 		<- tmp <- NULL
		Parms			<- vector("list", 5)
		SVhist			<- matrix(ncol=4, nrow=0)
		startweights 	<- pweights/startVC^2		
		tmp.msg 		<- " ... could not be fitted due to errors."	
		
		if (is.null(startvals)) 				# use random start values for gnm
		{
			tempdev=1.0e100
			res.gnm6 <- NULL
			for(ldJ in 0:4)
			{
				J  			<- 2^ldJ+0.1
				muJ 		<- mu^J
				st 			<- lm(VC ~ mu+muJ, data=Data,weights=startweights)
				startvals	<-t.coef(c(st$coefficients,J),model=6)
				tmp.res 	<- conditionHandler(gnm(formula = my.form6, family = Gamma(link = "identity"), data = Data, weights = pweights,
								start=startvals, trace=TRUE), file="./stdout.log")							
				
				if(tmp.res$status != 2)
				{
					if(!is.null(tmp.res$result) && tmp.res$result$deviance<tempdev)
					{
						res.gnm6 <- tmp.res$result
						tempdev  <- res.gnm6$deviance
						if(tmp.res$status == 1)
							warnings <- c(warnings, paste0("Model 6 (Warnings): ", tmp.res$warnings))
						if(!is.null(tmp.res$messages))
							messages <- c(messages, paste0("Model 6 (Messages): ", tmp.res$messages))
						
						tmp.msg <- " ... finished."
					}
				}
				else
					errors <- c(errors, paste0("Model 6 generated following error: ", tmp.res$errors))
			}			
		}
		else									# user-specified start values
		{
			startvals <- t.coef(startvals,model=6)
			tmp.res <- conditionHandler(gnm(formula = my.form6, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start=startvals, trace=TRUE), file="./stdout.log")		
			if(tmp.res$status != 2)
			{
				res.gnm6 <- tmp.res$result
				if(tmp.res$status == 1)
					warnings <- c(warnings, paste0("Model 6 (Warnings): ", tmp.res$warnings))
				if(!is.null(tmp.res$messages))
					messages <- c(messages, paste0("Model 6 (Messages): ", tmp.res$messages))
				
				tmp.msg <- " ... finished.\n"
			}
			else
			{
				errors 	<- c(errors, paste0("Model 6 (Errors): ", tmp.res$errors))
				tmp.msg <- " ... could not be fitted due to errors."		
			}
		}
		
		if(!is.null(res.gnm6))
		{   
			coeffs 			<- bt.coef(res.gnm6, model=6)#
			my.form6simple 	<- VC~-1+powfun6simple(Mean)
			tmp.res 		<- conditionHandler(gnm(formula = my.form6simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start=coeffs, trace=TRUE, iterMax=0), file="./stdout.log")
			if(tmp.res$status != 2)
			{
				res.gnm6 		<- tmp.res$result							 
				RSS[6] 			<- sum((res.gnm6$y-res.gnm6$fitted.values)^2)
				AIC[6]			<- res.gnm6$aic
				Deviance[6] 	<- res.gnm6$deviance
			}
			else
			{
				res.gnm6 	<- NULL
				errors 		<- c(errors, paste0("Model 6 (Errors): ", tmp.res$errors, "\nError occurred in call to 'gnm' with final parameter-estimates."))
				tmp.msg 	<- " ... could not be fitted due to errors."
			}
		}
		
		startvals 	<- NULL
		cat(tmp.msg)
	}
	
	if(7 %in% model.no)
	{
		cat("\nModel 7 ")
		mu				<- Mu[[7]] <- Data[,"Mean"]
		startweights 	<- pweights/startVC^2
		tmp.msg 		<- " ... could not be fitted due to errors."
		
		if (is.null(startvals) || startvals[3]<0 ) #negative power leads negative sigma^2 for small means
		{
			tempdev		<- 1.0e100
			res.gnm7 	<- NULL
			for(ldJ in -3:3){
				J  			<- 2^ldJ
				muJ			<- mu^J
				st 			<- lm(VC ~ muJ, data=Data,weights=startweights)
				startvals   <-t.coef(c(st$coefficients,J),Maxi=max(muJ),model=7)
				my.form7 	<- VC ~ -1+powfun7(mu) 
				tmp.res  	<- conditionHandler(gnm(formula=my.form7, family=Gamma(link = "identity"), data=Data,
								weights=pweights, start=startvals, trace=TRUE), file="./stdout.log")
				if(tmp.res$status != 2)
				{
					if(!is.null(tmp.res$result) && tmp.res$result$deviance<tempdev)
					{
						res.gnm7 <- tmp.res$result
						tempdev  <- res.gnm7$deviance
						if(tmp.res$status == 1)
							warnings <- c(warnings, paste0("Model 7 (Warnings): ", tmp.res$warnings))
						if(!is.null(tmp.res$messages))
							messages <- c(messages, paste0("Model 7 (Messages): ", tmp.res$messages))
						
						tmp.msg <- " ... finished."
					}
				}
				else
					errors <- c(errors, paste0("Model 7 (Errors): ", tmp.res$errors))
			}
		}
		else
		{
			my.form7 	<- VC ~ -1+powfun7(mu) 
			startvals[3]			<- max(0.1,min(10,startvals[3]))#restrict starting values to 0.1<startvals[3] <10
			muJmax<-max(mu^startvals[3])
			startvals <- t.coef(startvals,Maxi=muJmax,model=7)
			tmp.res <- conditionHandler(gnm(formula = my.form7, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start=startvals, trace=TRUE), file="./stdout.log")
			if(tmp.res$status != 2)
			{
				res.gnm7 <- tmp.res$result
				if(tmp.res$status == 1)
					warnings <- c(warnings, paste0("Model 7 (Warnings): ", tmp.res$warnings))
				if(!is.null(tmp.res$messages))
					messages <- c(messages, paste0("Model 7 (Messages): ", tmp.res$messages))
				
				tmp.msg <- " ... finished."
			}
			else
			{
				errors 	<- c(errors, paste0("Model 7 (Errors): ", tmp.res$errors))
				tmp.msg	<- " ... could not be fitted due to errors."
			}			
		}
		if(!is.null(res.gnm7))
		{
			coeffs 	<- bt.coef(res.gnm7, model=7)#
			my.form7simple <- VC~-1+powfun7simple(Mean)
			tmp.res <- conditionHandler(gnm(formula = my.form7simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start=coeffs, trace=TRUE,iterMax=0), file="./stdout.log")
			
			if(!is.null(tmp.res))
			{
				res.gnm7 <- tmp.res$result		
				RSS[7] 			<- sum((res.gnm7$y-res.gnm7$fitted.values)^2)
				AIC[7]		<- res.gnm7$aic
				Deviance[7] <- res.gnm7$deviance
			}
			else
			{
				res.gnm7	<- NULL
				errors 		<- c(errors, paste0("Model 7 (Errors): ", tmp.res$errors, "\nError occurred in call to 'gnm' with final parameter-estimates."))
				tmp.msg		<- " ... could not be fitted due to errors."
			}	
		}
		startvals 	<- NULL
		cat(tmp.msg)
	}
	
	if(8 %in% model.no)
	{
		cat("\nModel 8 ")
		mu			<- Mu[[8]] <- Data[,"Mean"]
		C			<- 	attr(Mu[[8]], "Constant") <- max(mu)
		my.form8 	<- VC~-1+powfun8(mu, C) 
		devns 		<- rep(NA, 1e3)
		tmp.msg		<- " ... could not be fitted due to errors."
		
		if (is.null(startvals)) 		# random start values
		{
			tempdev		<- 1.0e100
			res.gnm8 	<- NULL
			for(ldJ in -3:3){
				J  				<- 2^ldJ
				signJ			<- 1
				my.form8 	<- VC~-1+powfun8(mu, C,signJ) 
				startweights 	<- pweights/startVC^(2/J)
				VCrootJ     	<- Data[,"VC"]^(1/J)
				st 				<- lm(VCrootJ ~ mu, data=Data,weights=startweights)
				startvals		<- t.coef(c(st$coefficients,signJ*J),Maxi=C,signJ=signJ,model=8)
				tmp.res 		<- conditionHandler(gnm(formula = my.form8, family = Gamma(link = "identity"), 
								data = Data, weights = pweights ,start=startvals, 
								trace=TRUE), file="./stdout.log")
				if(tmp.res$status != 2)
				{
					if(!is.null(tmp.res$result))
					{
						if(tmp.res$result$deviance<tempdev)
						{
							res.gnm8 <- tmp.res$result
							sgnJ <- signJ
							tempdev  <- res.gnm8$deviance
							if(tmp.res$status == 1)
								warnings <- c(warnings, paste0("Model 8 (Warnings): ", tmp.res$warnings))
							if(!is.null(tmp.res$messages))
								messages <- c(messages, paste0("Model 8 (Messages): ", tmp.res$messages))
							
							tmp.msg <- " ... finished."
						}
					}
				}
				else
					errors <- c(errors, paste0("Model 8 (Errors): ", tmp.res$errors))
			}
			for(ldJ in -3:3)
			{
				J  				<- -2^ldJ
				signJ			<- -1				
				my.form8 	<- VC~-1+powfun8(mu, C,signJ) 
				startweights 	<- pweights/startVC^(2/J)
				VCrootJ     	<- Data[,"VC"]^(1/J)
				st 				<- lm(VCrootJ ~ mu, data=Data,weights=startweights)
				startvals		<- t.coef(c(st$coefficients,signJ*J),Maxi=C,signJ=signJ,model=8)
				tmp.res 		<- conditionHandler(gnm(formula = my.form8, family = Gamma(link = "identity"), 
								data = Data, weights = pweights ,start=startvals, 
								trace=TRUE), file="./stdout.log")
				if(tmp.res$status != 2)
				{
					if(!is.null(tmp.res$result))
					{
						if(tmp.res$result$deviance<tempdev)
						{
							res.gnm8 <- tmp.res$result
							sgnJ <- signJ
							tempdev  <- res.gnm8$deviance
							if(tmp.res$status == 1)
								warnings <- c(warnings, paste0("Model 8 (Warnings): ", tmp.res$warnings))
							if(!is.null(tmp.res$messages))
								messages <- c(messages, paste0("Model 8 (Messages): ", tmp.res$messages))
							
							tmp.msg <- " ... finished."
						}
					}
				}
				else
					errors <- c(errors, paste0("Model 8 (Errors): ", tmp.res$errors))
			}		
		}
		else							# user-specified startvalues
		{
			if(startvals[3]>0){ #restrict range of starting values to 0.1 <|J| <10
				startvals[3]			<- max(0.1,min(10,startvals[3]))
				signJ					<- 1
			}
			else{
				signJ					<- -1
				startvals[3]			<- -max(0.1,min(10,-startvals[3]))
			}
			my.form	<- VC~-1+powfun8(Mean, C,signJ) 
			startvals <-t.coef(startvals,Maxi=C,signJ=signJ,model=8)
			tmp.res <- conditionHandler(gnm(formula = my.form8, family = Gamma(link = "identity"), 
							data = Data, weights = pweights ,start=startvals, 
							trace=TRUE), file="./stdout.log")
			
			if(tmp.res$status != 2)
			{
				if(!is.null(tmp.res$result))
				{
					res.gnm8 <- tmp.res$result
					sgnJ <- signJ
					if(tmp.res$status == 1)
						warnings <- c(warnings, paste0("Model 8 (Warnings): ", tmp.res$warnings))
					if(!is.null(tmp.res$messages))
						messages <- c(messages, paste0("Model 8 (Messages): ", tmp.res$messages))
					
					tmp.msg <- " ... finished."
				}
			}
			else
			{
				errors 	<- c(errors, paste0("Model 8 (Errors): ", tmp.res$errors))
				tmp.msg	<- " ... could not be fitted due to errors."
			}
		}
		if(!is.null(res.gnm8))
		{
			coeffs 	<- bt.coef(res.gnm8,signJ=sgnJ, model=8)
			my.form8simple <- VC~-1+powfun8simple(Mean)
			tmp.res <- conditionHandler(gnm(formula = my.form8simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start=coeffs, trace=TRUE,iterMax=0), file="./stdout.log")
			
			if(!is.null(tmp.res))
			{
				
				res.gnm8    <- tmp.res$result
				RSS[8] 		<- sum((res.gnm8$y-res.gnm8$fitted.values)^2)
				AIC[8]		<- res.gnm8$aic
				Deviance[8] <- res.gnm8$deviance
			}
			else
			{
				res.gnm8	<- NULL
				errors 		<- c(errors, paste0("Model 8 (Errors): ", tmp.res$errors, "\nError occurred in call to 'gnm' with final parameter-estimates."))
				tmp.msg		<- " ... could not be fitted due to errors."
			}	
		}
		
		startvals 		<- NULL
		cat(tmp.msg)
	}
	
	if(9 %in% model.no)
	{
		cat("\nModel 9 ")
		mu			<- Mu[[9]] <- log(Data[,"Mean"])
		logVC  		<- log(Data[,"VC"])
		my.form9 	<- formula(paste("VC", "~ mu", sep = " ")) 	# variable power model without intercept
		if (is.null(startvals)) 								# random start values
		{
			startweights 	<- pweights
			st 				<- lm(logVC ~ mu, data=Data,weights=startweights)
			startvals		<- c(st$coefficients[1],st$coefficients[2])
		}	
		else
		{
			startvals <-t.coef(startvals,model=9)
		}
		
		tmp.res <- conditionHandler(gnm(formula = my.form9, family = Gamma(link = "log"), data = Data, weights = pweights,
						start=startvals, trace=TRUE), file="./stdout.log")
		
		if(tmp.res$status != 2)
		{
			res.gnm9 <- tmp.res$result
			if(tmp.res$status == 1)
				warnings <- c(warnings, paste0("Model 9 (Warnings): ", tmp.res$warnings))
			if(!is.null(tmp.res$messages))
				messages <- c(messages, paste0("Model 9 (Messages): ", tmp.res$messages))
			
			tmp.msg <- " ... finished."
		}
		else
		{
			errors 	<- c(errors, paste0("Model 9 (Errors): ", tmp.res$errors))
			tmp.msg	<- " ... could not be fitted due to errors."
		}
		
		if(!is.null(res.gnm9))
		{			
			coeffs 	<- bt.coef(res.gnm9, model=9)#
			
			my.form9simple 	<- VC~-1+powfun9simple(Mean)
			tmp.res 		<- conditionHandler(gnm(formula = my.form9simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start=coeffs, trace=TRUE, iterMax=0), file="./stdout.log")
			
			if(!is.null(tmp.res ))
			{		
				res.gnm9    	<- tmp.res$result
				RSS[9]			<- sum((res.gnm9$y-res.gnm9$fitted.values)^2)
				AIC[9]			<- res.gnm9$aic
				Deviance[9] 	<- res.gnm9$deviance
			}
			else
			{
				res.gnm9	<- NULL
				errors 		<- c(errors, paste0("Model 9 (Errors): ", tmp.res$errors, "\nError occurred in call to 'gnm' with final parameter-estimates."))
				tmp.msg		<- " ... could not be fitted due to errors."
			}	
		}
		
		startvals <- NULL
		cat(tmp.msg)
	}
	
	if(10 %in% model.no)
	{
		cat("\nModel 10")
		
		if(!is.null(col.cv))		# minimize error propagation effects by using CV-values directly
		{
			res.mod10 	<- fit.EP17(x=Data$Mean, y=Data$CV, DF=Data$DF, typeY="cv")
		}
		else
		{
			if(!is.null(col.sd))	# second best option
			{
				Data$CV		<- 100*Data$SD/Data$Mean 
				res.mod10 	<- fit.EP17(x=Data$Mean, y=Data$SD, DF=Data$DF, typeY="sd")
			}
			else					# worst option, since VC --> SD --> CV
			{
				Data$CV		<- 100*sqrt(Data$VC)/Data$Mean 	
				res.mod10 	<- fit.EP17(x=Data$Mean, y=Data$VC, DF=Data$DF, typeY="vc")
			}
		}
		
		RSS[10]		<- res.mod10$RSS
		AIC[10]		<- res.mod10$AIC
		Deviance[10]<- res.mod10$deviance
		cat(" ... finished.")
	}
	
	cat("\n")
	
	if(quiet)						# read logs and remove temporary files
	{
		#try(close(outputLog), silent=TRUE)				
		output 	<- scan("./stdout.log", sep="\n", what="character", quiet=TRUE)			
		try(file.remove("./stdout.log"), silent=TRUE)		
	}
	else
		output <- NULL
	
	mod <- list(
			model1 =res.gnm1,
			model2 =res.gnm2,
			model3 =res.gnm3,
			model4 =res.gnm4,
			model5 =res.gnm5,
			model6 =res.gnm6,
			model7 =res.gnm7,
			model8 =res.gnm8,
			model9 =res.gnm9,
			model10=res.mod10)
	
	ind <- which(!sapply(mod, is.null))
	res <- list(
			Call=Call,
			Models=mod[ind],
			RSS=RSS[ind],
			AIC=AIC[ind],
			GoF.pval=sapply(mod[ind], function(x) pchisq(x$deviance, x$df.residual, lower.tail=FALSE)),
			Deviance=Deviance[ind],
			Formulas=Formulas[ind],
			Data=Data,
			K=K,
			startvals=startvals,
			errors=errors,
			output=output,
			warnings=warnings,
			notes=notes)
	
	class(res) <- "VFP"
	
	return(res)
}




#' Transform list of VCA-object into VFP-matrix required for fitting.
#'
#' @param obj		(list) of VCA-objects
#' @param vc		(integer, character) either an integer specifying a variance component
#' 					or the name of a variance component; can also be a vector of integers
#' 					specifying a continuous sequence of variance components always including
#' 					'error' (repeatability)
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @examples 
#' \donttest{
#' library(VCA)
#' data(VCAdata1)
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by="sample")
#' getMat.VCA(lst)  # automatically selects 'total'
#' # pooled version of intermediate precision (error+run+day)
#' getMat.VCA(lst, 4:6)
#' # only repeatability ('error')
#' getMat.VCA(lst, "error")
#' }

getMat.VCA <- function(obj, vc=1)
{
	stopifnot(class(obj) == "list")
	stopifnot(all(sapply(obj, class) == "VCA"))
	stopifnot(is.numeric(vc) || is.character(vc))
	tab <- obj[[1]]$aov.tab
	if(is.numeric(vc))
	{
		stopifnot(all(vc %in% 1:nrow(tab)))
		nm <- vc
		vc <- rownames(tab)[vc]
	}
	else
	{
		stopifnot(all(vc %in% rownames(tab)))
		nm <- (1:nrow(tab))[which(rownames(tab) %in% vc)]	
	}
	
	if(length(nm) > 1)
	{
		if(max(nm) != nrow(tab) || !(all(diff(nm) == 1)))
			stop("When specifying a sequence of variance components, it must include 'error' and be continuous!")
	}
	if(length(vc) > 1)		
	{
		tmp <- lapply(obj, stepwiseVCA)			# perform all combinations of VCA
		idx <- suppressWarnings(which(unlist(lapply(tmp[[1]], function(x) all(rownames(x$aov.tab) == c("total", vc))))))
		for(i in 1:length(tmp))
			tmp[[i]] <- tmp[[i]][[idx]]
		obj <- tmp
		vc  <- "total"
	}
	
	mat <- t(sapply(obj, function(x) c(Mean=x$Mean, x$aov.tab[vc, c("DF", "VC")])))
	mat <- as.data.frame(mat[order(mat[,"Mean"]),])
	mat
}




#' Plot VFP-Ojbects.
#' 
#' Function takes an object of class 'VFP' and plots a fitted variance-function
#' either on the original variance-scale ('type="vc"') or on the CV-scale ("cv").
#' The corresponding 100x(1-alpha)\% confidencen interval around the variance-function
#' can be plotted either as lines ('ci.type="lines') or as per default as CI-band.
#' 
#' @param x				(VFP) object as returned by function 'fit.vfp'
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
#' @param Grid			(list) passed to function \code{\link{addGrid}} controlling the appearance
#' 						of a grid, set to NULL to omit
#' @param Crit			(list) passed to function \code{\link{legend}} per default used to present
#' 						the optimality criterion for choosing the best fitting model, per default this
#' 						is AIC and additionally residual sum of squares (RSS) is shown for the plotted
#' 						model in the upper-right corner
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
#' @seealso \code{\link{fit.vfp}}, \code{\link{predict.VFP}}, \code{\link{predictMean}}
#' 
#' @examples 
#' \donttest{
#' library(VCA)
#' data(VCAdata1)
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by="sample")
#' mat <- getMat.VCA(lst)		# automatically selects "total"
#' mat
#' 
#' res <- fit.vfp(model.no=1:10, Data=mat)
#' plot(res)
#' plot(res, type="cv")
#' plot(res, type="cv", ci.type="lines", ci.col="red",
#' 		Grid=list(col="wheat"), Points=list(pch=2, lwd=2, col="black"))
#' 
#' # same for repeatability
#' mat.err <- getMat.VCA(lst, "error")
#' res.err <- fit.vfp(1:10, Data=mat.err)
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
#' rep.CA19_9 <- getMat.VCA(fits.CA19_9, "error")
#' 
#' # extract reproducibility
#' repro.CA19_9 <- getMat.VCA(fits.CA19_9, "total")
#' 
#' # extract intermediate-precision (within-lab)
#' ip.CA19_9 <- getMat.VCA(fits.ip.CA19_9, "total")
#' 
#' # fit model (a+bX)^C (model 8) to all three matrices
#' 
#' mod8.repro 	<- fit.vfp(repro.CA19_9, 8)
#' mod8.ip		<- fit.vfp(ip.CA19_9, 8)
#' mod8.rep		<- fit.vfp(rep.CA19_9, 8)
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
#' legend.rm( x="center", pch=15:17, col=c("blue", "deepskyblue", "darkorchid3"),
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
		ylim=NULL, Prediction=NULL, Pred.CI=NULL, Model=TRUE,
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
	
	if(is.null(call$xlim))
	{
		xlim <- range(obj$Data[,"Mean"])
		xlim[1] <- xlim[1] * 0.75
		xlim[2] <- xlim[2] * 1.05
	}
	else
	{
		xlim <- eval(call$xlim)
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
			if(class(tmp) == "try-error")
				warning(paste0("Model '", mod, "' could not be plotted due to an error!\n\nError:\n\t", attr(tmp, "condition")$message,"\n\n"))
			mtext( 	side=1, at=par("usr")[1], line=3, font=6,
					text=paste(switch(i, "1"="", "2"="2nd", "3"="3rd", "4"="4th", 
									"5"="5th", "6"="6th", "7"="7th", "8"="8th", "9"="9th", "10"="10th"),
							"best-fitting model"))
			cat(paste0(	"(",i,") ",names(aic)[i], ifelse(names(aic)[i] == "Model_10", "\t", "\t\t"),	
							format(paste0("AIC: ", Signif(aic[i])), width=15),
							format(paste0("Deviance: ", Signif(dev[i])), width=20),
							format(paste0("RSS: ", Signif(rss[i])), width=15),
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
		do.call("addGrid", Grid)
	
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
				predMean <- predictMean(x, model.no=model.no, newdata=Prediction$y, type=type)
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
				Pred.CI.def 	<- list(col=as.rgb("blue", .15), border=NA)
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
		tmpAIC <- Signif(x$AIC[num])
		tmpRSS <- Signif(x$RSS[num])
		
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
			res.X	<- cbind(res.X, LCL.Mean=pred.X[,"LCL"])
			res.X	<- cbind(res.X, UCL.Mean=pred.X[,"UCL"])
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



#' Adapted Version of Function 'signif'
#' 
#' This function adapts base-function \code{\link{signif}}
#' by always returning integer values in case the number of
#' requested significant digits is less than the the number of
#' digits in front of the decimal separator.
#' 
#' @param x			(numeric) value to be rounded to the desired number
#' 					of significant digits
#' @param digits	(integer) number of significant digits
#' @param force		(logical) TRUE = force the return value to have at least 4 significant
#' 					digits, i.e. to integers with less digits zeros will be appended after
#' 					the decimal separator, otherwise the return value will be casted from
#' 					character to numeric
#' @param ...		additional parameters
#' 
#' @return 	number with 'digits' significant digits, if 'force=TRUE' "character" objects will be
#' 			returned otherwise objects of mode "numeric"
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}

Signif <- function(x, digits=4, force=TRUE, ...)
{
	call 	<- match.call()
	manyX 	<- call$manyX
	if(is.null(manyX))
		manyX <- FALSE
	stopifnot(is.numeric(x))
	if(length(x) > 1)
		return(sapply(x, Signif, digits=digits, manyX=TRUE))
	
	if(!manyX && "coef.gnm" %in% class(x))		# assign name to single gnm-coefficient
	{
		x <- as.numeric(x)
		names(x) <- "beta1"
	}
	Ndbc	<- nchar(substr(as.character(x), 1, regexpr("\\.", as.character(x))-1))
	x 		<- signif(x, ifelse(Ndbc > digits, Ndbc, digits))
	NcX		<- nchar(x)
	comma	<- grepl("\\.", x)
	if(comma)
		NcX <- NcX - 1
	if(NcX < digits)
		x <- paste0(x, ifelse(comma, "", "."), paste(rep(0, digits-NcX), collapse=""))
	if(!force)
		x <- as.numeric(x)
	x
}

#' Summary Objects of Class 'VFP'
#' 
#' @param object		(object) of class 'VFP'
#' @param model.no		(integer) specifying a fitted model in 'x', if NULL the best fitting
#' 						model will be printed, i.e. the one with min(RSS)
#' @param digits		(integer) number of significant digits
#' @param type			(character) "simple" = short summary, "complex" = calls the summary-method
#' 						for objects of class "gnm"
#' @param ...			additional parameters passed forward
#' 
#' @method summary VFP
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' @examples 
#' \donttest{
#' library(VCA)
#' data(CA19_9)
#' fits.CA19_9 <- anovaVCA(result~site/day, CA19_9, by="sample")
#' # extract repeatability
#' mat.CA19_9 <- getMat.VCA(fits.CA19_9, "error")
#' res.CA19_9 <- fit.vfp(mat.CA19_9, 1:10)
#' summary(res.CA19_9)
#' print(res.CA19_9)
#' }

summary.VFP <- function(object, model.no=NULL, digits=4, type=c("simple", "complex"), ...)
{
	
	AIC 	<- object$AIC
	num 	<- which(AIC== min(AIC))[1]			# index
	AIC 	<- NULL
	type 	<- match.arg(type[1], choices=c("simple", "complex"))
	
	if(!is.null(model.no))
	{
		models <- sub("Model_", "", names(object$RSS))
		if(!model.no %in% models)
			stop("Specified model ", paste0("'", model.no, "'")," is not among fitted models: ", paste(models, collapse=", "),"!")
		else
			num <- which(models == model.no)		
		
		model 	<- names(object$RSS[num])			# name
		Nmodel	<- 1
	}
	else{
		Nmodel		<- length(object$Models)		
		model 		<- names(object$RSS[num])			# name
		model.no	<- as.numeric(sub("Model_", "", model))
	}	
	
	if(model.no == 10)
		type <- "simple"
	
	form 	<- object$Formulas[num]
	fun	<- function(x)
	{
		gsub("==", "=", gsub("\\}", "", gsub("\\{", "", gsub("\\]", "", gsub("\\[", "", gsub("[\n ]", "", x))))))
	}
	form 	<-  fun(form)
	
	cat("\n\n +++ Summary Fitted VFP-Model(s) +++ ")
	cat(  "\n-------------------------------------\n\n")
	cat("Call:\n")
	print(object$Call)
	cat("\n")
	
	if(Nmodel > 1)
	{
		cat("\t[+++", Nmodel, "Models Fitted +++]\n")
		for(i in 1:Nmodel)
			cat(paste0("\n", names(object$RSS[i]),": "), fun(object$Formulas[i]))
		cat("\n\n\t[+++ RSS (unweighted) +++]\n\n")
		print(Signif(object$RSS, digits=digits), quote=FALSE)
		cat("\n\t[+++ AIC +++]\n\n")
		print(Signif(object$AIC, digits=digits), quote=FALSE)
		cat("\n\t[+++ Deviance +++]\n\n")
		print(Signif(object$Deviance, digits=digits), quote=FALSE)
		cat("\n\t[+++ Best Model +++]\n\n")
	}
	cat(paste0("Model ", sub("Model_", "", model),":"),"\t")
	cat(form)
	cat("\n\nCoefficients:\n")
	
	if(type == "complex")
	{
		Smry <- summary(object$Models[[num]])
		
		printCoefmat(	Smry$coefficients, digits = digits, 
				signif.stars=getOption("show.signif.stars"), 
				signif.legend=TRUE, na.print = "NA", ...)
		
		cat("\nDeviance Residuals:\n")
		print(summary(Smry$deviance.resid))
	}
	else
		print(Signif(coef(object, model.no), digits), quote=FALSE)
	
	cat("\nAIC =", 		 Signif(object$AIC[num], digits), 
			" RSS =", 		 Signif(object$RSS[num], digits), 
			" Deviance =",   Signif(object$Deviance[num], digits), 
			" GoF P-value=", Signif(object$GoF.pval[num], digits), "\n\n")
	
	if(!is.null(object$note) && Nmodel > 1)
	{
		cat("\t[+++ Note +++]\n\n")
		cat(object$note, sep="\n")
		cat("\n\n")
	}
}

#' Print Objects of Class 'VFP'
#' 
#' @param x				(object) of class 'VFP'
#' @param model.no		(integer) specifying a fitted model in 'x', if NULL the best fitting
#' 						model will be printed, i.e. the one with min(AIC)
#' @param digits		(integer) number of significant digits
#' @param ...			additional parameters passed forward
#' 
#' @method print VFP
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' @examples 
#' \donttest{
#' library(VCA)
#' data(CA19_9)
#' fits.CA19_9 <- anovaVCA(result~site/day, CA19_9, by="sample")
#' # extract repeatability
#' mat.CA19_9 <- getMat.VCA(fits.CA19_9, "error")
#' res.CA19_9 <- fit.vfp(mat.CA19_9, 1:10)
#' res.CA19_9
#' }

print.VFP <- function(x, model.no=NULL, digits=4, ...)
{
	if(is.null(model.no))				# automatically determine best fitting model
	{
		AIC 		<- x$AIC
		num 		<- which(AIC== min(AIC))[1]
		AIC 		<- NULL
		num0		<- TRUE
	}
	else
	{
		num0 	<- FALSE
		models 	<- sub("Model_", "", names(x$RSS))
		if(!model.no %in% models)
			stop("Specified model ", paste0("'", model.no, "'")," is not among fitted models: ", paste(models, collapse=", "),"!")
		else
			num <- which(models == model.no)
	}
	model 		<- names(x$RSS[num])
	model.no	<- as.numeric(sub("Model_", "", model))
	form 		<- x$Formulas[num]
	form 		<-  gsub("==", "=", gsub("\\}", "", gsub("\\{", "", gsub("\\]", "", gsub("\\[", "", gsub("[\n ]", "", form))))))
	cat("\n\n(VFP) Variance-Function")
	cat(  "\n-----------------------\n\n")
	cat(paste0("Model ", sub("Model_", "", model), ifelse(length(x$Models)>1 && num0, "*:", ":")),"\t")
	cat(form)
	cat("\n\nCoefficients:\n")
	print(Signif(coef(x, model.no), digits), quote=FALSE)
	cat("\nAIC =", Signif(x$AIC[num], digits), " RSS =", Signif(x$RSS[num], digits), " Deviance =", Signif(x$Deviance[num], digits),"GoF P-value=",Signif(x$GoF.pval[num], digits),"\n\n")
	if(length(x$Models) > 1 && num0)
		cat("*Best-fitting model of", length(x$Models),"VFP-models overall\n\n")
}

#' Extract Model-Coefficients from VFP-Objects.
#' 
#' @param object		(object) of class "VFP"
#' @param model.no		(integer) specifying one of models 1:10, must be one of the 
#' 						fitted models
#' @param ...			additional parameters passed forward
#' 
#' @method coef VFP 
#' 
#' @author 	Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 			Florian Dufey \email{florian.dufey@@roche.com}
#' 
#' @return (numeric) model coefficients
#' 
#' @examples 
#' \donttest{
#' library(VCA)
#' data(VCAdata1)
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by="sample")
#' mat <- getMat.VCA(lst)		# automatically selects "total"
#' res <- fit.vfp(model.no=1:10, Data=mat)
#' coef(res)
#' }

coef.VFP <- function(object, model.no=NULL, ...)
{
	
	if(is.null(model.no))				# automatically determine best fitting model
	{
		AIC <- object$AIC
		num <- which(AIC== min(AIC))[1]
		AIC <- NULL
		message(paste0("No 'model.no' specied! Best-fitting model assumed ('",names(object$RSS)[num],"')!"))
	}
	else
	{
		models <- sub("Model_", "", names(object$RSS))
		if(!model.no %in% models)
			stop("Specified model ", paste0("'", model.no, "'")," is not among fitted models: ", paste(models, collapse=", "),"!")
		else
			num <- which(models == model.no)
	}
	
	model <- names(object$RSS[num])
	
	if(model == "Model_10")
	{
		coeffs 			<- object$Models[[num]]$coefficients
		coeffs[1] 		<- exp(coeffs[1]) 
		names(coeffs) 	<- c("beta_1", "pot") 
	}	
	else
		coeffs 	<- coef(object$Models[[num]])
	
	pot		<- which(names(coeffs) == "pot")
	if(length(pot) > 0)
		names(coeffs)[pot] <- "J"
	
	coeffs
}


#' Back-Transformation of Estimated Coefficients.
#' 
#' This function performs back-transformation from re-parameterized forms in the 'VFP'-package
#' into the original form.
#' 
#' In the 'VFP' package models are re-parameterized to have better control over the constraint
#' solution-space, i.e. only models may be fitted generating non-negative fitted values. This 
#' function is intended to be for internal use only.
#' 
#' @param object			(object) of class 'gnm' representing a fitted model in re-parameterized form
#' @param K					(numeric) constant value 'K'
#' @param signJ				(integer) either 1 or -1
#' @param model				(integer) specifying which model shall be back-transformed
#' @param ...				additional parameters
#' 
#' @author 	Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 			Florian Dufey \email{florian.dufey@@roche.com}
#' 
#' @return (numeric) vector of coefficients in original parameterized form

bt.coef <- function(object, K=NULL, signJ=NULL, model=NULL, ...)
{
	stopifnot(model %in% 1:10)
	coeffs0 <- coef(object)
	cutval<-0.95 				# should be between 0 and 1 
	
	# back transformation to original scale
	
	if(model %in% c(1,2))				# models 1 and 2
	{
		coeffs <- c(exp(coeffs0))
		names(coeffs) <- c("beta")
	}
	else if(model == 3)
	{
		coef1	<- exp(coeffs0[1])
		coef2	<- coef1*(exp(coeffs0[2])-cutval)/max(object$data[,"Mean"]^2)
		coeffs  <-c(coef1,coef2)
		names(coeffs) <- c("beta1", "beta2")
	}
	else if(model == 4)
	{
		coef1 	<- exp(coeffs0[1]/K)
		coef2 	<- coef1*(exp(coeffs0[2])-cutval)/max(object$data[,"Mean"])
		coeffs 	<- c(coef1, coef2)
		names(coeffs) <- c("beta1", "beta2")
	}
	else if(model == 5)
	{
		coef1	<- exp(coeffs0[1])
		coef2	<- coef1*(exp(coeffs0[2])-cutval)/max(object$data[,"Mean"]^K)
		coeffs  <-c(coef1,coef2)
		names(coeffs) <- c("beta1", "beta2")
	}
	else if(model == 6)
	{
		coef1 	<- exp(coeffs0[1])
		coef2 	<- exp(coeffs0[1]+coeffs0[3])*(1+exp(-coeffs0[4]))*(atan(coeffs0[2])/pi-.5)
		coef3	<- -exp(coeffs0[1]+coeffs0[3]-coeffs0[4])*(atan(coeffs0[2])/pi-.5)*exp(coeffs0[3]*exp(coeffs0[4]))
		coef4	<- 1 + exp(coeffs0[4])
		coeffs 	<- c(coef1, coef2, coef3, coef4)
		names(coeffs) <- c("beta1", "beta2", "beta3", "J")
	}
	else if(model == 7)
	{		
		coef1	<- exp(coeffs0[1])
		coef3	<-((0.1+10*exp(coeffs0[3]))/(1+exp(coeffs0[3])))
		coef2	<- coef1*(exp(coeffs0[2])-cutval)/max(object$data[,"Mean"]^coef3)
		coeffs	<- c(coef1, coef2, coef3)
		names(coeffs) <- c("beta1", "beta2", "J")
	}
	else if(model == 8)
	{
		coef3	<-signJ*((0.1+10*exp(coeffs0[3]))/(1+exp(coeffs0[3])))
		coef1 	<- exp(coeffs0[1]/coef3)
		coef2	<- coef1*(exp(coeffs0[2])-cutval)/max(object$data[,"Mean"])
		coeffs	<- c(coef1, coef2, coef3)
		names(coeffs) <- c("beta1", "beta2", "J")
	}
	else if(model == 9)
	{
		coeffs <- c(exp(coeffs0[1]), coeffs0[2])
		names(coeffs) <- c("beta1", "J")
	}
	
	coeffs
}

#' Transformation of Coefficients.
#' 
#' This function performs transformation from the original parameterization into the 'VFP'-package
#' internal re-parameterized form.
#' 
#' In the 'VFP' package models are re-parameterized to have better control over the constrained
#' solution-space, i.e. only models may be fitted generating non-negative fitted values. This 
#' function is intended to be for internal use only.
#' 
#' @param coeffs0			(numeric) vector of function coefficients to be transformed into the
#' 							re-parameterized form
#' @param K					(numeric) constant value 'K'
#' @param Maxi				(numeric) max. value
#' @param model				(integer) specifying which model shall be back-transformed
#' @param signJ				(integer) either 1 or -1
#' @param eps				(numeric) constant used instead of zero in case of log-transformation
#' @param ...				additional parameters
#' 
#' @author 	Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 			Florian Dufey \email{florian.dufey@@roche.com}
#' 
#' @return (numeric) vector of coefficients in re-parameterized form

t.coef <- function(coeffs0, K=NULL, Maxi=NULL, model=NULL, signJ=NULL, eps=sqrt(.Machine$double.eps), ...)
{
	stopifnot(model %in% 1:10)
	cutval<-0.95 				# should be between 0 and 1 
	
	# back transformation to original scale
	
	if(model %in% c(1,2))				# models 1 and 2
	{
		coeffs <- c(log(pmax(eps,coeffs0)))
		names(coeffs) <- c("gamma1")
	}
	else if(model == 3)	
	{
		coef1 <- log(max(eps,coeffs0[1]))
		coef2 <- log(max(eps,Maxi*coeffs0[2]/max(eps,coeffs0[1])+cutval))
		coeffs<- c(coef1,coef2)
		names(coeffs) <- c("gamma1","gamma2")
	}
	else if(model == 4)
	{
		coef1 	<- K*log(max(eps,coeffs0[1]))
		coef2   <- log(max(eps,Maxi*coeffs0[2]/max(eps,coeffs0[1])+cutval))
		coeffs 	<- c(coef1, coef2)
		names(coeffs) <- c("gamma1", "gamma2")
	}
	else if(model == 5)	
	{
		coef1 <- log(max(eps,coeffs0[1]))
		coef2 <- log(max(eps,Maxi*coeffs0[2]/max(eps,coeffs0[1])+cutval))
		coeffs<- c(coef1,coef2)
		names(coeffs) <- c("gamma1","gamma2")
	}
	else if(model == 6)
	{				
		coef1 	<- log(max(eps, coeffs0[1]))
		coef3   <- log(max(eps,(-coeffs0[3]/coeffs0[2]*coeffs0[4])))/(coeffs0[4]-1)
		coef2   <- tan((coeffs0[2]/exp(-coef3)*(coeffs0[4]-1)/coeffs0[4]+0.5)*pi)
		coef4   <- log(max(eps, coeffs0[4]-1))
		coeffs 	<- c(coef1, coef2, coef3, coef4)
		names(coeffs) <- c("gamma1", "gamma2", "gamma3", "gamma4")
	}
	else if(model == 7)
	{
		coef1 <- log(max(eps,coeffs0[1]))
		coef2 <- log(max(eps,Maxi*coeffs0[2]/max(eps,coeffs0[1])+cutval))
		coef3 <- log((max(0.1,coeffs0[3])-0.1)/(10-pmin(9.999,coeffs0[3])))
		coeffs  <- c(coef1,coef2,coef3)
		names(coeffs) <- c("gamma1", "gamma2", "gamma3")
	}
	else if(model == 8)
	{
		coef1 	<- log(max(eps,coeffs0[1]))*signJ*coeffs0[3]
		coef2	<- log(max(eps,coeffs0[2]/max(eps,coeffs0[1])*Maxi+cutval))
		coef3   <- log((max(0.1,coeffs0[3])-0.1)/(10-pmin(9.999,coeffs0[3])))
		coeffs	<- c(coef1, coef2, coef3)
		names(coeffs) <- c("gamma1", "gamma2", "gamma3")
	}
	else if(model == 9)
	{
		coeffs  <- c(log(max(eps,coeffs0[1])),coeffs0[2])
		names(coeffs) <- c("gamma1", "gamma2")
	}
	
	coeffs
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
#' @examples 
#' \dontrun{
#' # perform variance component analysis
#' library(VCA)
#' data(VCAdata1)
#' # perform VCA-anaylsis
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by="sample")
#' # transform list of VCA-objects into required matrix
#' mat <- getMat.VCA(lst)		# automatically selects "total"
#' mat
#' # fit all models batch-wise, the best fitting will be used automatically
#' res <- fit.vfp(model.no=1:10, Data=mat)
#' # plot hit and visualize imprecision usign default settings
#' precisionPlot(res, cutoff=20)
#' # without normal distribution at cutoff do
#' precisionPlot(res, cutoff=20, prob=c(.05, .95), col=c("blue", "red"))
#' # highlight the proportion > cutoff (hit rate) more 
#' precisionPlot(res, cutoff=20, prob=c(.05, .95), col=c("blue", "red"), alpha2=.5)
#' # plot with legend
#' precisionPlot(res, cutoff=20, prob=c(.05, .95), col=c("blue", "red"), alpha2=.5, Legend=TRUE)
#' # use different probabilities and colors
#' precisionPlot(res, cutoff=20, prob=c(.05, .95), col="black", alpha2=.3)
#' 
#' # now using two cutoffs, i.e. with equivocal zone
#' precisionPlot(	res, cutoff=c(17, 19), prob=c(.05, .95), col=c("mediumblue", "red3"), 
#' 					alpha2=.5, HRLine=list(col=c("mediumblue", "red3")))
#' }
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}

precisionPlot <- function(	vfp, model.no=NULL, cutoff, prob=c(.05, .5, .95), col=c("blue", "black", "red"), 
		Cutoff=list(), Title=list(), Xlabel=list(), Ylabel=list(), HRLine=list(),
		Legend=FALSE, nclass=-1,  BG="gray90", digits=3, alpha=.15, alpha2=0, 
		xlim=NULL, col.grid="white", Nrand=1e6)
{
	stopifnot(class(vfp) == "VFP")
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
		res1	<- deriveCx(vfp, model.no=model.no, cutoff=cutoff, Cx=p)
	else
	{
		res1	<- deriveCx(vfp, model.no=model.no, cutoff=cutoff[1], Cx=p)
		res2	<- deriveCx(vfp, model.no=model.no, cutoff=cutoff[2], Cx=p)
	}
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
	addGrid(x=pretty(xlim, 20), y=seq(0,1,.05), col=col.grid)
	axis(1, at=pretty(xlim, 10), mgp=c(3, .75, 0), col.ticks="gray60")
	axis(2, las=1, mgp=c(3, .75, 0), col.ticks="gray60")
	
	Means 	<- NULL
	scl		<- res1[which(p == tail(prob, 1)),]
	scl		<- dnorm(scl["Mean"], mean=scl["Mean"], sd=scl["SD"])
	
	for(i in 1:length(prob))
	{
		tmpInd  <- which(p == prob[i])
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
		
		plot(	h, las=1, xlim=xlim, ylim=ylim, col=as.rgb(col[i], alpha), 
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
			rect(h$breaks[j], 0, h$breaks[j+1], h$density[j], col=as.rgb(col[i], alpha2), border="white")
	}
	
	abline(v=Means, h=prob, col="gray60", lty=2, lwd=2)
	
	mtext(side=3, at=Means, line=0, col="gray60", text=paste0("C", 100*prob))
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
		Fill 	<- as.rgb(col[1:length(col)], alpha)
		if(alpha2 > 0)
			Fill <- c(Fill, as.rgb(col[1:length(col)], ifelse(alpha+alpha2>1, 1, alpha+alpha2)))
		
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
		
		
		legend.rm(	x="center", fill=Fill, legend=Legend, y.intersp=2, 
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
#' This function makes use of a precision profile, determining the
#' concentration of that sample when measured a large number of times,
#' 100 * 'Cx'\\% of the measurements lie above 'cutoff'. In case of e.g.
#' "C5" exactly 5\\% of will be above cutoff, whereas for "C95" 95\\% will
#' be larger than cutoff. This follows the CLSI EP12 guideline whenever 
#' an internal continuous result (ICR) is available and measurement
#' imprecision can be assumed to be normally distributed. The CLSI EP12
#' recommends to base derivation of C5 and C95 on the results of intermediate
#' precision analyses using multiple samples. This includes between-day and 
#' between-run as additional variance components besides repeatability.
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
#' @examples 
#' \dontrun{
#' # perform variance component analysis
#' library(VCA)
#' data(VCAdata1)
#' # perform VCA-anaylsis
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by="sample")
#' # transform list of VCA-objects into required matrix
#' mat <- getMat.VCA(lst)		# automatically selects "total"
#' mat
#' # fit all models batch-wise
#' res <- fit.vfp(model.no=1:10, Data=mat)
#' # now search for the C5 concentration
#' deriveCx(res, start=15, cutoff=20, Cx=0.05, plot=TRUE)
#' deriveCx(res, start=25, cutoff=20, Cx=0.95, plot=TRUE)
#' deriveCx(res, start=25, cutoff=20, Cx=0.25, plot=TRUE)
#' deriveCx(res, start=25, cutoff=20, Cx=0.75, plot=TRUE)
#' 
#' #
#' p <- c(seq(.01, .12, .01), seq(.15, .85, .05), seq(.88, .99, .01))
#' system.time(x <- deriveCx(res, Cx=p, cutoff=20))
#' }

deriveCx <- function(vfp, model.no=NULL, start=NULL, cutoff=NULL, Cx=.05, tol=1e-6, plot=FALSE)
{
	stopifnot(class(vfp) == "VFP")
	stopifnot(is.numeric(cutoff))
	if(is.null(start))
		start <- cutoff
	stopifnot(is.numeric(start))
	if(length(Cx) > 1)
	{
		res <- t(sapply(Cx, deriveCx, vfp=vfp, model.no=model.no, start=start, cutoff=cutoff))
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
			res <- c(Mean=Mean, SD=SD)
			return(res)
		}
		
		if(Diff < 0)
			Mean <- Mean + abs(Diff)/2
		else
			Mean <- Mean - abs(Diff)/2
		
		iter <- iter + 1
		if(iter == 500)
			break
	}	
}


#' Predict Method for Objects of Class 'VFP'.
#' 
#' @param object		(object) of class "VFP"
#' @param model.no		(integer) specifying a fitted model stored in 'object'
#' @param newdata		(numeric) optionally, a vector specifying mean-values for which predictions
#' 						on the user-defined scale ('type') are requested. If omitted, fitted values 
#' 						will be returned.
#' @param alpha			(numeric) value specifying the 100 x (1-alpha)\% confidence interval of predicted
#' 						values
#' @param dispersion	(numeric) NULL = the dispersion parameter will be estimated,
#' 						numeric value = the dispersion parameter will be used as specified
#' @param type			(character) specifying on which scale the predicted values shall be returned, 
#' 						possible are "vc" = variance, "sd"=standard deviation, "cv"=coefficient of variation
#' @param CI.method		(character) one of "t", "normal", "chisq" specifying which CI-method to use
#' @param use.log		(logical) TRUE = X- and Y-axis will be log-transformed
#' @param ...			additional parameters passed forward to function \code{\link{predict.gnm}}
#' 						\code{\link{predict.gnm}}
#' 
#' @return (data.frame) with numeric variables:\cr
#' 			\item{Mean}{value at which predictions were requested}
#' 			\item{Fitted}{prediction at 'Mean'}
#'			\item{SE}{standard error of prediction}
#' 			\item{Scale}{residual scale}
#' 			\item{LCL}{lower confidence limit of the 100x(1-'alpha')\% CI}
#'  		\item{UCL}{upper confidence limit of the 100x(1-'alpha')\% CI}			
#' 
#' @examples 
#' \donttest{
#' library(VCA)
#' data(VCAdata1)
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by="sample")
#' mat <- getMat.VCA(lst)		# automatically selects "total"
#' res <- fit.vfp(model.no=1:10, Data=mat)
#' predict(res)
#' predict(res, dispersion=0.95)
#' }

predict.VFP <- function(object, model.no=NULL, newdata=NULL, alpha=.05,
		dispersion=NULL, type=c("vc", "sd", "cv"), 
		CI.method=c("chisq", "t", "normal"), use.log=FALSE, ...)
{
	call 		<- match.call()
	predOnly 	<- call$predOnly
	if(is.null(predOnly))
		predOnly <- FALSE
	
	stopifnot(is.null(dispersion) || (is.numeric(dispersion) && dispersion > 0))
	
	type 		<- match.arg(type[1], choices=c("vc", "sd", "cv"))
	CI.method	<- match.arg(CI.method[1], choices=c("t", "normal", "chisq"))
	
	if(is.null(model.no))				# automatically determine best fitting model
	{
		AIC <- object$AIC
		num <- which(AIC== min(AIC))[1]
		AIC <- NULL
	}
	else
	{
		models <- sub("Model_", "", names(object$RSS))
		if(!model.no %in% models)
			stop("Specified model ", paste0("'", model.no, "'")," is not among fitted models: ", paste(models, collapse=", "),"!")
		else
			num <- which(models == model.no)
	}
	if(is.null(dispersion))
	{
		dispersion <- 1
	}
	model <- names(object$RSS[num])
	
	if(is.null(newdata))
	{
		Mean 	<- object$Data$Mean
		newdata	<- data.frame(Mean=Mean)
	}
	else
	{
		Mean 	<- newdata
		newdata	<- data.frame(Mean=newdata)	
		if(use.log)
			newdata$Mean <- exp(newdata$Mean)
	}
	
	suppressWarnings(
			pred <- try(predict(object$Models[[num]], newdata=newdata,
							dispersion=dispersion, type="response", 
							se=TRUE, ci.type=type, CI.method=CI.method,
							use.log=use.log, ...),
					silent=TRUE)
	)
	
	if(class(pred) == "try-error")
		pred <- newdata
	
	if(model != "Model_10")
	{
		# transform to log-scale for t- and normal-dist
		if(CI.method %in% c("t", "normal"))
		{
			pred$se.fit <- pred$se.fit / pred$fit
			pred$fit	<- log(pred$fit)
		}
		else			# chisq
		{
			if( type %in% c("sd", "cv") )
			{
				pred$fit 	<- sqrt(pred$fit)
				pred$se.fit <- pred$se.fit/(2*pred$fit)
				
				if(type == "cv" && !use.log)
				{
					pred$fit 	<- 100 * pred$fit / Mean
					pred$se.fit <- 100 * pred$se.fit / Mean
				}
			}
		}
		
		if(predOnly)
			return(pred$fit)
		
		pred <- as.data.frame(pred)
		pred <- cbind(newdata, pred)
		
		if(CI.method == "normal")
		{
			Qnorm 		<- qnorm(1-alpha/2)
			CIupper 	<- pred$fit + Qnorm * pred$se.fit
			CIlower 	<- pred$fit - Qnorm * pred$se.fit
		}
		else if(CI.method == "chisq")
		{
			df.qchisq		<- 2*(pred$fit/pred$se.fit)^2
			lower.qchisq 	<- qchisq(1-alpha/2, df=df.qchisq)
			upper.qchisq 	<- qchisq(alpha/2, df=df.qchisq)
			CIlower			<- pred$fit*df.qchisq/lower.qchisq
			CIupper			<- pred$fit*df.qchisq/upper.qchisq
		}
		else if(CI.method == "t")
		{
			Qtdist 		<- qt(1 - alpha/2, df.residual(object$Models[[num]]))
			CIupper 	<- pred$fit+Qtdist*pred$se.fit
			CIlower		<- pred$fit-Qtdist*pred$se.fit
		}
		
		if(CI.method %in% c("t", "normal") && !use.log)
		{
			pred$fit 	<- exp(pred$fit)
			CIlower		<- exp(CIlower)
			CIupper		<- exp(CIupper)
		}
		
		if(use.log)
			pred$Mean <- log(pred$Mean)
		
		if(CI.method == "chisq" && use.log)
		{
			pred$fit 	<- log(pred$fit)
			CIlower		<- log(CIlower)
			CIupper		<- log(CIupper)
			
			if(type == "cv")
			{
				pred$fit 	<- pred$fit + log(100) - pred$Mean
				CIlower		<- CIlower  + log(100) - pred$Mean
				CIupper		<- CIupper  + log(100) - pred$Mean
			}
		}
		
		if(CI.method != "chisq")
		{
			if(!use.log)
			{	
				if(type %in% c("sd", "cv"))
				{
					pred$fit 	<- sqrt(pred$fit)
					CIlower		<- sqrt(CIlower )
					CIupper		<- sqrt(CIupper )
					
					if(type == "cv")
					{
						pred$fit 	<-  100 * pred$fit / Mean
						CIlower		<-  100 * CIlower  / Mean	
						CIupper		<-  100 * CIupper  / Mean	
					}
				}	
			}
			else
			{	
				if(type %in% c("sd", "cv"))
				{
					pred$fit 	<- pred$fit/2
					CIlower		<- CIlower / 2
					CIupper		<- CIupper / 2
					
					if(type == "cv")
					{
						pred$fit 	<-  pred$fit + log(100) - Mean
						CIlower		<-  CIlower  + log(100) - Mean
						CIupper		<-  CIupper  + log(100) - Mean
					}
				}	
			}
		}
		pred$CIlower 	<- CIlower
		pred$CIupper 	<- CIupper
		attr(pred, "conf.level") <- 1-alpha 
		
		colnames(pred)	<- c("Mean", "Fitted", "SE", "Scale", "LCL", "UCL")
	}
	
	if(any(pred$Fitted %in% c(NA, NaN)))
		warning("Numerical problems occurred during prediction generating 'NaN' and/or 'NA' values!")
	
	pred
}

#' Finding X-Value for Given Y-Value Using a Bisection-Approach.
#' 
#' For given variability-values (Y-axis) on one of three scales (see 'type'), those values on
#' the X-axis are determined which give fitted values equal to the specification. 
#' 
#' This is achieved using a bisection algorithm which converges according to the specified tolerance 'tol'.
#' In case of 'type="cv"', i.e. if specified Y-values are coefficients of variation, these are interpreted
#' as percentages (15 = 15\%).
#' 
#' @param obj				(object) of class 'VFP'
#' @param type				(character) "vc" = variance, "sd" = standard deviation = sqrt(variance), 
#' 							"cv" = coefficient of variation
#' @param model.no			(integer) specifying which model to use in case 'obj' represents multiple
#' 							fitted models
#' @param alpha				(numeric) value specifying the 100x(1-alpha)\% confidence interval for the 
#' 							predicted value(s)
#' @param newdata			(numeric) values representing variability-values on a specific scale ('type')
#' @param tol				(numeric) tolerance value relative to 'newdata' specifying the stopping criterion
#' 							for the bisection algorithm, also used to evaluate equality of lower and upper bounds
#' 							in a bisection step for checking whether a boundary can be determined or not 
#' @param ci				(logical) indicates whether confidence intervals for predicted concentrations are
#' 							required (TRUE) or not (FALSE), if 'newdata' contains many values the overall computation
#' 							time can be minimized to 1/3 leaving out runs of the bisection-algorithm for LCL and UCL  
#' @param ...				additional parameter passed forward or used internally
#' 
#' @return (data.frame) with variables "Mean" (X-value), "VC", "SD" or "CV" depending on 'type',
#' 			"Diff" the difference to the specified Y-value, "LCL" and "UCL" as limits of the 100x(1-alpha)\% CI. 
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @seealso \code{\link{fit.vfp}}, \code{\link{predict.VFP}}, \code{\link{plot.VFP}}
#' 
#' @examples 
#' \donttest{
#' 
#' # perform variance component analyses first
#' library(VCA)
#' data(CA19_9)
#' fits.CA19_9 <- anovaVCA(result~site/day, CA19_9, by="sample")
#' 
#' # extract repeatability
#' mat.CA19_9 <- getMat.VCA(fits.CA19_9, "error")
#' res.CA19_9 <- fit.vfp(mat.CA19_9, 1:10)
#' summary(res.CA19_9)
#' print(res.CA19_9)
#' 
#' # predict CA19_9-concentration with 5\% CV
#' predictMean(res.CA19_9, newdata=5) 
#' 
#' # this is used in function plot.VFP as well
#' plot(res.CA19_9, Prediction=list(y=5), type="cv")
#' plot(res.CA19_9, Prediction=list(y=5), type="cv", 
#' 		xlim=c(0, 80), ylim=c(0, 10))
#' }

predictMean <- function(obj, type=c("vc", "sd", "cv"), model.no=NULL, 
		alpha=.05, newdata=NULL, tol=1e-4, ci=TRUE, ...)
{
	call 	<- match.call()
	CI.type	<- call$CI.type
	if(is.null(CI.type))
		CI.type <- "estimate"
	
	stopifnot(!is.null(newdata))
	stopifnot(class(obj) == "VFP")
	type <- match.arg(type[1], choices=c("vc", "sd", "cv"))
	
	if(is.null(model.no))				# automatically determine best fitting model
	{
		AIC <- obj$AIC
		num <- which(AIC== min(AIC))[1]
		AIC <- NULL
	}
	else
	{
		models <- sub("Model_", "", names(obj$RSS))
		if(!model.no %in% models)
			stop("Specified model ", paste0("'", model.no, "'")," is not among fitted models: ", paste(models, collapse=", "),"!")
		else
			num <- which(models == model.no)
	}
	
	model <- as.numeric(sub("Model_", "", names(obj$RSS[num])))	
	
	### start bisection algorithm to find mean for given vc, sd or cv value
	
	if(length(newdata) > 1)
	{
		return(as.data.frame(t(sapply(newdata, function(x) predictMean(obj=obj, newdata=x, type=type, tol=tol, model.no=model, ci=ci)))))
	}
	
	# adapt relative convergence tolerance to absolute value
	tol0 <- tol											# original relative tolerance
	tol  <- newdata * tol
	
	Min <- min(obj$Data$Mean, na.rm=TRUE)				# extrema
	Max <- max(obj$Data$Mean, na.rm=TRUE)
	
	Type <- switch(	type, 
			vc = "VC",
			sd = "SD",
			cv = "CV")
	
	# narrow search space, of special interst for functions with multiple intersections with target variance
	
	# iteratively increase range of X-values if necessary
	for(i in 1:5)
	{	
		cand	<- seq(Min, Max, length.out=100)
		pred 	<- predict(obj, model.no=model.no, type=type, newdata=cand)
		
		above	<- switch(	CI.type,
				estimate= which(pred$Fitted > newdata),
				LCL 	= which(pred$LCL 	> newdata),
				UCL		= which(pred$UCL 	> newdata)
		)
		below	<- switch(	CI.type,
				estimate= which(pred$Fitted < newdata),
				LCL 	= which(pred$LCL 	< newdata),
				UCL	  	= which(pred$UCL 	< newdata)
		)	
		
		if((length(above) == 0 || length(below) == 0) )
		{
			Max <- Max * 10
			Min <- Min / 10	
		}
		else
			break
	}
	
	if(CI.type == "estimate" && (length(above) == 0 || length(below) == 0) )
	{
		message(paste0("No intersection with variance-function found for specified Y-value up to X=",Max,"!"))
		res <- data.frame(Mean=NA, Y=newdata, Diff=NA, LCL=NA, UCL=NA)
		colnames(res)[2] <- Type
		return(res)
	}
	
	if(length(above) == 0)
		above <- NULL
	if(length(below) == 0)
		below <- NULL
	
	if(!is.null(above))
	{
		above1 	<- above[ -length(above)]	# predecessor
		above2 	<- above[2:length(above)]	# successor
	}
	if(!is.null(below))
	{
		below1 	<- below[ -length(below)]	# predecessor
		below2 	<- below[2:length(below)]	# successor
	}
	
	suppressWarnings(
			if(type == "cv")
					{			
						if(!is.null(above))
						{
							ind1	<- min(which(above1+1 != above2))
							Min 	<- cand[ind1]
						}
						if(!is.null(below))
						{
							ind2	<- min(below)
							Max		<- cand[ind2]
						}
					}
					else
					{
						if(!is.null(above))
						{
							ind1	<- min(above)
							Max 	<- cand[ind1]
						}
						if(!is.null(below))
						{
							ind2	<- min(which(below1+1 != below2))
							Min 	<- cand[ind2]
						}
					}
	)
	
	lower <- 0
	upper <- Max
	conc  <- lower + diff(c(lower, upper))/2
	best  <- c(Est=Inf, Diff=Inf)
	
	while(1)
	{
		pred <- predict(obj, newdata=conc, type=type, model.no=model)
		
		pred <- switch(	CI.type,
				estimate = pred[1, "Fitted"],
				LCL		 = pred[1, "LCL"],
				UCL		 = pred[1, "UCL"])
		
		Diff <- abs(newdata - pred)
		
		if(Diff < best["Diff"])							# remember best fit in case of non-convergence
			best <- c(Est=conc, Diff=Diff)
		
		if( pred < newdata && type == "cv" ||
				pred > newdata && type != "cv")
		{
			upper 	<- conc
			conc 	<- conc - (conc - lower) / 2
		}
		if( pred < newdata && type != "cv" ||
				pred > newdata && type == "cv")
		{
			lower 	<- conc
			conc 	<- conc + (upper-conc) / 2
		}
		
		if(Diff < tol || abs(diff(c(lower, upper))) < tol0*lower)#.Machine$double.eps)
		{
			if(Diff >= tol)								# not converged
			{
				if( (type == "cv" && is.null(below)) || (type != "cv" && is.null(above)) )
				{
					message("Upper bound of the CI could not be determined, return max(X-value)!")
					conc <- Max
				}	
				else if( (type == "cv" && is.null(above)) || (type != "cv" && is.null(below)) )
				{
					message("Lower bound of the CI could not be determined, return min(X-value)!")
					conc <- Min	
				}
				else
				{
					message("Convergence criterion not met, return best approximation!")
					conc <- best["Est"]
				}
				
				Diff <- best["Diff"]
			}
			break
		}
	}
	
	res <- data.frame(Mean=conc, Yvalue=newdata, Diff=Diff)
	
	if(ci)
	{
		# CI for prediction derived from CI of variance at prediction
		LCL <- UCL <- NULL
		
		if(CI.type == "estimate")
		{
			LCL	<- predictMean(obj, model.no=model.no, newdata=newdata, alpha=alpha, CI.type="LCL", type=type)$Mean
			UCL	<- predictMean(obj, model.no=model.no, newdata=newdata, alpha=alpha, CI.type="UCL", type=type)$Mean
			
			if(LCL < UCL)						# on variance-scale function is increasing on CV-scale decreasing
				CI  <- c(LCL=LCL, UCL=UCL)
			else
				CI <- c(LCL=UCL, UCL=LCL)
		}
		
		res <- data.frame(Mean=conc, Yvalue=newdata, Diff=Diff)
		
		if(CI.type == "estimate")
		{
			res$LCL <- CI["LCL"]
			res$UCL <- CI["UCL"]
			colnames(res)[2] <- Type
		}	
	}
	
	return(res)
}

#' Add a Grid to an Existing Plot.
#' 
#' It is possible to use automatically determined grid lines (\code{x=NULL, y=NULL}) or specifying the number 
#' of cells \code{x=3, y=4} as done by \code{grid}. Additionally, x- and y-locations of grid-lines can be specified,
#' e.g. \code{x=1:10, y=seq(0,10,2)}.
#' 
#' @param x         (integer, numeric) single integer specifies number of cells, numeric vector specifies vertical grid-lines
#' @param y         (integer, numeric) single integer specifies number of cells, numeric vector specifies horizontal grid-lines
#' @param col       (character) color of grid-lines
#' @param lwd       (integer) line width of grid-lines
#' @param lty       (integer) line type of grid-lines
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}

addGrid <- function(x=NULL, y=NULL, col="lightgray", lwd=1L, lty=3L)
{
	if(all(is.null(c(x,y))) || all(length(c(x,y))<2))               # call grid function
		grid(nx=x, ny=y, col=col, lwd=lwd, lty=lty)
	else
	{
		if(length(x) == 0)                                          # NULL
			xticks <- axTicks(side=1)
		else if(length(x) == 1)
		{
			U <- par("usr")
			xticks <- seq.int(U[1L], U[2L], length.out = x + 1)
		}
		else
			xticks <- x
		
		if(length(y) == 0)                                          # NULL
			yticks <- axTicks(side=2)
		else if(length(y) == 1)
		{
			U <- par("usr")
			yticks <- seq.int(U[3L], U[4L], length.out = y + 1)
		}
		else
			yticks <- y
		
		abline(v=xticks, col=col, lwd=lwd, lty=lty)
		abline(h=yticks, col=col, lwd=lwd, lty=lty)
	}
}

#' Convert Color-Name or RGB-Code to Possibly Semi-Transparent RGB-code.
#' 
#' Function takes the name of a color and converts it into the rgb space. Parameter "alpha" allows
#' to specify the transparency within [0,1], 0 meaning completey transparent and 1 meaning completey
#' opaque. If an RGB-code is provided and alpha != 1, the RGB-code of the transparency adapted color 
#' will be returned.
#' 
#' @param col (character) name of the color to be converted/transformed into RGB-space (code). Only
#'               those colors can be used which are part of the set returned by function colors(). Defaults
#'               to "black".
#' @param alpha (numeric) value specifying the transparency to be used, 0 = completely transparent, 
#'               1 = opaque.
#' 
#' @return RGB-code
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @examples 
#' # convert character string representing a color to RGB-code
#' # using alpha-channel of .25 (75\% transparent)
#' as.rgb("red", alpha=.25)
#' 
#' # same thing now using the RGB-code of red (alpha=1, i.e. as.rgb("red"))
#' as.rgb("#FF0000FF", alpha=.25)

as.rgb <- function(col="black", alpha=1)
{
	if(length(col) > 1 && (length(alpha) == 1 || length(alpha) < length(col)))         # unclear which alpha to use or only one alpha specified
	{
		if(length(alpha) < length(col) && length(alpha) > 1)
			warning("Multiple (but too few) 'alpha' specified! Only use 'alpha[1]' for each color!")
		return(sapply(col, as.rgb, alpha=alpha[1]))
	}
	if(length(col) > 1 && length(col) <= length(alpha))                                 # process each color separately
	{
		res <- character()
		for(i in 1:length(col))
			res <- c(res, as.rgb(col[i], alpha[i]))
		return(res)
	}
	if( col %in% colors() )
		return( rgb(t(col2rgb(col))/255, alpha=alpha) )
	else
	{
		col <- sub("#", "", col)
		R <- as.numeric(paste("0x", substr(col, 1,2), sep=""))
		G <- as.numeric(paste("0x", substr(col, 3,4), sep=""))
		B <- as.numeric(paste("0x", substr(col, 5,6), sep=""))
		return( rgb(R/255, G/255, B/255, alpha=alpha, maxColorValue=1) )
	}        
}


#' Fit CLSI EP17 Model Using log-transformaed X and Y.
#' 
#' This function fits the model proposed in CLSI EP17 by log-transforming
#' CV (Y) as well as mean-values (X) und performing a linear regression of these.
#' More specifically CV = A * Conc^B, where Conc = mean concentration of a sample and CV is
#' on the percent-scale, is fitted by ordinary least squares (OLS) estimation of
#' log(CV) = A + B * log(Conc). Fitted values are subsequently back-transformed
#' using formula cv = exp(a) * C^b, where cv, a and b represent estimates of CV, A and B.
#' 
#' The AIC is computed following the implementation of \code{extractAIC.lm} in the
#' 'stats' package with the adaption of using 'n = sum(df)' instead of 'n' being the number
#' of residuals. The 'df' come from a precision analysis, thus, there are far more observations
#' used to fit this model than indicated by the number of residuals. 
#'
#' @param x			(numeric) mean concentrations of samples
#' @param y			(numeric) variability at 'x' on VC-, SD-, or CV-scale
#' @param DF		(numeric) vector of degrees of freedom linked to variabilities 'y'
#' 					used in derivation of deviance and AIC  
#' @param typeY		(character) specifying the scale of 'y'-values
#' @param k			(numeric) numeric specifying the 'weight' of the equivalent
#' 					degrees of freedom (edf) part in the AIC formula.
#' @param ...		additional arguments
#' 
#' @return (list) with items "x" and "y" as provided, and "x.out" and "y.out"
#'          representing X- and Y-coordiantes of fitted values for plotting
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @examples 
#' \donttest{
#' # data from appendix D of CLSI EP17-A2 (pg. 54)
#' EP17.dat <- data.frame(
#'  Lot=c(rep("Lot1", 9), rep("Lot2", 9)),
#' 	Mean=c(	0.04, 0.053, 0.08, 0.111, 0.137, 0.164, 0.19, 0.214, 0.245,
#' 			0.041, 0.047, 0.077, 0.106, 0.136, 0.159, 0.182, 0.205, 0.234),
#' 	CV=c(40.2, 29.6, 19.5, 15.1, 10.0, 7.4, 6.0, 7.5, 5.4,
#' 		 44.1, 28.8, 15.1, 17.8, 11.4, 9.2, 8.4, 7.8, 6.2),
#'  SD=c(0.016, 0.016, 0.016, 0.017, 0.014, 0.012, 0.011, 0.016, 0.013,
#' 		 0.018, 0.014, 0.012, 0.019, 0.016, 0.015, 0.015, 0.016, 0.014),
#'  DF=rep(1, 18)
#' )
#' 
#' EP17.dat$VC <- EP17.dat$SD^2
#' 
#' lot1 <- subset(EP17.dat, Lot=="Lot1")
#' lot2 <- subset(EP17.dat, Lot=="Lot2")
#' 
#' # function fit.EP17 is not exported, use package namesspace in call
#' fit.lot1 <- VFP:::fit.EP17(x=lot1$Mean, y=lot1$CV, typeY="cv", DF=lot1$DF)
#' }

fit.EP17 <- function(x, y, DF, typeY=c("vc", "sd", "cv"), k=2, ...)
{
	x		<- as.numeric(unlist(x))
	y		<- as.numeric(unlist(y))
	typeY 	<- match.arg(typeY[1], choices=c("vc", "sd", "cv"))
	x.out 	<- y.out <- NULL
	
	if(typeY == "vc")
	{
		y.vc 	<- y
		y 		<- 100*sqrt(y)/x
	}
	else if(typeY == "sd")
	{
		y.vc 	<- y^2
		y 		<- 100*y/x
	}
	else
	{
		y.vc <- (y*x/100)^2
	}
	
	xl 				<- log(x)
	yl 				<- log(y)
	fit 			<- lm(yl~xl, weights=DF)
	fit$fit.orig	<- fit
	fitted.vc		<- predict.modelEP17(fit, ci.type="vc")$Fitted	# transform to variance-scale for comparison to other models
	residuals		<- y.vc - fitted.vc		
	n				<- sum(DF)										# taking into account that VCA-results are based on way larger data set
	edf				<- n - fit$df.residual
	fit$RSS			<- sum(residuals^2)								# RSS, deviance and AIC should now be on the variance-scale as well
	fit$RSSw		<- sum(DF*residuals^2)
	fit$deviance 	<- n * log(fit$RSSw/n)							# RSS on variance-scale, thus, comparable to other models
	fit$AIC			<- fit$deviance + k * edf	
	
	class(fit) <- "modelEP17"
	
	fit	
}

#' Predict Method for Objects of Class 'modelEP17'.
#' 
#' This is a helper function not intented to be used directly.
#' 
#' @param object		(object) of class 'modelEP17'
#' @param newdata		(numeric) vector of data points at which prediction shall be made
#' @param alpha			(numeric) value defining the 100(1-alpha)\% confidence interval for
#' 						predictions
#' @param ci.type		(character) string specifying on which scale prediction shall be made
#' @param CI.method		(character) string specifying which type of CI shall be used
#' @param use.log		(logical) TRUE X- and Y-values will be returned on log-scale
#' @param ...			additional arguments
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}

predict.modelEP17 <- function(	object, newdata=NULL, alpha=.05,
		ci.type=c("vc", "sd", "cv"), CI.method=c("chisq", "t", "normal"), 
		use.log=FALSE,  ...)
{	
	CI.method	<- match.arg(CI.method[1], choices=c("chisq", "t", "normal"))
	ci.type 	<- match.arg(ci.type[1], choices=c("vc", "sd", "cv", "log"))
	
	if(is.null(newdata))
	{
		Mean	<- exp(object$model$xl)
		newdata <- object$model$xl		# fitted values will be returned
		x.out	<- exp(newdata)
	}
	else
	{
		x.out <- Mean <- newdata
		
		if(is.numeric(newdata))
			newdata <- log(newdata)
		else
			newdata <- log(newdata$Mean)
	}
	Mean		<- as.numeric(unlist(Mean))
	pred 		<- predict(object$fit.orig, newdata=data.frame(xl=newdata), se.fit=TRUE)
	pred		<- as.data.frame(pred)
	pred$fit	<- as.numeric(pred$fit)
	pred$se.fit	<- as.numeric(pred$se.fit)
	if(CI.method == "normal")
	{
		Qnorm 		<- qnorm(1-alpha/2)
		CIupper 	<- exp(pred$fit+Qnorm*pred$se.fit)
		CIlower		<- exp(pred$fit-Qnorm*pred$se.fit)
		pred$fit	<- exp(as.numeric(pred$fit))
	}
	else if(CI.method == "chisq")
	{
		pred$fit	<- exp(as.numeric(pred$fit))
		pred$se.fit <- pred$se.fit * pred$fit			# adjust standard errors for original CV-scale
		df.qchisq		<- 0.5*(pred$fit/pred$se.fit)^2
		lower.qchisq 	<- qchisq(1-alpha/2, df=df.qchisq)
		upper.qchisq 	<- qchisq(alpha/2, df=df.qchisq)
		CIlower			<- pred$fit*sqrt(df.qchisq/lower.qchisq)
		CIupper			<- pred$fit*sqrt(df.qchisq/upper.qchisq)
	}
	else if(CI.method == "t")
	{
		Qtdist 		<- qt(1 - alpha/2, df.residual(object$fit.orig))
		CIupper 	<- exp(pred$fit+Qtdist*pred$se.fit)
		CIlower		<- exp(pred$fit-Qtdist*pred$se.fit)
		pred$fit	<- exp(as.numeric(pred$fit))
	}
	
	pred$CIlower 	<- CIlower
	pred$CIupper 	<- CIupper
	attr(pred, "conf.level") <- 1-alpha 
	
	# transfrom fit and CIlower and CIupper to respective user-requested scale
	if(ci.type %in% c("sd", "vc"))
	{
		pred$fit 		<- pred$fit * Mean / 100
		pred$CIlower 	<- pred$CIlower * Mean / 100
		pred$CIupper 	<- pred$CIupper * Mean / 100
		
		if(ci.type == "vc")
		{
			pred$fit 		<- pred$fit^2
			pred$CIlower 	<- pred$CIlower^2
			pred$CIupper 	<- pred$CIupper^2
		}
	}
	
	if(use.log)
	{
		pred$fit 		<- log(pred$fit)
		pred$CIlower 	<- log(pred$CIlower)
		pred$CIupper 	<- log(pred$CIupper)
	}
	
	colnames(pred)	<- c("Fitted", "SE", "Mean", "Scale", "LCL", "UCL")
	
	pred$Mean <- Mean
	
	return(pred)
}


#' Add Legend to Right Margin.
#' 
#' This function accepts all parameters applicable in and forwards them to function \code{\link{legend}}.
#' There will be only made some modifications to the X-coordinate ensuring that the legend is plotted in
#' the right margin of the graphic device. Make sure that you have reserved sufficient space in the right
#' margin, e.g. 'plot.VFP(....., mar=c(4,5,4,10))'.
#' 
#' @param x			(character, numeric) either one of the character strings "center","bottomright", "bottom", "bottomleft", 
#' 					"left", "topleft", "top", "topright", "right" or a numeric values specifying the X-coordinate in user
#' 					coordinates
#' @param y			(numeric) value specifying the Y-coordiante in user coordinates, only used in case 'x' is numeric
#' @param offset	(numeric) value in [0, 0.5] specifying the offset as fraction in regard to width of the right margin
#' @param ...		all parameters applicable in function \code{\link{legend}}
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @examples 
#' \donttest{
#' library(VCA)
#' data(VCAdata1)
#' # perform VCA-anaylsis
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by="sample")
#' # transform list of VCA-objects into required matrix
#' mat <- getMat.VCA(lst)		# automatically selects "total"
#' mat
#' 
#' # fit all 9 models batch-wise
#' res <- fit.vfp(model.no=1:10, Data=mat)
#' 
#' plot(res, mar=c(5.1, 4.1, 4.1,15), Crit=NULL)
#' 
#' legend.rm(cex=1.25, text.font=10,
#' 	 legend=c(
#'     paste0("AIC:    ", signif(as.numeric(res$AIC["Model_6"]), 4)),
#' 	   paste0("Dev:    ", signif(as.numeric(res$Deviance["Model_6"]), 4)),
#'     paste0("RSS:    ", signif(as.numeric(res$RSS["Model_6"]),4))))

#' }

legend.rm <- function(	x=c("center","bottomright", "bottom", "bottomleft", 
				"left", "topleft", "top", "topright", "right"), 
		y=NULL, offset=.05, ...)
{
	stopifnot(	is.numeric(x) || is.character(x) )
	if(is.character(x))
	{
		x <- match.arg(x[1], choices=c("center","bottomright", "bottom", "bottomleft", 
						"left", "topleft", "top", "topright", "right"))
	}else
	{
		stopifnot(is.numeric(y))
	}
	
	par(xpd=TRUE)
	args <- list(...)
	
	USR  <- par("usr")
	PLT  <- par("plt")
	FIG  <- par("fig")
	
	wrm <- FIG[2] - PLT[2]						# width right margin
	hrm	<- PLT[4] - PLT[3]
	
	if(is.character(x))
	{
		X.orig	<- x
		xjust 	<- 0.5							# defaults to center
		x 		<- PLT[2] + 0.5 * wrm
		yjust   <- 0.5
		y		<- PLT[3] + 0.5 * hrm
		
		if(grepl("left", X.orig))
		{
			xjust 	<- 0
			x 		<- PLT[2] + offset * wrm
		}		
		if(grepl("right", X.orig))
		{
			xjust 	<- 1
			x 		<- PLT[2] + (1-offset) * wrm
		}
		if(grepl("top", X.orig))
		{
			yjust 	<- 1
			y 		<- PLT[3] + (1-offset) * hrm
		}
		if(grepl("bottom", X.orig))
		{
			yjust 	<- 0
			y 		<- PLT[3] + offset * hrm
		}
	}
	
	x <- grconvertX(x, from="nic", to="user")
	y <- grconvertY(y, from="nic", to="user")
	
	args$x 		<- x
	args$y 		<- y
	args$xjust 	<- xjust
	args$yjust 	<- yjust
	
	do.call(legend, args)
	par(xpd=FALSE)
}