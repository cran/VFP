# TODO: Add comment
#
# Author: schueta6
###############################################################################


#' Keep backward compatibility by assigning new to old function names when
#' loading the package. Old name did not comply to new CRAN package check rules.
#' 
#' @param libname     (character) string giving the library directory where
#'                    the package defining the namespace was found
#' @param pkgname     (character) string giving the name of the package
#' 

.onLoad<- function(libname, pkgname)
{
	fit.vfp           <<- fit_vfp
	fit.EP17          <<- fit_ep17
	predict.modelEP17 <<- predict_model_ep17
	predictMean       <<- predict_mean
	deriveCx          <<- derive_cx
	precisionPlot     <<- precision_plot
	addGrid           <<- add_grid
	getMat.VCA        <<- get_mat
	Signif            <<- signif2
	legend.rm         <<- legend_rm
}


#' Internal Function Model 2.
#' @param x    (numeric) parameter

powfun2simple <- function(x) {
	list(
			predictors = list(beta1 = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste(predictors[1], "*", variables[1], "^2")
			}
	)
}
class(powfun2simple) <- "nonlin"

#' Internal Function Model 3.
#' @param x    (numeric) parameter

powfun3 <- function(x) {
	cutval <- 0.95
	xmax   <- max(x)
	list(
			predictors = list(beta1 = 1, beta2 = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste("exp(", predictors[1], ")*(1+(exp(", predictors[2], ")-",
						cutval, ")*", variables[1], "/", xmax, ")")
			}
	)
}
class(powfun3) <- "nonlin"

#' Internal Function Model 3.
#' @param x    (numeric) parameter

powfun3simple <- function(x) {
	list(
			predictors = list(beta1 = 1, beta2 = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste(predictors[1], "+", predictors[2], "*", variables[1], "^2")
			}
	)
}
class(powfun3simple) <- "nonlin"



#' Internal Function Model 4.
#' @param x      (numeric) parameter 1
#' @param potenz  (numeric) parameter 2

powfun4 <- function(x, potenz) {
	cutval   <- 0.95
	xmax  <- max(x)
	list(
			predictors = list(beta1 = 1, beta2 = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste("exp(", predictors[1], ")*(1+(exp(", predictors[2], ")-",
						cutval, ")*", variables[1], "/", xmax, ")^", potenz)
			}
	)
}
class(powfun4) <- "nonlin"


#' Internal Function Model 4.
#' @param x      (numeric) parameter 1
#' @param potenz  (numeric) parameter 2

powfun4simple <- function(x, potenz) {
	list(
			predictors = list(beta1 = 1, beta2 = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste("(", predictors[1], "+", predictors[2], "*", variables[1],
						")^", potenz)
			}
	)
}
class(powfun4simple) <- "nonlin"


#' Internal Function Model 5.
#' @param x      (numeric) parameter 1

powfun5 <- function(x) {
	cutval   <- 0.95
	xmax  <- max(x)
	
	list(
			predictors = list(beta1 = 1, beta2 = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste("exp(", predictors[1], ")*(1+(exp(", predictors[2], ")-",
						cutval, ")*", variables[1], "/", xmax, ")")
			}
	)
}
class(powfun5) <- "nonlin"

#' Internal Function Model 5.
#' @param x    (numeric) parameter 1
#' @param K    (numeric) parameter 2

powfun5simple <- function(x, K) {
	list(
			predictors = list(beta1 = 1, beta2 = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste(predictors[1], "+", predictors[2], "*", variables[1], "^", K)
			}
	)
}
class(powfun5simple) <- "nonlin"


#' Internal Function Model 6.
#' @param x      (numeric) parameter 1

powfun6 <- function(x) {
	list(
			predictors = list(beta1 = 1, beta2 = 1, beta3 = 1, pot = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste("exp(", predictors[1], ")*(1+(atan(", predictors[2],
						")/pi-0.5)*", variables[1],
						"*exp(", predictors[3], ")*(1+exp(", predictors[4],
						")-(", variables[1], "*exp(", predictors[3],
						"))^exp(", predictors[4], "))/exp(", predictors[4], "))")
			}
	)
}
class(powfun6) <- "nonlin"

#' Internal Function Model 6.
#' @param x      (numeric) parameter 1

powfun6simple <- function(x) {
	list(
			predictors = list(beta1 = 1, beta2 = 1, beta3 = 1, pot = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste(predictors[1], "+", predictors[2], "*", variables[1], "+",
						predictors[3], "*", variables[1], "^", predictors[4])
			}
	)
}
class(powfun6simple) <- "nonlin"


#' Internal Function Model 7.
#' @param x      (numeric) parameter 1

powfun7 <- function(x) {
	cutval <- 0.95
	xmax  <- max(x)
	
	list(
			predictors = list(beta = 1, beta2 = 1, pot = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste("exp(", predictors[1], ")*(1+(exp(", predictors[2], ")-",
						cutval, ")*(", variables[1], "/", xmax, ")^((0.1+10*exp(",
						predictors[3], "))/(1+exp(", predictors[3], "))))")
			}
	)
}
class(powfun7) <- "nonlin"


#' Internal Function Model 7.
#' @param x      (numeric) parameter 1

powfun7simple <- function(x) {
	list(
			predictors = list(beta = 1, beta2 = 1, pot = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste(predictors[1], "+", predictors[2], "*", variables[1],
						"^", predictors[3])
			}
	)
}
class(powfun7simple) <- "nonlin"


#' Internal Function Model 8.
#' @param x      (numeric) parameter 1
#' @param C      (numeric) parameter 2
#' @param signJ    (numeric) parameter 3

powfun8 <- function(x, C, signJ) {
	cutval <- 0.95
	
	list(
			predictors = list(beta1 = 1, beta2 = 1, beta3 = 1),
			variables = list(substitute(x / C)),
			term = function(predictors, variables) {
				paste("exp(", predictors[1], ")*(1+(exp(", predictors[2], ")-",
						cutval, ")*", variables[1], ")^(", signJ, "*(0.1+10*exp(",
						predictors[3], "))/(1+exp(", predictors[3], ")))")
			}
	)
}
class(powfun8) <- "nonlin"


#' Internal Function Model 8.
#' @param x      (numeric) parameter 1

powfun8simple <- function(x) {
	list(
			predictors = list(beta1 = 1, beta2 = 1, pot = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste("(", predictors[1], "+", predictors[2], "*",
						variables[1], ")^", predictors[3], sep = " ")
			}
	)
}
class(powfun8simple) <- "nonlin"

#' Internal Function Model 9.
#' @param x      (numeric) parameter 1

powfun9simple <- function(x) {
	list(
			predictors = list(beta1 = 1, pot = 1),
			variables = list(substitute(x)),
			term = function(predictors, variables) {
				paste(predictors[1], "*", variables[1], "^", predictors[2], sep = " ")
			}
	)
}
class(powfun9simple) <- "nonlin"



#' Estimate (Im)Precision-Profiles Modeling the Relationship 'Var ~ Mean'.
#'
#' This function fits one out of ten or any subset of ten non-linear functions
#' at once. This is done on precision data consisting of information about the
#' variance, concentration at which this variance was observed and the
#' respective degrees of freedom. Provided data must contain at least three
#' columns with this information. There are following variance-functions
#' covered: \cr
#' \describe{
#'   \item{constant variance}{ \eqn{sigma^2}}
#'   \item{constant CV}{\eqn{ sigma^2 = beta_1*u^2 }}
#'   \item{mixed constant, proportional variance}{
#' 		\eqn{sigma^2 = beta_1+beta_2*u^2}}
#'   \item{constrained power model, constant exponent}{
#' 		\eqn{sigma^2 = (beta_1+beta_2*u)^K}}
#'   \item{alternative constrained power model}{
#' 			\eqn{sigma^2 = beta_1+beta_2*u^K}}
#'   \item{alternative unconstrained power model for VF's with a minimum}{
#' 		\eqn{sigma^2 = beta_1+beta_2*u+beta_3*u^J}}
#'   \item{alternative unconstrained power model}{
#' 		\eqn{sigma^2 = beta_1+beta_2*u^J}}
#'   \item{unconstrained power model (default model of Sadler)}{
#' 		\eqn{sigma^2 = (beta_1 + beta_2 * u)^J}}
#'   \item{CLSI EP17 similar model}{ \eqn{sigma^2 = beta_1 * u^J}}
#'   \item{Exact CLSI EP17 model (fitted by linear regression on logarithmic
#' 		scale)}{ \eqn{cv = beta_1 * u^J}}
#' }
#' Fitting all ten models is somehow redundant if constant 'C' is chosen to be
#' equal to 2, since models 3 and 5 are equivalent and these are constrained
#' versions of model 7 where the exponent is also estimated. The latter also
#' applies to model 4 which is a constrained version of model 8. Note that
#' model 10 fits the same predictor function as model 9 using simple linear
#' regression on a logarithmic scale. Therefore, regression statistics like
#' AIC, deviance etc. is not directly comparable to that of models 1 to 9.
#' Despite these possible redundancies, as computation time is not critical
#' here for typical precision-profiles (of in-vitro diagnostics precision
#' experiments) we chose to offer batch-processing as well.
#' During computation, all models are internally reparameterized so as to
#' guarantee that the variance function is positive in the range 'u' from 0
#' to 'u_max'. In models 7 and 8, 'J' is restricted to 0.1<J<10 to avoid
#' the appearance of sharp hooks.
#' Occasionally, these restrictions may lead to a failure of convergence.
#' This is then a sign that the model parameters are on the boundary and that
#' the model fits the data very badly. This should not be taken as reason for
#' concern. It occurs frequently for model 6 when the variance function has
#' no minimum, which is normally the case.
#'
#' Functions \code{\link{predict.VFP}} and \code{\link{predict_mean}} are
#' useful to make inference based on a fitted model. #' It is possible
#' to derive concentrations at which a predefined variability is reached,
#' which is sometimes referred to as #' "functional sensitivity" and/or
#' "limit of quantitation" (LoQ). Funtion \code{\link{predict_mean}} returns
#' the fitted value #' at which a user-defined variance ("vc"), SD or CV is
#' reached with its corresponding 100(1-alpha)\% CI derived from the CI of
#' the fitted model. The plotting method for objects of class 'VFP' can
#' automatically add this information to a plot using arguments 'Prediction'
#' and 'Pred.CI' (see \code{\link{plot.VFP}} for details. Function
#' \code{\link{predict.VFP}} makes #' predictions for specified mean-values
#' based on fitted models.
#'
#' Note, that in cases where a valid solution was found in the re-
#' parameterized space but the final fit with 'gnm' in the original
#' parameter-space did not converge no variance-covariance matrix can be
#' estimated. Therefore, no confidence-intervals will be available downstream.
#'
#' @param Data			(data.frame, matrix) containing mean-values, estimated
#' 						variances and degrees of freedom for each sample
#' @param model.no    	(integer) in 1:10, may be any combination of these
#' 						integer-values specifying models 1 to 10 which
#'             			are consequently fitted to the data; defaults to 1:9
#' @param K        		(numeric) value defining the constant used in models 4
#' 						and 5; defaults to 2
#' @param startvals    	(numeric) vector of start values for the optimization
#' 						algorithm, only used if a single model is specified by
#' 						the user, otherwise, start values will be sought
#' 						automatically
#' @param quiet      	(logical) TRUE = all output messages (warnings, errors)
#' 						and regular console output is suppressed and can
#'             			be found in elements "stderr" and "stdout" of the
#' 						returned object
#' @param col.mean    	(character) string specifying a column/variable in
#' 						'Data' containing mean-values; defaults to "Mean"
#' @param col.var    	(character) string specifying a column/variable in
#' 						'Data' containing the variances; Defaults to "VC"
#' @param col.df    	(character) string specifying a column/variable in
#' 						'Data' containing the degrees of freedom; defaults
#' 						to "DF"
#' @param col.sd    	(character) string (optional) specifying a column/
#' 						variable in 'Data' containing the SD-values, used for
#' 						model 10 to derive more precise CV-values compared to
#' 						derivation from 'col.var' in case 'col.cv' is not
#' 						specified directly. Note that column "col.var" must
#' 						nevertheless be set and existing
#' @param col.cv    	(character) string (optional) specifying a column/
#' 						variable in 'Data' containing the CV-values, if
#' 						missing, first 'col.sd' is checked, if missing
#' 						'col.var' used to derive per-sample CV-values.
#'           			Note that column "col.var" must nevertheless be set
#' 						and existing
#' @param minVC      	(numeric) value assigned to variances being equal to
#' 						zero, defaults to NA, which results in removing
#'             			these observations; could also be set to the smallest
#' 						possible positive double (\code{.Machine$double.eps})
#' @param ...      		additional parameters passed forward, e.g. 'vc' of
#' 						function \code{\link{get_mat}} for selecting a
#'             			specific variance component in case of 'Data' being a
#' 						list of 'VCA'-objects (see examples)
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
#' \item{K}{constant as specified by the user}
#' \item{startvals}{start values as specified by the user}
#' \item{errors}{messages of all errors caught}
#' \item{output}{if 'quiet = TRUE' all output that was redirected to a file}
#' \item{warning}{messages of all warnings caught}
#' \item{notes}{all other notes caught during execution}
#'
#' @author   Florian Dufey \email{florian.dufey@@roche.com},
#'       Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#'
#' @seealso \code{\link{plot.VFP}}, \code{\link{predict.VFP}},
#' 			\code{\link{predict_mean}}
#' 
#' @aliases fit.vfp
#' 
#' @examples
#' \donttest{
#' # load VCA-package and data
#' library(VCA)
#' data(VCAdata1)
#' # perform VCA-anaylsis
#' lst <- anovaVCA(y~(device+lot)/day/run, VCAdata1, by = "sample")
#' # transform list of VCA-objects into required matrix
#' mat <- get_mat(lst)    # automatically selects "total"
#' mat
#'
#' # fit all 9 models batch-wise
#' res <- fit_vfp(model.no = 1:9, Data = mat)
#'
#' # if 'mat' is not required for later usage, following works
#' # equally well
#' res2 <- fit_vfp(lst, 1:9)
#'
#' # plot best-fitting model
#' plot(res)
#' plot(res, type = "cv")
#' plot(res, type = "cv", ci.type = "lines", ci.col = "red",
#'     	Grid = list(col = "wheat"),
#' 		Points = list(pch = 2, lwd = 2, col = "black"))
#'
#' # now derive concentation at which a specific reproducibility-
#' # imprecision of 10\% is reached and add this to the plot
#' pred <- plot(res, type = "cv", ci.type = "band",
#'         ci.col = as_rgb("red", .25), Grid = list(col = "orange"),
#'         Points = list(pch = 2, lwd = 2, col = "black"),
#'         Prediction = list(y = 10, col = "red"), Pred.CI = TRUE)
#'
#' # (invisibly) returned object contains all relevant information
#' pred
#'
#' # same for repeatability
#' mat.err <- get_mat(lst, "error")
#' res.err <- fit_vfp(1:9, Data = mat.err)
#'
#' # without extracting 'mat.err'
#' res.err2 <- fit_vfp(lst, 1:9, vc = "error")
#'
#' plot(res.err)
#'
#' #######################################################################
#' # another example using CA19_9 data from CLSI EP05-A3
#'
#' data(CA19_9)
#'
#' # fit reproducibility model to data
#' fits.CA19_9 <- anovaVCA(result~site/day, CA19_9, by = "sample")
#'
#' # fit within-laboratory-model treating site as fixed effect
#' fits.ip.CA19_9 <- anovaMM(result~site/(day), CA19_9, by = "sample")
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
#' mod8.repro   <- fit_vfp(repro.CA19_9, 8)
#' mod8.ip    <- fit_vfp(ip.CA19_9, 8)
#' mod8.rep    <- fit_vfp(rep.CA19_9, 8)
#'
#' # plot reproducibility precision profile first
#' # leave enough space in right margin for a legend
#' plot(mod8.repro, mar = c(5.1, 7, 4.1, 15),
#'     type = "cv", ci.type = "none", Model = FALSE,
#'     Line = list(col = "blue", lwd = 3),
#'     Points = list(pch = 15, col = "blue", cex = 1.5),
#'     xlim = c(10, 450), ylim = c(0, 10),
#'     Xlabel = list(text = "CA19-9, kU/L (LogScale) - 3 Patient Pools,
#' 					 3 QC Materials",
#'         cex = 1.5), Title = NULL,
#'     Ylabel = list(text = "% CV", cex = 1.5),
#'     Grid = NULL, Crit = NULL, log = "x")
#'
#' # add intermediate precision profile
#' plot  (mod8.ip, type = "cv", add = TRUE, ci.type = "none",
#'     Points = list(pch = 16, col = "deepskyblue", cex = 1.5),
#'     Line = list(col = "deepskyblue", lwd = 3), log = "x")
#'
#' # add repeatability precision profile
#' plot( mod8.rep, type = "cv", add = TRUE, ci.type = "none",
#'     Points = list(pch = 17, col = "darkorchid3", cex = 1.5),
#'     Line = list(col = "darkorchid3", lwd = 3), log = "x")
#'
#' # add legend to right margin
#' legend_rm(x = "center", pch = 15:17, col = c("blue", "deepskyblue",
#' 		"darkorchid3"), cex = 1.5, legend = c("Reproducibility",
#' 		"Within-Lab Precision", "Repeatability"), box.lty = 0)
#' #' }


fit_vfp <- function(Data, model.no = 1:9, K = 2, startvals = NULL, quiet = TRUE,
		col.mean = "Mean", col.var = "VC", col.df = "DF",
		col.sd = NULL, col.cv = NULL, minVC = NA, ...) {
	
	Call <- match.call()
	stopifnot(model.no %in% 1:10)
	stopifnot(K>0)
	stopifnot(is.data.frame(Data) || is.matrix(Data) || is(Data, "list") && 
					all(sapply(Data, class) == "VCA"))
	if(is(Data, "list") && all(sapply(Data, class) == "VCA"))
		Data <- get_mat(Data, ...)
	if(is.matrix(Data))
		Data <- as.data.frame(Data)
	
	cn  <- colnames(Data)
	stopifnot(all(c(col.mean, col.var, col.df) %in% cn))
	
	stopifnot(is.numeric(Data[, col.mean]))
	stopifnot(is.numeric(Data[, col.var]))
	stopifnot(is.numeric(Data[, col.df]))
	stopifnot(is.null(col.sd) || is.character(col.sd) && is.numeric(Data[, col.sd]))
	stopifnot(is.null(col.cv) || is.character(col.cv) && is.numeric(Data[, col.cv]))
	
	Data[, col.mean]  <- as.numeric(Data[, col.mean])
	Data[, col.var] <- as.numeric(Data[, col.var])
	Data[, col.df]  <- as.numeric(Data[, col.df])
	
	ind.NegVC <- which(Data[, col.var] <= 0)
	
	if(length(ind.NegVC) > 0)
	{
		message("Variance(s) <= 0 detected! This/these will be set to 'minVC'!")
		Data[ind.NegVC, col.var] <- minVC
	}
	
	if(!is.null(col.sd))
	{
		Data[, col.sd] <- as.numeric(Data[, col.sd])
		colnames(Data)[which(cn == col.sd)] <- "SD"
	}
	if(!is.null(col.cv))
	{
		Data[, col.cv] <- as.numeric(Data[, col.cv])
		colnames(Data)[which(cn == col.cv)] <- "CV"
	}
	
	if(quiet)
	{
		file.create("./stdout.log")
	}
	
	errors <- warnings <- messages <- notes <- NULL
	
	skip5 <- FALSE
	if(all(c(3, 5) %in% model.no) && K==2)
	{
		skip5 <- TRUE                # models 3 and 5 are identical in case K = 2
		if(!quiet)
			message("Model 5 will not be fitted because it is identical to model 3 with 'K = 2'!")
		notes <- c(notes, "Model 5 will not be fitted because it is identical to model 3 with 'K = 2'!")
	}
	
	if(length(model.no) > 1)
		startvals <- NULL
	
	if(col.mean != "Mean")
	{
		if("Mean" %in% cn)
		{
			Data <- Data[, -which(colnames(Data) == "Mean")]
			cn   <- colnames(Data)
		}
		colnames(Data)[which(cn == col.mean)] <- "Mean"
	}
	if(col.var != "VC")
	{
		if("VC" %in% cn)
		{
			Data <- Data[, -which(colnames(Data) == "VC")]
			cn   <- colnames(Data)
		}
		colnames(Data)[which(cn == col.var)] <- "VC"
	}
	if(col.df != "DF")
	{
		if("DF" %in% cn)
		{
			Data <- Data[, -which(colnames(Data) == "DF")]
			cn   <- colnames(Data)
		}
		colnames(Data)[which(cn == col.df)] <- "DF"
	}
	
	ind.NA <- which(apply(Data[c("Mean", "VC", "DF")], 1, function(x) any(is.na(x))))
	if(length(ind.NA) > 0)
	{
		Data <- Data[-ind.NA, , drop = FALSE]
		message(paste(length(ind.NA), "observation(s) with at least one NA value were removed!"))
	}
	
	Data     <- subset(Data, "Mean">0)
	Data    <- subset(Data, "VC">0)
	pweights   <- Data[, "DF"]/2                     # prior weight parameters
	Mean     <- Data[, "Mean"]
	RSS     <- Df.resid <- Deviance <- AIC <- numeric(10)
	
	Formulas  <- c("sigma^{2}==beta[1]",                  # model 1
			"sigma^{2}==beta[1]*u^{2}",                # model 2
			"sigma^{2}==beta[1]+beta[2]*u^2",            # model 3
			paste("sigma^{2}==(beta[1]+beta[2]*u)^{", K, "}"),    # model 4, replace K by acutal value of K
			paste("sigma^{2}==beta[1]+beta[2]*u^{", K, "}"),      # model 5,        - " -
			"sigma^{2}==beta[1]+beta[2]*u+beta[3]*u^{J}",      # model 6
			"sigma^{2}==beta[1]+beta[2]*u^{J}",            # model 7
			"sigma^{2}==(beta[1]+beta[2]*u)^{J}",          # model 8
			"sigma^{2}==beta[1]*u^{J}",                # model 9
			"cv==beta[1]*u^{J}")                  # model 10 (true CLSI EP17 model)
	
	names(RSS)   <- names(Deviance) <- names(AIC) <- paste0("Model_", 1:10)
	
	res.gnm1   <- res.gnm2 <- res.gnm3 <- res.gnm4 <- res.gnm5 <- res.gnm6 <- res.gnm7 <- res.gnm8 <- res.gnm9 <- res.mod10 <- NULL
	mittel    <- Mu <- mu.fun <- vector("list", length = 9)
	eps      <- 1.0e-07
#  cutval    <- 0.95 #determines lower boundary in models, values between 0 and 1 are possible
	# find a crude analytical model for the calculation of the start weights
	imin = 0
	for(i in 0:2){
		if (i>0) {mindevalt<-mindev}
		mindev <- sum((Data[, "DF"]*(1-weighted.mean(Data[, "VC"]/Data[, "Mean"]^i, pweights)*Data[, "Mean"]^i/Data[, "VC"])^2))
		if (i>0){
			if(mindev<mindevalt){imin = i}
		}
	}
	startVC  <- weighted.mean(Data[, "VC"]/Data[, "Mean"]^imin, pweights)*Data[, "Mean"]^imin+0.05*weighted.mean(Data[, "VC"], pweights)
	
	
	
	################################################################################################################################
	
	if(1 %in% model.no)
	{
		cat("\nModel 1 ")
		my.form1   <- VC ~ 1                   #constant VC
		startvals  <- log(weighted.mean(Data[, "VC"], pweights)) #exact solution
		tmp.res   <- condition_handler(gnm(formula = my.form1, family = Gamma(link = "log"), data = Data, weights = pweights,
						start = startvals, trace = TRUE), file = "./stdout.log")
		if(tmp.res$status != 2)
		{
			coeffs   <- bt_coef(tmp.res$result, model = 1)#
			my.form1simple   <- VC ~ 1
			tmp.res <- condition_handler(gnm(formula = my.form1simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start = coeffs, trace = TRUE, iterMax = 1), file = "./stdout.log")
			res.gnm1   <- tmp.res$result
			RSS[1]     <- sum((res.gnm1$y-res.gnm1$fitted.values)^2)
			AIC[1]    <- res.gnm1$aic
			Df.resid[1] <- res.gnm1$df.residual <- sum(Data[, "DF"])-1
			Deviance[1] <- res.gnm1$deviance
			tmp.msg   <- " ... finished."
		}
		else
		{
			errors   <- c(errors, paste0("Model 1 (Errors original space): ", tmp.res$errors))
			tmp.msg <- " ... could not be fitted due to errors."
		}
		
		messages <- c(messages, paste0("Model 1 (Messages): ", tmp.res$messages))
		warnings <- c(warnings, paste0("Model 1 (Warnings): ", tmp.res$warnings))
		
		startvals  <- NULL
		cat(tmp.msg)
	}
	
	if(2 %in% model.no)
	{
		cat("\nModel 2 ")
		#mu.fun[[2]]  <- function(x) 2*log(x)
		mu       <- Mu[[2]] <- 2*log(Data[, "Mean"])
		my.form2   <- VC ~ offset(mu) # constant CV
		startvals  <- log(weighted.mean(Data[, "VC"]/Data[, "Mean"]^2, pweights)) #exact solution
		tmp.res   <- condition_handler(gnm(formula = my.form2, family = Gamma(link = "log"), data = Data, weights = pweights,
						start = startvals, trace = TRUE), file = "./stdout.log")
		if(tmp.res$status != 2)
		{
			coeffs   <- bt_coef(tmp.res$result, model = 2)#
			mu    <- Data[, "Mean"]^2
			my.form2simple   <- VC ~ powfun2simple(Mean)-1
			
			tmp.res <- condition_handler(gnm(formula = my.form2simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start = coeffs, trace = TRUE, iterMax = 0), file = "./stdout.log")
			res.gnm2   <- tmp.res$result
			RSS[2]     <- sum((res.gnm2$y-res.gnm2$fitted.values)^2)
			AIC[2]    <- res.gnm2$aic
			Df.resid[2] <- res.gnm2$df.residual  <- sum(Data[, "DF"])-1
			Deviance[2] <- res.gnm2$deviance
			tmp.msg   <- " ... finished."
		}
		else
		{
			errors   <- c(errors, paste0("Model 2 (Errors original space): ", tmp.res$errors))
			tmp.msg <- " ... could not be fitted due to errors."
		}
		
		messages <- c(messages, paste0("Model 2 (Messages): ", tmp.res$messages))
		warnings <- c(warnings, paste0("Model 2 (Warnings): ", tmp.res$warnings))
		
		startvals  <- NULL
		cat(tmp.msg)
	}
	
	# models 3 4, and 5 are covered by models and, therefore, do not need to be fitted separately if the more
	# general models optimizing the exponent are fitted per default
	
	
	if (3 %in% model.no) {
		cat("\nModel 3 ")
		mu         <- Mu[[3]] <- Data[, "Mean"]^2
		mumax      <- max(mu)
		startweights   <- pweights/startVC^2
		
		if(is.null(startvals))
		{
			st <- lm(VC ~ mu, data = Data, weights = startweights)
			startvals <- t_coef(st$coefficients, Maxi = mumax, model = 3)
		}
		else
		{
			startvals <- t_coef(startvals, Maxi = mumax, model = 3)
		}
		my.form3  <- VC~ -1 + powfun3(mu) #quadratic model
		tmp.res   <- condition_handler(gnm(formula = my.form3, family = Gamma(link = "identity"), data = Data, weights = pweights, start = startvals, trace = TRUE), file = "./stdout.log")
		
		if(tmp.res$status != 2)
		{
			coeffs       <- bt_coef(tmp.res$result, model = 3)
			AIC[3]      <- tmp.res$result$aic
			tmp.df      <- tmp.res$result$df.residual
			Deviance[3]   <- tmp.res$result$deviance
			my.form3simple   <- VC ~ powfun3simple(Mean)-1
			tmp.res     <- condition_handler(gnm(formula = my.form3simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start = coeffs, trace = TRUE, iterMax = 1000), file = "./stdout.log")
			if(tmp.res$status != 2)
			{
				res.gnm3   <- tmp.res$result
				RSS[3]     <- sum((res.gnm3$y-res.gnm3$fitted.values)^2)
				tmp.msg   <- " ... finished."
			}
			else
			{
				res.gnm3   <- list(coefficients = coeffs, aic = AIC[3], deviance = Deviance[3], df.residual = tmp.df)
				tmp.msg   <- paste(" ... estimation of covariance-matrix failed, CIs will not be available!", sep = "")
			}
		}
		else
		{
			errors   <- c(errors, paste0("Model 3 (Errors transformed space): ", tmp.res$errors))
			tmp.msg <- " ... could not be fitted due to errors."
		}
		
		messages <- c(messages, paste0("Model 3 (Messages): ", tmp.res$messages))
		warnings <- c(warnings, paste0("Model 3 (Warnings): ", tmp.res$warnings))
		
		startvals  <- NULL
		cat(tmp.msg)
	}
	
	if(4 %in% model.no)
	{
		cat("\nModel 4 ")
		mu      <- Mu[[4]] <- Data[, "Mean"]
		mumax = max(mu)
		startweights <- pweights/startVC^(2/K)
		VCrootK   <- Data[, "VC"]^(1/K)
		my.form4   <- VC~-1+powfun4(mu, K)   #fixed power model
		if (is.null(startvals))
		{
			st <- lm(VCrootK ~ mu, data = Data, weights = startweights)
			startvals <- t_coef(st$coefficients, K = K, Maxi = mumax, model = 4)
		}
		else
		{
			startvals <- t_coef(startvals, K = K, Maxi = mumax, model = 4)
		}
		
		tmp.res <- condition_handler(gnm(formula = my.form4, family = Gamma(link = "identity"), data = Data, weights = pweights,
						start = startvals, trace = TRUE), file = "./stdout.log")
		
		if(tmp.res$status != 2)
		{
			coeffs     <- bt_coef(tmp.res$result, K = K, model = 4)#
			AIC[4]     <- tmp.res$result$aic
			Deviance[4] <- tmp.res$result$deviance
			my.form4simple   <- VC~-1+powfun4simple(Mean, K) #fixed power model
			tmp.res <- condition_handler(gnm(formula = my.form4simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start = coeffs, trace = TRUE, iterMax = 0), file = "./stdout.log")
			
			if(tmp.res$status != 2)
			{
				res.gnm4  <- tmp.res$result
				RSS[4]     <- sum((res.gnm4$y-res.gnm4$fitted.values)^2)
				Df.resid[4] <- res.gnm4$df.residual  <- sum(Data[, "DF"])-2
				tmp.msg <- " ... finished."
			}
			else
			{
				Df.resid[4] <- sum(Data[, "DF"])-2
				res.gnm4   <- list(coefficients = coeffs, aic = AIC[4], deviance = Deviance[4], df.residual = Df.resid[4])
				tmp.msg   <- paste(" ... estimation of covariance-matrix failed, CIs will not be available!", sep = "")
			}
		}
		else
		{
			res.gnm4   <- NULL
			errors     <- c(errors, paste0("Model 4 (Errors transformed space): ", tmp.res$errors))
			tmp.msg   <- " ... could not be fitted due to errors."
		}
		messages <- c(messages, paste0("Model 4 (Messages): ", tmp.res$messages))
		warnings <- c(warnings, paste0("Model 4 (Warnings): ", tmp.res$warnings))
		
		startvals   <- NULL        # re-set start values
		cat(tmp.msg)
	}
	
	if(5 %in% model.no){
		cat("\nModel 5 ")
		if(skip5)
			cat(" ... skipped!")
		else
		{
			mu         <- Mu[[5]] <- Data[, "Mean"]^K
			mumax      <- max(mu)
			startweights   <- pweights/startVC^2
			
			if (is.null(startvals))
			{
				st <- lm(VC ~ mu, data = Data, weights = startweights)
				startvals  <- t_coef(st$coefficients, Maxi = mumax, model = 5)
			}
			else
			{
				startvals  <-t_coef(startvals, Maxi = mumax, Model = 5)
			}
			
			my.form5   <- VC~-1+powfun5(mu)    #const model, requires a fixed power on input
			tmp.res   <- condition_handler(gnm(formula = my.form5, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start = startvals, trace = TRUE), file = "./stdout.log")
			if(tmp.res$status != 2)
			{
				coeffs     <- bt_coef(tmp.res$result, K = K, model = 5)
				AIC[5]    <- tmp.res$result$aic
				Deviance[5]  <- tmp.res$result$deviance
				my.form5simple <- VC~powfun5simple(Mean, K)-1
				tmp.res <- condition_handler(gnm(formula = my.form5simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
								start = coeffs, trace = TRUE, iterMax = 0), file = "./stdout.log")
				
				if(tmp.res$status != 2)
				{
					res.gnm5   <- tmp.res$result
					RSS[5]     <- sum((res.gnm5$y-res.gnm5$fitted.values)^2)
					Df.resid[5] <- res.gnm5$df.residual <- sum(Data[, "DF"])-2
					tmp.msg <- " ... finished."
				}
				else
				{
					Df.resid[5] <- sum(Data[, "DF"])-2
					res.gnm5   <- list(coefficients = coeffs, aic = AIC[5], deviance = Deviance[5], df.residual = Df.resid[5])
					tmp.msg   <- paste(" ... estimation of covariance-matrix failed, CIs will not be available!", sep = "")
				}
			}
			else
			{
				res.gnm5   <- NULL
				errors     <- c(errors, paste0("Model 5 (Errors transformed space): ", tmp.res$errors))
				tmp.msg   <- " ... could not be fitted due to errors.\n"
			}
			messages <- c(messages, paste0("Model 5 (Messages): ", tmp.res$messages))
			warnings <- c(warnings, paste0("Model 5 (Warnings): ", tmp.res$warnings))
			
			startvals  <- NULL
			cat(tmp.msg)
		}
	}
	
	if(6 %in% model.no)
	{
		cat("\nModel 6 ")
		mu        <- Mu[[6]] <- Data[, "Mean"]
		my.form6     <- VC~-1+ powfun6(mu) #linear + variable power model
		results      <- vector("list", 5)
		par.ind     <- 4
		res.ind      <- 1
		res.gnm6     <- tmp <- NULL
		Parms      <- vector("list", 5)
		SVhist      <- matrix(ncol = 4, nrow = 0)
		startweights   <- pweights/startVC^2
		tmp.msg     <- " ... could not be fitted due to errors."
		
		if (is.null(startvals))         # use random start values for gnm
		{
			tempdev = 1.0e100
			res.gnm6 <- NULL
			for(ldJ in 0:4)
			{
				J       <- 2^ldJ+0.1
				muJ     <- mu^J
				st       <- lm(VC ~ mu+muJ, data = Data, weights = startweights)
				startvals  <-t_coef(c(st$coefficients, J), model = 6)
				tmp.res   <- condition_handler(gnm(formula = my.form6, family = Gamma(link = "identity"), data = Data, weights = pweights,
								start = startvals, trace = TRUE), file = "./stdout.log")
				
				if(tmp.res$status != 2)
				{
					if(!is.null(tmp.res$result) && tmp.res$result$deviance<tempdev)
					{
						res.gnm6 <- tmp.res$result
						tempdev <- res.gnm6$deviance
						tmp.msg <- " ... finished."
					}
				}
				else
					errors <- c(errors, paste0("Model 6 (Errors transformed space): ", tmp.res$errors))
			}
		}
		else                  # user-specified start values
		{
			startvals <- t_coef(startvals, model = 6)
			tmp.res <- condition_handler(gnm(formula = my.form6, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start = startvals, trace = TRUE), file = "./stdout.log")
			if(tmp.res$status != 2)
			{
				res.gnm6 <- tmp.res$result
				tmp.msg <- " ... finished.\n"
			}
			else
			{
				errors   <- c(errors, paste0("Model 6 (Errors transformed space): ", tmp.res$errors))
				tmp.msg <- " ... could not be fitted due to errors."
			}
		}
		
		if(!is.null(res.gnm6))
		{
			coeffs     <- bt_coef(res.gnm6, model = 6)#
			AIC[6]     <- res.gnm6$aic
			Deviance[6]  <- res.gnm6$deviance
			my.form6simple   <- VC~-1+powfun6simple(Mean)
			tmp.res     <- condition_handler(gnm(formula = my.form6simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start = coeffs, trace = TRUE, iterMax = 0), file = "./stdout.log")
			
			if(tmp.res$status != 2)
			{
				res.gnm6     <- tmp.res$result
				RSS[6]       <- sum((res.gnm6$y-res.gnm6$fitted.values)^2)
				Df.resid[6]    <- res.gnm6$df.residual <- sum(Data[, "DF"])-4
			}
			else
			{
				Df.resid[6] <- sum(Data[, "DF"])-4
				res.gnm6   <- list(coefficients = coeffs, aic = AIC[6], deviance = Deviance[6], df.residual = Df.resid[6])
				tmp.msg   <- paste(" ... estimation of covariance-matrix failed, CIs will not be available!", sep = "")
			}
		}
		
		messages <- c(messages, paste0("Model 6 (Messages): ", tmp.res$messages))
		warnings <- c(warnings, paste0("Model 6 (Warnings): ", tmp.res$warnings))
		
		startvals   <- NULL
		cat(tmp.msg)
	}
	
	if(7 %in% model.no)
	{
		cat("\nModel 7 ")
		mu        <- Mu[[7]] <- Data[,"Mean"]
		startweights   <- pweights/startVC^2
		tmp.msg     <- " ... could not be fitted due to errors."
		
		if (is.null(startvals) || startvals[3]<0 ) #negative power leads negative sigma^2 for small means
		{
			tempdev    <- 1.0e100
			res.gnm7   <- NULL
			for(ldJ in -3:3){
				J       <- 2^ldJ
				muJ      <- mu^J
				st       <- lm(VC ~ muJ, data = Data, weights = startweights)
				startvals  <-t_coef(c(st$coefficients, J), Maxi = max(muJ), model = 7)
				my.form7   <- VC ~ -1+powfun7(mu)
				tmp.res   <- condition_handler(gnm(formula = my.form7, family = Gamma(link = "identity"), data = Data,
								weights = pweights, start = startvals, trace = TRUE), file = "./stdout.log")
				if(tmp.res$status != 2)
				{
					if(!is.null(tmp.res$result) && tmp.res$result$deviance<tempdev)
					{
						res.gnm7 <- tmp.res$result
						tempdev <- res.gnm7$deviance
						tmp.msg <- " ... finished."
					}
				}
				else
					errors <- c(errors, paste0("Model 7 (Errors transformed space): ", tmp.res$errors))
			}
		}
		else
		{
			my.form7   <- VC ~ -1+powfun7(mu)
			startvals[3]      <- max(0.1, min(10, startvals[3]))#restrict starting values to 0.1<startvals[3] <10
			muJmax<-max(mu^startvals[3])
			startvals <- t_coef(startvals, Maxi = muJmax, model = 7)
			tmp.res <- condition_handler(gnm(formula = my.form7, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start = startvals, trace = TRUE), file = "./stdout.log")
			if(tmp.res$status != 2)
			{
				res.gnm7 <- tmp.res$result
				tmp.msg <- " ... finished."
			}
			else
			{
				errors   <- c(errors, paste0("Model 7 (Errors transformed space): ", tmp.res$errors))
				tmp.msg  <- " ... could not be fitted due to errors."
			}
		}
		if(!is.null(res.gnm7))
		{
			coeffs       <- bt_coef(res.gnm7, model = 7)#
			AIC[7]       <- res.gnm7$aic
			Deviance[7]   <- res.gnm7$deviance
			my.form7simple   <- VC~-1+powfun7simple(Mean)
			tmp.res     <- condition_handler(gnm(formula = my.form7simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start = coeffs, trace = TRUE, iterMax = 0), file = "./stdout.log")
			
			if(!is.null(tmp.res))
			{
				res.gnm7   <- tmp.res$result
				RSS[7]     <- sum((res.gnm7$y-res.gnm7$fitted.values)^2)
				Df.resid[7] <- res.gnm7$df.residual <- sum(Data[, "DF"])-3
			}
			else
			{
				Df.resid[7] <- sum(Data[, "DF"])-3
				res.gnm7   <- list(coefficients = coeffs, aic = AIC[7], deviance = Deviance[7], df.residual = Df.resid[7])
				tmp.msg   <- paste(" ... estimation of covariance-matrix failed, CIs will not be available!", sep = "")
			}
		}
		messages <- c(messages, paste0("Model 7 (Messages): ", tmp.res$messages))
		warnings <- c(warnings, paste0("Model 7 (Warnings): ", tmp.res$warnings))
		
		startvals   <- NULL
		cat(tmp.msg)
	}
	
	if(8 %in% model.no)
	{
		cat("\nModel 8 ")
		mu      <- Mu[[8]] <- Data[, "Mean"]
		C      <-   attr(Mu[[8]], "Constant") <- max(mu)
		my.form8   <- VC~-1+powfun8(mu, C)
		devns     <- rep(NA, 1e3)
		tmp.msg    <- " ... could not be fitted due to errors."
		
		if (is.null(startvals))     # random start values
		{
			tempdev    <- 1.0e100
			res.gnm8   <- NULL
			for(ldJ in -3:3){
				J         <- 2^ldJ
				signJ      <- 1
				my.form8     <- VC~-1+powfun8(mu, C, signJ)
				startweights   <- pweights/startVC^(2/J)
				VCrootJ     <- Data[, "VC"]^(1/J)
				st         <- lm(VCrootJ ~ mu, data = Data, weights = startweights)
				startvals    <- t_coef(c(st$coefficients, signJ*J), Maxi = C, signJ = signJ, model = 8)
				tmp.res     <- condition_handler(gnm(formula = my.form8, family = Gamma(link = "identity"),
								data = Data, weights = pweights ,start = startvals,
								trace = TRUE), file = "./stdout.log")
				if(tmp.res$status != 2)
				{
					if(!is.null(tmp.res$result))
					{
						if(tmp.res$result$deviance<tempdev)
						{
							res.gnm8 <- tmp.res$result
							sgnJ <- signJ
							tempdev <- res.gnm8$deviance
							tmp.msg <- " ... finished."
						}
					}
				}
				else
					errors <- c(errors, paste0("Model 8 (Errors transformed space): ", tmp.res$errors))
			}
			for(ldJ in -3:3)
			{
				J         <- -2^ldJ
				signJ      <- -1
				my.form8   <- VC~-1+powfun8(mu, C, signJ)
				startweights   <- pweights/startVC^(2/J)
				VCrootJ     <- Data[, "VC"]^(1/J)
				st         <- lm(VCrootJ ~ mu, data = Data, weights = startweights)
				startvals    <- t_coef(c(st$coefficients, signJ*J), Maxi = C, signJ = signJ, model = 8)
				tmp.res     <- condition_handler(gnm(formula = my.form8, family = Gamma(link = "identity"),
								data = Data, weights = pweights, start = startvals,
								trace = TRUE), file = "./stdout.log")
				if(tmp.res$status != 2)
				{
					if(!is.null(tmp.res$result))
					{
						if(tmp.res$result$deviance<tempdev)
						{
							res.gnm8 <- tmp.res$result
							sgnJ <- signJ
							tempdev <- res.gnm8$deviance
							tmp.msg <- " ... finished."
						}
					}
				}
				else
					errors <- c(errors, paste0("Model 8 (Errors transformed space): ", tmp.res$errors))
			}
		}
		else              # user-specified startvalues
		{
			if(startvals[3]>0){ #restrict range of starting values to 0.1 <|J| <10
				startvals[3]      <- max(0.1, min(10, startvals[3]))
				signJ          <- 1
			}
			else{
				signJ          <- -1
				startvals[3]      <- -max(0.1, min(10, -startvals[3]))
			}
			my.form  <- VC~-1+powfun8(Mean, C, signJ)
			startvals <-t_coef(startvals, Maxi = C, signJ = signJ, model = 8)
			tmp.res <- condition_handler(gnm(formula = my.form8, family = Gamma(link = "identity"),
							data = Data, weights = pweights ,start = startvals,
							trace = TRUE), file = "./stdout.log")
			
			if(tmp.res$status != 2)
			{
				if(!is.null(tmp.res$result))
				{
					res.gnm8   <- tmp.res$result
					sgnJ     <- signJ
					tmp.msg   <- " ... finished."
				}
			}
			else
			{
				errors   <- c(errors, paste0("Model 8 (Errors transformed space): ", tmp.res$errors))
				tmp.msg  <- " ... could not be fitted due to errors."
			}
		}
		if(!is.null(res.gnm8))
		{
			coeffs       <- bt_coef(res.gnm8, signJ = sgnJ, model = 8)
			AIC[8]      <- res.gnm8$aic
			Deviance[8]   <- res.gnm8$deviance
			my.form8simple   <- VC~-1+powfun8simple(Mean)
			tmp.res <- condition_handler(gnm(formula = my.form8simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start = coeffs, trace = TRUE, iterMax = 0), file = "./stdout.log")
			
			if(!is.null(tmp.res))
			{
				
				res.gnm8  <- tmp.res$result
				RSS[8]     <- sum((res.gnm8$y-res.gnm8$fitted.values)^2)
				Df.resid[8] <- res.gnm8$df.residual <- sum(Data[, "DF"])-3
				
			}
			else
			{
				Df.resid[8] <- sum(Data[, "DF"])-3
				res.gnm8   <- list(coefficients = coeffs, aic = AIC[8], deviance = Deviance[8], df.residual = Df.resid[8])
				tmp.msg   <- paste(" ... estimation of covariance-matrix failed, CIs will not be available!", sep = "")
			}
		}
		messages <- c(messages, paste0("Model 8 (Messages): ", tmp.res$messages))
		warnings <- c(warnings, paste0("Model 8 (Warnings): ", tmp.res$warnings))
		
		startvals     <- NULL
		cat(tmp.msg)
	}
	
	if(9 %in% model.no)
	{
		cat("\nModel 9 ")
		mu      <- Mu[[9]] <- log(Data[, "Mean"])
		logVC     <- log(Data[, "VC"])
		my.form9   <- formula(paste("VC", "~ mu", sep = " "))   # variable power model without intercept
		if (is.null(startvals))                 # random start values
		{
			startweights   <- pweights
			st         <- lm(logVC ~ mu, data = Data, weights = startweights)
			startvals    <- c(st$coefficients[1], st$coefficients[2])
		}
		else
		{
			startvals <-t_coef(startvals, model = 9)
		}
		
		tmp.res <- condition_handler(gnm(formula = my.form9, family = Gamma(link = "log"), data = Data, weights = pweights,
						start = startvals, trace = TRUE), file = "./stdout.log")
		
		if(tmp.res$status != 2)
		{
			res.gnm9 <- tmp.res$result
			tmp.msg <- " ... finished."
		}
		else
		{
			errors   <- c(errors, paste0("Model 9 (Errors transformed space): ", tmp.res$errors))
			tmp.msg  <- " ... could not be fitted due to errors."
		}
		
		if(!is.null(res.gnm9))
		{
			coeffs       <- bt_coef(res.gnm9, model = 9)#
			AIC[9]      <- res.gnm9$aic
			Deviance[9]   <- res.gnm9$deviance
			my.form9simple   <- VC~-1+powfun9simple(Mean)
			tmp.res     <- condition_handler(gnm(formula = my.form9simple, family = Gamma(link = "identity"), data = Data, weights = pweights,
							start = coeffs, trace = TRUE, iterMax = 0), file = "./stdout.log")
			
			if(!is.null(tmp.res ))
			{
				res.gnm9    <- tmp.res$result
				RSS[9]      <- sum((res.gnm9$y-res.gnm9$fitted.values)^2)
				Df.resid[9]   <- res.gnm9$df.residual <- sum(Data[, "DF"])-2
			}
			else
			{
				Df.resid[9] <- sum(Data[, "DF"])-2
				res.gnm9   <- list(coefficients = coeffs, aic = AIC[9], deviance = Deviance[9], df.residual = Df.resid[9])
				tmp.msg   <- paste(" ... estimation of covariance-matrix failed, CIs will not be available!", sep = "")
			}
		}
		messages <- c(messages, paste0("Model 9 (Messages): ", tmp.res$messages))
		warnings <- c(warnings, paste0("Model 9 (Warnings): ", tmp.res$warnings))
		
		startvals <- NULL
		cat(tmp.msg)
	}
	
	if(10 %in% model.no)
	{
		cat("\nModel 10")
		
		if(!is.null(col.cv))    # minimize error propagation effects by using CV-values directly
		{
			res.mod10   <- fit_ep17(x = Data$Mean, y = Data$CV, DF = Data$DF, typeY = "cv")
		}
		else
		{
			if(!is.null(col.sd))  # second best option
			{
				Data$CV    <- 100*Data$SD/Data$Mean
				res.mod10   <- fit_ep17(x = Data$Mean, y = Data$SD, DF = Data$DF, typeY = "sd")
			}
			else          # worst option, since VC --> SD --> CV
			{
				Data$CV    <- 100*sqrt(Data$VC)/Data$Mean
				res.mod10   <- fit_ep17(x = Data$Mean, y = Data$VC, DF = Data$DF, typeY = "vc")
			}
		}
		
		RSS[10]      <- res.mod10$RSS
		AIC[10]      <- res.mod10$AIC
		Df.resid[10]  <- sum(Data[, "DF"])-2
		Deviance[10]  <- res.mod10$deviance
		cat(" ... finished.")
	}
	
	cat("\n")
	
	if(quiet)            # read logs and remove temporary files
	{
		#try(close(outputLog), silent = TRUE)
		output   <- scan("./stdout.log", sep = "\n", what = "character", quiet = TRUE)
		try(file.remove("./stdout.log"), silent = TRUE)
	}
	else
		output <- NULL
	
	mod <- list(
			model1 = res.gnm1,
			model2 = res.gnm2,
			model3 = res.gnm3,
			model4 = res.gnm4,
			model5 = res.gnm5,
			model6 = res.gnm6,
			model7 = res.gnm7,
			model8 = res.gnm8,
			model9 = res.gnm9,
			model10= res.mod10)
	
	ind <- which(!sapply(mod, is.null))
	res <- list(
			Call = Call,
			Models = mod[ind],
			RSS = RSS[ind],
			AIC = AIC[ind],
			GoF.pval = sapply(mod[ind], function(x) pchisq(x$deviance, x$df.residual, lower.tail = FALSE)),
			DF.resid = Df.resid[ind],
			Deviance = Deviance[ind],
			Formulas = Formulas[ind],
			Data = Data,
			K = K,
			startvals = startvals,
			errors = errors,
			output = output,
			warnings = warnings,
			notes = notes)
	
	class(res) <- "VFP"
	
	return(res)
}
