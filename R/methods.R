# TODO: Add comment
#
# Author: schueta6
###############################################################################






#' Summary Objects of Class 'VFP'
#'
#' @param object		(object) of class 'VFP'
#' @param model.no		(integer) specifying a fitted model in 'x', if NULL the
#'                      best fitting
#' 						model will be printed, i.e. the one with min(RSS)
#' @param digits		(integer) number of significant digits
#' @param type			(character) "simple" = short summary, "complex" = calls
#'                      the summary-method
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
#' mat.CA19_9 <- get_mat(fits.CA19_9, "error")
#' res.CA19_9 <- fit_vfp(mat.CA19_9, 1:9)
#' summary(res.CA19_9)
#' print(res.CA19_9)
#' }

summary.VFP <- function(object, model.no = NULL, digits = 4,
		type = c("simple", "complex"), ...) {
	if (length(object$Models) == 0) {
		warning("There is not valid VFP-model that can be summarized!")
		return(NA)
	}
	AIC 	<- object$AIC
	num 	<- which(AIC == min(AIC))[1]			# index
	AIC 	<- NULL
	type 	<- match.arg(type[1], choices = c("simple", "complex"))
	
	if (!is.null(model.no)) {
		models <- sub("Model_", "", names(object$RSS))
		if (!model.no %in% models) {
			stop("Specified model ", paste0("'", model.no, "'"),
					" is not among fitted models: ", 
					paste(models, collapse = ", "),"!")
		} else {
			num <- which(models == model.no)		
		}
		model 	<- names(object$RSS[num])			# name
		Nmodel	<- 1
	} else {
		Nmodel		<- length(object$Models)		
		model 		<- names(object$RSS[num])			# name
		model.no	<- as.numeric(sub("Model_", "", model))
	}	
	
	if (model.no == 10)
		type <- "simple"
	
	form 	<- object$Formulas[num]
	fun	<- function(x) {
		gsub("==", "=", gsub("\\}", "", gsub("\\{", "", gsub("\\]", "", 
										gsub("\\[", "", gsub("[\n ]", "", x))))))
	}
	form 	<-  fun(form)
	
	cat("\n\n +++ Summary Fitted VFP-Model(s) +++ ")
	cat(  "\n-------------------------------------\n\n")
	cat("Call:\n")
	print(object$Call)
	cat("\n")
	
	if (Nmodel > 1) {
		cat("\t[+++", Nmodel, "Models Fitted +++]\n")
		for (i in 1:Nmodel)
			cat(paste0("\n", names(object$RSS[i]),": "), fun(object$Formulas[i]))
		cat("\n\n\t[+++ RSS (unweighted) +++]\n\n")
		print(signif2(object$RSS, digits = digits), quote = FALSE)
		cat("\n\t[+++ AIC +++]\n\n")
		print(signif2(object$AIC, digits = digits), quote = FALSE)
		cat("\n\t[+++ Deviance +++]\n\n")
		print(signif2(object$Deviance, digits = digits), quote = FALSE)
		cat("\n\t[+++ Best Model +++]\n\n")
	}
	cat(paste0("Model ", sub("Model_", "", model),":"),"\t")
	cat(form)
	cat("\n\nCoefficients:\n")
	
	if (type == "complex") {
		Smry <- summary(object$Models[[num]])
		
		printCoefmat(Smry$coefficients, digits = digits, 
				     signif.stars = getOption("show.signif.stars"), 
				     signif.legend = TRUE, na.print = "NA", ...)
		
		cat("\nDeviance Residuals:\n")
		print(summary(Smry$deviance.resid))
	} else {
		print(signif2(coef(object, model.no), digits), quote = FALSE)
	}
	cat("\nAIC =", 		 signif2(object$AIC[num], digits), 
			" RSS =", 		 signif2(object$RSS[num], digits), 
			" Deviance =",   signif2(object$Deviance[num], digits), 
			" GoF P-value=", signif2(object$GoF.pval[num], digits), "\n\n")
	
	if (!is.null(object$note) && Nmodel > 1) {
		cat("\t[+++ Note +++]\n\n")
		cat(object$note, sep = "\n")
		cat("\n\n")
	}
}

#' Print Objects of Class 'VFP'
#' 
#' @param x             (object) of class 'VFP'
#' @param model.no      (integer) specifying a fitted model in 'x', if NULL 
#'                      the best fitting model will be printed, i.e. the one 
#'                      with min(AIC)
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
#' mat.CA19_9 <- get_mat(fits.CA19_9, "error")
#' res.CA19_9 <- fit_vfp(mat.CA19_9, 1:9)
#' res.CA19_9
#' }

print.VFP <- function(x, model.no = NULL, digits = 4, ...) {
	
	if (length(x$AIC) == 0) {
		warning("There is no fitted VFP-model to print!")
		return(NA)
	}
	
	if (is.null(model.no)) {				# automatically determine best fitting model
		AIC 	<- x$AIC
		nam 	<- names(AIC)
		if("Model_10" %in% nam)
			AIC <- AIC[-which(nam == "Model_10")]
		num 	<- which(AIC== min(AIC))[1]
		AIC 	<- NULL
		num0	<- TRUE
	} else {
		num0 	<- FALSE
		models 	<- sub("Model_", "", names(x$RSS))
		if(!model.no %in% models)
			stop("Specified model ", paste0("'", model.no, "'")," is not among 
							fitted models: ", paste(models, collapse = ", "),"!")
		else
			num <- which(models == model.no)
	}
	model 		<- names(x$RSS[num])
	model.no	<- as.numeric(sub("Model_", "", model))
	form 		<- x$Formulas[num]
	form 		<-  gsub("==", "=", gsub("\\}", "", gsub("\\{", "", gsub("\\]", 
									"", gsub("\\[", "", gsub("[\n ]", "", form))))))
	cat("\n\n(VFP) Variance-Function")
	cat(  "\n-----------------------\n\n")
	cat(paste0("Model ", sub("Model_", "", model), 
					ifelse(length(x$Models)>1 && num0, "*:", ":")),"\t")
	cat(form)
	cat("\n\nCoefficients:\n")
	print(signif2(coef(x, model.no), digits), quote = FALSE)
	cat("\nAIC =", signif2(x$AIC[num], digits), " RSS =", 
			signif2(x$RSS[num], digits), " Deviance =", 
			signif2(x$Deviance[num], digits),"GoF P-value=",
			signif2(x$GoF.pval[num], digits),"\n\n")
	if (length(x$Models) > 1 && num0)
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
#' mat <- get_mat(lst)		# automatically selects "total"
#' res <- fit_vfp(model.no=1:9, Data=mat)
#' coef(res)
#' }

coef.VFP <- function(object, model.no = NULL, ...) {
	if (length(object$AIC) == 0) {
		warning("There is no fitted VFP-model to extract coefficients from!")
		return(NA)
	}
	
	if (is.null(model.no)) {			# automatically determine best fitting model
		AIC <- object$AIC
		nam <- names(AIC)
		if("Model_10" %in% nam)
			AIC <- AIC[-which(nam == "Model_10")]
		num <- which(AIC == min(AIC))[1]
		AIC <- NULL
		message(paste0("No 'model.no' specied! Best-fitting model assumed ('",names(object$RSS)[num],"')!"))
	} else {
		models <- sub("Model_", "", names(object$RSS))
		if (!model.no %in% models)
			stop("Specified model ", paste0("'", model.no, "'")," is not among fitted models: ", paste(models, collapse = ", "),"!")
		else
			num <- which(models == model.no)
	}
	
	model <- names(object$RSS[num])
	
	if (model == "Model_10") {
		coeffs 			<- object$Models[[num]]$coefficients
		coeffs[1] 		<- exp(coeffs[1]) 
		names(coeffs) 	<- c("beta_1", "pot") 
	} else {
		coeffs 	<- coef(object$Models[[num]])
	}
	pot		<- which(names(coeffs) == "pot")
	if (length(pot) > 0)
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

bt_coef <- function(object, K = NULL, signJ = NULL, model = NULL, ...) {
	stopifnot(model %in% 1:10)
	coeffs0 <- coef(object)
	cutval<-0.95 				# should be between 0 and 1 
	
	# back transformation to original scale
	
	if (model %in% c(1, 2)) {				# models 1 and 2
		coeffs <- c(exp(coeffs0))
		names(coeffs) <- c("beta")
	} else if(model == 3) {
		coef1	<- exp(coeffs0[1])
		coef2	<- coef1*(exp(coeffs0[2]) - cutval) / max(object$data[,"Mean"]^2)
		coeffs  <-c(coef1, coef2)
		names(coeffs) <- c("beta1", "beta2")
	} else if(model == 4) {
		coef1 	<- exp(coeffs0[1] / K)
		coef2 	<- coef1 * (exp(coeffs0[2]) - cutval) / max(object$data[, "Mean"])
		coeffs 	<- c(coef1, coef2)
		names(coeffs) <- c("beta1", "beta2")
	} else if(model == 5) {
		coef1	<- exp(coeffs0[1])
		coef2	<- coef1 * (exp(coeffs0[2]) - cutval) / max(object$data[, "Mean"]^K)
		coeffs  <-c(coef1, coef2)
		names(coeffs) <- c("beta1", "beta2")
	} else if(model == 6) {
		coef1 	<- exp(coeffs0[1])
		coef2 	<- exp(coeffs0[1] + coeffs0[3]) * (1 + exp( -coeffs0[4]))*(atan(coeffs0[2]) / pi - .5)
		coef3	<- -exp(coeffs0[1] + coeffs0[3] - coeffs0[4]) * (atan(coeffs0[2]) / pi - .5) * exp(coeffs0[3] * exp(coeffs0[4]))
		coef4	<- 1 + exp(coeffs0[4])
		coeffs 	<- c(coef1, coef2, coef3, coef4)
		names(coeffs) <- c("beta1", "beta2", "beta3", "J")
	} else if(model == 7) {		
		coef1	<- exp(coeffs0[1])
		coef3	<-((0.1 + 10*exp(coeffs0[3])) / (1+exp(coeffs0[3])))
		coef2	<- coef1 * (exp(coeffs0[2]) - cutval) / max(object$data[,"Mean"]^coef3)
		coeffs	<- c(coef1, coef2, coef3)
		names(coeffs) <- c("beta1", "beta2", "J")
	} else if(model == 8) {
		coef3	<-signJ*((0.1+10*exp(coeffs0[3]))/(1+exp(coeffs0[3])))
		coef1 	<- exp(coeffs0[1]/coef3)
		coef2	<- coef1*(exp(coeffs0[2])-cutval)/max(object$data[,"Mean"])
		coeffs	<- c(coef1, coef2, coef3)
		names(coeffs) <- c("beta1", "beta2", "J")
	} else if(model == 9) {
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

t_coef <- function(coeffs0, K = NULL, Maxi = NULL, model = NULL, signJ = NULL, 
		eps = sqrt(.Machine$double.eps), ...) {
	stopifnot(model %in% 1:10)
	cutval<-0.95 				# should be between 0 and 1 
	
	# back transformation to original scale
	
	if(model %in% c(1,2)) {				# models 1 and 2
		coeffs <- c(log(pmax(eps,coeffs0)))
		names(coeffs) <- c("gamma1")
	} else if(model == 3)	{
		coef1 <- log(max(eps,coeffs0[1]))
		coef2 <- log(max(eps,Maxi*coeffs0[2]/max(eps,coeffs0[1])+cutval))
		coeffs<- c(coef1,coef2)
		names(coeffs) <- c("gamma1","gamma2")
	} else if(model == 4) {
		coef1 	<- K*log(max(eps,coeffs0[1]))
		coef2   <- log(max(eps,Maxi*coeffs0[2]/max(eps,coeffs0[1])+cutval))
		coeffs 	<- c(coef1, coef2)
		names(coeffs) <- c("gamma1", "gamma2")
	} else if(model == 5)	{
		coef1 <- log(max(eps,coeffs0[1]))
		coef2 <- log(max(eps,Maxi*coeffs0[2]/max(eps,coeffs0[1])+cutval))
		coeffs<- c(coef1,coef2)
		names(coeffs) <- c("gamma1","gamma2")
	} else if(model == 6) {				
		coef1 	<- log(max(eps, coeffs0[1]))
		coef3   <- log(max(eps,(-coeffs0[3]/coeffs0[2]*coeffs0[4])))/(coeffs0[4]-1)
		coef2   <- tan((coeffs0[2]/exp(-coef3)*(coeffs0[4]-1)/coeffs0[4]+0.5)*pi)
		coef4   <- log(max(eps, coeffs0[4]-1))
		coeffs 	<- c(coef1, coef2, coef3, coef4)
		names(coeffs) <- c("gamma1", "gamma2", "gamma3", "gamma4")
	} else if(model == 7) {
		coef1 <- log(max(eps,coeffs0[1]))
		coef2 <- log(max(eps,Maxi*coeffs0[2]/max(eps,coeffs0[1])+cutval))
		coef3 <- log((max(0.1,coeffs0[3])-0.1)/(10-pmin(9.999,coeffs0[3])))
		coeffs  <- c(coef1,coef2,coef3)
		names(coeffs) <- c("gamma1", "gamma2", "gamma3")
	} else if(model == 8) {
		coef1 	<- log(max(eps,coeffs0[1]))*signJ*coeffs0[3]
		coef2	<- log(max(eps,coeffs0[2]/max(eps,coeffs0[1])*Maxi+cutval))
		coef3   <- log((max(0.1,coeffs0[3])-0.1)/(10-pmin(9.999,coeffs0[3])))
		coeffs	<- c(coef1, coef2, coef3)
		names(coeffs) <- c("gamma1", "gamma2", "gamma3")
	} else if(model == 9) {
		coeffs  <- c(log(max(eps,coeffs0[1])),coeffs0[2])
		names(coeffs) <- c("gamma1", "gamma2")
	}
	
	coeffs
}



#' Predict Method for Objects of Class 'VFP'.
#' 
#' Predictions are made for the variance (type="vc"), standard deviation ("sd") or 
#' coefficient of variation ("cv") and their corresponding confidence intervals.
#' The latter are calculated primarily on the variance scale and then transformed to the
#' other scales, if required. 
#' 
#' @param object		(object) of class "VFP"
#' @param model.no		(integer) specifying a fitted model stored in 'object'
#' @param newdata		(numeric) optionally, a vector specifying mean-values for which predictions
#' 						on the user-defined scale ('type') are requested. If omitted, fitted values 
#' 						will be returned.
#' @param alpha			(numeric) value specifying the 100 x (1-alpha)\% confidence interval of predicted
#' 						values
#' @param dispersion	(numeric) NULL = the dispersion will be set =1 (should usually not be changed; 
#'						For the Saddler model, the dispersion is 1.),
#' 						numeric value = the dispersion parameter will be used as specified
#' @param type			(character) specifying on which scale the predicted values shall be returned, 
#' 						possible are "vc" = variance, "sd"=standard deviation, "cv"=coefficient of variation
#' @param CI.method		(character) one of "t", "normal", "chisq" specifying which CI-method to use
#' @param use.log		(logical) TRUE = X- and Y-axis will be log-transformed
#' @param ...			additional parameters passed forward to function \code{\link[gnm]{predict.gnm}}
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
#' mat <- get_mat(lst)		# automatically selects "total"
#' res <- fit_vfp(model.no=1:9, Data=mat)
#' predict(res)
#' predict(res, dispersion=0.95)
#' }

predict.VFP <- function(object, model.no = NULL, newdata = NULL, alpha = .05,
		dispersion = NULL, type = c("vc", "sd", "cv"), 
		CI.method = c("chisq", "t", "normal"), use.log = FALSE, ...) {
	call 		<- match.call()
	predOnly 	<- call$predOnly
	if (is.null(predOnly))
		predOnly <- FALSE
	
	stopifnot(is.null(dispersion) || (is.numeric(dispersion) && dispersion > 0))
	
	type 		<- match.arg(type[1], choices=c("vc", "sd", "cv"))
	CI.method	<- match.arg(CI.method[1], choices=c("t", "normal", "chisq"))
	
	if (is.null(model.no)) {				# automatically determine best fitting model
		AIC <- object$AIC
		nam <- names(AIC)
		if("Model_10" %in% nam)
			AIC <- AIC[-which(nam == "Model_10")]
		num <- which(AIC == min(AIC, na.rm=TRUE))[1]
		AIC <- NULL
	} else {
		models <- sub("Model_", "", names(object$RSS))
		if(!model.no %in% models)
			stop("Specified model ", paste0("'", model.no, "'"),
					" is not among fitted models: ", 
					paste(models, collapse=", "),"!")
		else
			num <- which(models == model.no)
	}
	
	if (is.null(dispersion)) {
		dispersion <- 1
	}
	
	model <- names(object$RSS[num])
	if (is.null(model.no))
		model.no <- sub("Model_", "", model)
	
	if (is.null(newdata)) {
		Mean 	<- object$Data$Mean
		newdata	<- data.frame(Mean=Mean)
	} else {
		Mean 	<- newdata
		newdata	<- data.frame(Mean=newdata)	
		if (use.log)
			newdata$Mean <- exp(newdata$Mean)
	}
	
	suppressWarnings(
			pred <- try(predict(object$Models[[num]], newdata = newdata,
							dispersion = dispersion, type = "response", 
							se = TRUE, ci.type = type, CI.method = CI.method,
							use.log = use.log, ...),
					silent = TRUE)
	)
	
	if(is(pred, "try-error")) {
		
		#	pred <- newdata
		parms <- object$Models[[num]]$coefficients
		fun1 <- function(x, parms) return(rep(parms, length(x)))
		fun2 <- function(x, parms) return( parms * x^2)
		fun3 <- function(x, parms) return( parms[1] + parms[2] * x^2)
		fun4 <- function(x, parms) return((parms[1] + parms[2] * x)^2)
		fun5 <- function(x, parms) return((parms[1] + parms[2] * x^parms[3]))
		fun6 <- function(x, parms) return( parms[1] + parms[2] * x + parms[3] * x^parms[4])
		fun7 <- function(x, parms) return( parms[1] + parms[2] * x^parms[3])
		fun8 <- function(x, parms) return((parms[1] + parms[2] * x)^parms[3])
		fun9 <- function(x, parms) return( parms[1] * x^parms[2])
		
		pred 	<- do.call(paste0("fun", model.no),  args=list(x=newdata$Mean, parms=parms)) 
		pred    <- data.frame(fit=as.numeric(pred), se.fit=NA, residual.scale=dispersion)
	}
	
	if (model != "Model_10") {
		# transform to log-scale for t- and normal-dist
		if (CI.method %in% c("t", "normal")) {
			pred$se.fit <- pred$se.fit / pred$fit
			pred$fit	<- log(pred$fit)
		}
		
		
		if (predOnly)
			return(pred$fit)
		
		pred <- as.data.frame(pred)
		pred <- cbind(newdata, pred)
		
		if (CI.method == "normal") { 						# calculate CI's on log scale to assure that they are positive on linear scale
			Qnorm 		<- qnorm(1-alpha/2)
			CIupper 	<- pred$fit + Qnorm * pred$se.fit
			CIlower 	<- pred$fit - Qnorm * pred$se.fit
		} else if (CI.method == "t") { 						# calculate CI's on log scale to assure that they are positive on linear scale
			Qtdist 		<- qt(1 - alpha/2, df.residual(object$Models[[num]]))
			CIupper 	<- pred$fit+Qtdist*pred$se.fit
			CIlower		<- pred$fit-Qtdist*pred$se.fit
		} else {											# chisq; This is calculated on linear scale, as chisquare distribution is always positive
			df.qchisq		<- 2*(pred$fit/pred$se.fit)^2 	#calculate relevant degrees of freedom from the known relation between the point estimator and it's variance for a chi square distributed variable
			lower.qchisq 	<- qchisq(alpha/2, df=df.qchisq)
			upper.qchisq 	<- qchisq(1-alpha/2, df=df.qchisq)
			CIlower			<- pred$fit*lower.qchisq/df.qchisq
			CIupper			<- pred$fit*upper.qchisq/df.qchisq
		}
		
		if (CI.method %in% c("t", "normal") && !use.log) {
			pred$fit 	<- exp(pred$fit)
			CIlower		<- exp(CIlower)
			CIupper		<- exp(CIupper)
		}
		
		if (CI.method == "chisq" && use.log) {
			pred$fit 	<- log(pred$fit)
			CIlower		<- log(CIlower)
			CIupper		<- log(CIupper)
		}
		
		if (!use.log) {	
			if (type %in% c("sd", "cv")) {
				pred$fit 	<- sqrt(pred$fit)
				CIlower		<- sqrt(CIlower )
				CIupper		<- sqrt(CIupper )
				
				if (type == "cv") {
					pred$fit 	<-  100 * pred$fit / Mean
					CIlower		<-  100 * CIlower  / Mean	
					CIupper		<-  100 * CIupper  / Mean	
				}
			}	
		} else {
			if (type %in% c("sd", "cv")) {
				pred$fit 	<- pred$fit/2
				CIlower		<- CIlower/2 
				CIupper		<- CIupper/2
				
				if (type == "cv") {
					pred$fit 	<-  log(100) + pred$fit - log(Mean)
					CIlower		<-  log(100) + CIlower  - log(Mean)	
					CIupper		<-  log(100) + CIupper  - log(Mean)	
				}
			}	
		}
		
		
		if (use.log)
			pred$Mean <- log(pred$Mean)
		pred$CIlower 	<- CIlower
		pred$CIupper 	<- CIupper
		attr(pred, "conf.level") <- 1-alpha 
		
		colnames(pred)	<- c("Mean", "Fitted", "SE", "Scale", "LCL", "UCL")
	}
	
	if (any(pred$Fitted %in% c(NA, NaN)))
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
#' @seealso \code{\link{fit_vfp}}, \code{\link{predict.VFP}}, \code{\link{plot.VFP}}
#' 
#' @aliases predictMean
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
#' mat.CA19_9 <- get_mat(fits.CA19_9, "error")
#' res.CA19_9 <- fit_vfp(mat.CA19_9, 1:9)
#' summary(res.CA19_9)
#' print(res.CA19_9)
#' 
#' # predict CA19_9-concentration with 5\% CV
#' predict_mean(res.CA19_9, newdata=5) 
#' 
#' # this is used in function plot.VFP as well
#' plot(res.CA19_9, Prediction=list(y=5), type="cv")
#' plot(res.CA19_9, Prediction=list(y=5), type="cv", 
#' 		xlim=c(0, 80), ylim=c(0, 10))
#' }

predict_mean <- function(obj, type = c("vc", "sd", "cv"), model.no = NULL, 
		alpha = .05, newdata = NULL, tol = 1e-4, ci = TRUE, ...) {
	call 	<- match.call()
	CI.type	<- call$CI.type
	if (is.null(CI.type))
		CI.type <- "estimate"
	
	stopifnot(!is.null(newdata))
	stopifnot(is(obj, "VFP"))
	type <- match.arg(type[1], choices=c("vc", "sd", "cv"))
	
	if (is.null(model.no)) {				# automatically determine best fitting model
		AIC <- obj$AIC
		nam <- names(AIC)
		if("Model_10" %in% nam)
			AIC <- AIC[-which(nam == "Model_10")]
		num <- which(AIC== min(AIC))[1]
		AIC <- NULL
	} else {
		models <- sub("Model_", "", names(obj$RSS))
		if(!model.no %in% models) {
			stop("Specified model ", paste0("'", model.no, "'"),
					" is not among fitted models: ", 
					paste(models, collapse=", "),"!")
		} else {
			num <- which(models == model.no)
		}
	}
	
	model <- as.numeric(sub("Model_", "", names(obj$RSS[num])))	
	
	if (!"gnm" %in% class(obj$Models[[num]]) && model != 10) {
		message("Covariance-matrix for model ", model,
				" not available, CI cannot be estimated!")
		ci <- FALSE
	}
	
	### start bisection algorithm to find mean for given vc, sd or cv value
	
	if (length(newdata) > 1) {
		return(as.data.frame(t(sapply(newdata, function(x) predict_mean(obj=obj, newdata=x, type=type, tol=tol, model.no=model, ci=ci)))))
	}
	
	# adapt relative convergence tolerance to absolute value
	tol0 <- tol											# original relative tolerance
	tol  <- newdata * tol
	
	Min <- min(obj$Data$Mean, na.rm = TRUE)				# extrema
	Max <- max(obj$Data$Mean, na.rm = TRUE)
	
	Type <- switch(	type, 
			vc = "VC",
			sd = "SD",
			cv = "CV")
	
	# narrow search space, of special interst for functions with multiple intersections with target variance
	
	# iteratively increase range of X-values if necessary
	for (i in 1:5) {	
		cand	<- seq(Min, Max, length.out = 100)
		pred 	<- predict(obj, model.no = model.no, type = type, newdata = cand)
		
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
		
		if ((length(above) == 0 || length(below) == 0) ) {
			Max <- Max * 10
			Min <- Min / 10	
		} else {
			break
		}
	}
	
	if (CI.type == "estimate" && (length(above) == 0 || length(below) == 0) ) {
		message(paste0("No intersection with variance-function found for 
								specified Y-value up to X=",Max,"!"))
		res <- data.frame(Mean = NA, Y = newdata, Diff = NA, LCL = NA, UCL = NA)
		colnames(res)[2] <- Type
		return(res)
	}
	
	if (length(above) == 0)
		above <- NULL
	if (length(below) == 0)
		below <- NULL
	
	if (!is.null(above)) {
		above1 	<- above[ -length(above)]	# predecessor
		above2 	<- above[2:length(above)]	# successor
	}
	if (!is.null(below)) {
		below1 	<- below[ -length(below)]	# predecessor
		below2 	<- below[2:length(below)]	# successor
	}
	
	suppressWarnings(
			if (type == "cv") {			
						if (!is.null(above)) {
							ind1	<- min(which(above1+1 != above2))
							Min 	<- cand[ind1]
						}
						if (!is.null(below)) {
							ind2	<- min(below)
							Max		<- cand[ind2]
						}
					} else {
						if (!is.null(above)) {
							ind1	<- min(above)
							Max 	<- cand[ind1]
						}
						if (!is.null(below)) {
							ind2	<- min(which(below1+1 != below2))
							Min 	<- cand[ind2]
						}
					}
	)
	
	lower <- .Machine$double.eps*10				# to avoid infinite loops if solution is close to zero
	upper <- Max
	conc  <- lower + diff(c(lower, upper))/2
	best  <- c(Est = Inf, Diff = Inf)
	
	iter <- 0
	while(1) {
		pred <- predict(obj, newdata = conc, type = type, model.no = model)
		
		pred <- switch(	CI.type,
				estimate = pred[1, "Fitted"],
				LCL		 = pred[1, "LCL"],
				UCL		 = pred[1, "UCL"])
		
		Diff <- abs(newdata - pred)
		
		if (Diff < best["Diff"])							# remember best fit in case of non-convergence
			best <- c(Est = conc, Diff = Diff)
		
		if ( pred < newdata && type == "cv" ||
				pred > newdata && type != "cv") {
			upper 	<- conc
			conc 	<- conc - (conc - lower) / 2
		}
		if ( pred < newdata && type != "cv" ||
				pred > newdata && type == "cv") {
			lower 	<- conc
			conc 	<- conc + (upper-conc) / 2
		}
		
		if (Diff < tol || abs(diff(c(lower, upper))) < tol0 * lower) {		     #.Machine$double.eps)
			if (Diff >= tol)	{											    # not converged
				if ( (type == "cv" && is.null(below)) || 
						(type != "cv" && is.null(above)) ) {
					message("Upper bound of the CI could not be determined, return max(X-value)!")
					conc <- Max
				} else if ( (type == "cv" && is.null(above)) || (type != "cv" && is.null(below)) ) {
					message("Lower bound of the CI could not be determined, return min(X-value)!")
					conc <- Min	
				} else {
					message("Convergence criterion not met, return best approximation!")
					conc <- best["Est"]
				}
				
				Diff <- best["Diff"]
			}
			break
		}
	}
	
	res <- data.frame(Mean=conc, Yvalue=newdata, Diff=Diff)
	
	if (ci) {
		# CI for prediction derived from CI of variance at prediction
		LCL <- UCL <- NULL
		
		if (CI.type == "estimate") {
			LCL	<- predict_mean(obj, model.no = model.no, newdata = newdata, 
					alpha = alpha, CI.type = "LCL", type = type)$Mean
			UCL	<- predict_mean(obj, model.no = model.no, newdata = newdata, 
					alpha = alpha, CI.type = "UCL", type = type)$Mean
			
			if (LCL < UCL)						# on variance-scale function is increasing on CV-scale decreasing
				CI  <- c(LCL=LCL, UCL=UCL)
			else
				CI <- c(LCL=UCL, UCL=LCL)
		}
		
		res <- data.frame(Mean=conc, Yvalue=newdata, Diff=Diff)
		
		if (CI.type == "estimate") {
			res$LCL <- CI["LCL"]
			res$UCL <- CI["UCL"]
			colnames(res)[2] <- Type
		}	
	} else {
		res$LCL <- NA
		res$UCL <- NA
		colnames(res)[2] <- Type
	}
	return(res)
}




#' Fit CLSI EP17 Model Using log-transformed X and Y.
#' 
#' This function fits the model proposed in CLSI EP17 by log-transforming
#' CV (Y) as well as mean-values (X) und performing a linear regression of these.
#' More specifically CV = A * Conc^B, where Conc = mean concentration of a sample and CV is
#' on the percent-scale, is fitted by ordinary least squares (OLS) estimation of
#' log(CV) = A + B * log(Conc). Fitted values are subsequently back-transformed
#' using formula cv = exp(a) * C^b, where cv, a and b represent estimates of CV, A and B.
#' Therefore, this model does not fall within the same class as models 1 to 9, 
#' although the predictor function is identical to that of model 9. This also has 
#' the consequence that regression statistics, like AIC or deviance, are not directly
#' comparable to those of models 1 to 9. 
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
#' @aliases fit.EP17
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
#' # function fit_ep17 is not exported, use package namesspace in call
#' fit.lot1 <- VFP:::fit_ep17(x=lot1$Mean, y=lot1$CV, typeY="cv", DF=lot1$DF)
#' }

fit_ep17 <- function(x, y, DF, typeY = c("vc", "sd", "cv"), k = 2, ...) {
	x		<- as.numeric(unlist(x))
	y		<- as.numeric(unlist(y))
	typeY 	<- match.arg(typeY[1], choices = c("vc", "sd", "cv"))
	x.out 	<- y.out <- NULL
	
	if (typeY == "vc") {
		y.vc 	<- y
		y 		<- 100*sqrt(y)/x
	} else if (typeY == "sd") {
		y.vc 	<- y^2
		y 		<- 100*y/x
	} else {
		y.vc <- (y*x/100)^2
	}
	
	xl 				<- log(x)
	yl 				<- log(y)
	fit 			<- lm(yl~xl, weights=DF)
	fit$fit.orig	<- fit
	fitted.vc		<- predict_model_ep17(fit, ci.type="vc")$Fitted	# transform to variance-scale for comparison to other models
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

predict_model_ep17 <- function(	object, newdata = NULL, alpha = .05,
		ci.type = c("vc", "sd", "cv"), CI.method = c("chisq", "t", "normal"), 
		use.log = FALSE,  ...) {	
	CI.method	<- match.arg(CI.method[1], choices=c("chisq", "t", "normal"))
	ci.type 	<- match.arg(ci.type[1], choices=c("vc", "sd", "cv", "log"))
	
	if (is.null(newdata)) {
		Mean	<- exp(object$model$xl)
		newdata <- object$model$xl		# fitted values will be returned
		x.out	<- exp(newdata)
	} else {
		x.out <- Mean <- newdata
		
		if (is.numeric(newdata))
			newdata <- log(newdata)
		else
			newdata <- log(newdata$Mean)
	}
	Mean		<- as.numeric(unlist(Mean))
	pred 		<- predict(object$fit.orig, newdata=data.frame(xl=newdata), se.fit=TRUE)
	pred		<- as.data.frame(pred)
	pred$fit	<- as.numeric(pred$fit)
	pred$se.fit	<- as.numeric(pred$se.fit)
	if (CI.method == "normal") {
		Qnorm 		<- qnorm(1-alpha/2)
		CIupper 	<- exp(pred$fit+Qnorm*pred$se.fit)
		CIlower		<- exp(pred$fit-Qnorm*pred$se.fit)
		pred$fit	<- exp(as.numeric(pred$fit))
	} else if (CI.method == "chisq") {
		pred$fit	<- exp(as.numeric(pred$fit))
		pred$se.fit <- pred$se.fit * pred$fit			# adjust standard errors for original CV-scale
		df.qchisq		<- 0.5*(pred$fit/pred$se.fit)^2
		lower.qchisq 	<- qchisq(1-alpha/2, df=df.qchisq)
		upper.qchisq 	<- qchisq(alpha/2, df=df.qchisq)
		CIlower			<- pred$fit*sqrt(df.qchisq/lower.qchisq)
		CIupper			<- pred$fit*sqrt(df.qchisq/upper.qchisq)
	} else if (CI.method == "t") {
		Qtdist 		<- qt(1 - alpha/2, df.residual(object$fit.orig))
		CIupper 	<- exp(pred$fit+Qtdist*pred$se.fit)
		CIlower		<- exp(pred$fit-Qtdist*pred$se.fit)
		pred$fit	<- exp(as.numeric(pred$fit))
	}
	
	pred$CIlower 	<- CIlower
	pred$CIupper 	<- CIupper
	attr(pred, "conf.level") <- 1-alpha 
	
	# transfrom fit and CIlower and CIupper to respective user-requested scale
	if (ci.type %in% c("sd", "vc")) {
		pred$fit 		<- pred$fit * Mean / 100
		pred$CIlower 	<- pred$CIlower * Mean / 100
		pred$CIupper 	<- pred$CIupper * Mean / 100
		
		if (ci.type == "vc") {
			pred$fit 		<- pred$fit^2
			pred$CIlower 	<- pred$CIlower^2
			pred$CIupper 	<- pred$CIupper^2
		}
	}
	
	if(use.log) {
		pred$fit 		<- log(pred$fit)
		pred$CIlower 	<- log(pred$CIlower)
		pred$CIupper 	<- log(pred$CIupper)
	}
	
	colnames(pred)	<- c("Fitted", "SE", "Mean", "Scale", "LCL", "UCL")
	
	pred$Mean <- Mean
	
	return(pred)
}

