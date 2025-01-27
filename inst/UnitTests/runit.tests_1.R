# TODO: Add unit-test functions which can be automatically run
# 
# Author: schueta6
###############################################################################

library(VCA)

###
data(MultiLotReproResults)


fit.all.models  <- fit.vfp(MultiLotReproResults, model.no=1:9)
means			<- MultiLotReproResults$Mean 

#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc Tests model 6 against parameter estimates of VFP-software version 14.0

TF001.model6 <- function(x)
{
    ref.coef <- c( 0.094117579, -0.010166326, 0.001864494,  2.030292924) 
    ref.deviance <-0.8143275
    # For comparison values from Sadlers Variance Function Program 2016:
	#B1 = 0.0941176016, B2 = -0.010166383, B3 = 0.0018645146, J = 2.03029006
    #Log[LR] = 0.81433
	tst.coef <- as.numeric(fit.all.models$Model$model6$coefficients)
	tst.deviance <- as.numeric(fit.all.models$Model$model6$deviance)
	
	checkEquals(tst.coef, ref.coef,tolerance=1E-6)
	checkEquals(tst.deviance, ref.deviance,tolerance=1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc Tests model 7 against parameter estimates of VFP-software version 14.0

TF002.model7 <- function(x)
{
    ref.coef <- c(0.0780494174,0.0005268289,2.3337820976)
    ref.deviance <- 2.05935
	
    tst.coef <- as.numeric(fit.all.models$Model$model7$coefficients)
	tst.deviance <- as.numeric(fit.all.models$Model$model7$deviance)
	
	checkEquals(tst.coef, ref.coef,tolerance=1E-6)
	checkEquals(tst.deviance, ref.deviance,tolerance=1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc Tests model 8 against parameter estimates of VFP-software version 14.0

TF003.model8 <- function(x)
{
    ref.coef <- c(0.49406868,0.01869976,3.97360976)
 
    ref.deviance <- 8.892474
    # For comparison values from Sadlers Variance Function Program 2016:
	#B1 = 0.4939963105, B2 = 0.0187035392, J = 3.97287275
    #Log[LR] = 8.89247	
    tst.coef <- as.numeric(fit.all.models$Model$model8$coefficients)
	tst.deviance <- as.numeric(fit.all.models$Model$model8$deviance)
	
	checkEquals(tst.coef, ref.coef,tolerance=1E-6)
	checkEquals(tst.deviance, ref.deviance,tolerance=1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc check whether exact parameters of model 1 will be recovered

TF004.exact1 <- function(x){
	#exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
	Mean <- seq(1,10,1)
	VC0   <- rep(1,10)
	DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
	Daten <-data.frame(Mean,VC=VC0,DF)
	res <- fit.vfp(Data=Daten,model.no=1,quiet=T)$Models$model1$coefficients
	checkEquals(as.numeric(res),c(1),tolerance=.Machine$double.eps^0.5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc check whether exact parameters of model 2 will be recovered

TF005.exact2 <- function(x){
	#exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
	Mean <- seq(1,10,1)
	VC0   <- rep(1,10)
	DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
	VC <- VC0 * Mean^2
	Daten <-data.frame(Mean,VC=VC,DF)
	res <- fit.vfp(Data=Daten,model.no=2,quiet=T)$Models$model2$coefficients
	checkEquals(as.numeric(res),c(1),tolerance=.Machine$double.eps^0.5)
}
#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc check whether exact parameters of model 3 will be recovered, exponent 1

TF006.exact3 <- function(x){
	#exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
	Mean <- seq(1,10,1)
	VC0   <- rep(1,10)
	DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
	VC <- VC0 * (1 + Mean^2)
	Daten <-data.frame(Mean,VC=VC,DF)
	res <- fit.vfp(Data=Daten,model.no=3,quiet=T)$Models$model3$coefficients
	checkEquals(as.numeric(res),c(1,1),tolerance=.Machine$double.eps^0.5)
}
#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc check whether exact parameters of model 3 will be recovered, negative exponent

TF007.exact3minus <- function(x){
	#exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
	Mean <- seq(1,10,1)
	VC0   <- rep(1,10)
	DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
	VC <- VC0 * (1 - 0.005*Mean^2)
	Daten <-data.frame(Mean,VC=VC,DF)
	res <- fit.vfp(Data=Daten,model.no=3,quiet=T)$Models$model3$coefficients
	checkEquals(as.numeric(res),c(1,-0.005),tolerance=.Machine$double.eps^0.5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc check whether exact parameters of model 4 will be recovered

TF008.exact4 <- function(x){
	#exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
	Mean <- seq(1,10,1)
	VC0   <- rep(1,10)
	DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
	VC <- VC0 * (1 + Mean)^2
	Daten <-data.frame(Mean,VC=VC,DF)
	res <- fit.vfp(Data=Daten,model.no=4,quiet=T)$Models$model4$coefficients
	checkEquals(as.numeric(res),c(1,1),tolerance=.Machine$double.eps^0.5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc check whether exact parameters of model 5 will be recovered

TF009.exact5 <- function(x){
	#exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
	Mean <- seq(1,10,1)
	VC0   <- rep(1,10)
	DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
	VC <- VC0 * (1 + Mean^3)
	Daten <-data.frame(Mean,VC=VC,DF)
	res <- fit.vfp(Data=Daten,model.no=5,K=3,quiet=T)$Models$model5$coefficients
	checkEquals(as.numeric(res),c(1,1),tolerance=.Machine$double.eps^0.5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc check whether exact parameters of model 6 will be recovered

TF010.exact6 <- function(x){
	#exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
	Mean <- seq(1,10,1)
	VC0   <- rep(1,10)
	DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
	VC <- 1000 - 100 * Mean + Mean^3
	Daten <-data.frame(Mean,VC=VC,DF)
	res <- fit.vfp(Data=Daten,model.no=6,quiet=T)$Models$model6$coefficients
	checkEquals(as.numeric(res),c(1000,-100,1,3),tolerance=.Machine$double.eps^0.5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc check whether exact parameters of model 7 will be recovered

TF011.exact7 <- function(x){
	#exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
	Mean <- seq(1,10,1)
	VC0   <- rep(1,10)
	DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
	VC <- VC0 * (1 + Mean^3)
	Daten <-data.frame(Mean,VC=VC,DF)
	res <- fit.vfp(Data=Daten,model.no=7,quiet=T)$Models$model7$coefficients
	checkEquals(as.numeric(res),c(1,1,3),tolerance=.Machine$double.eps^0.5)
}
#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc check whether exact parameters of model 8 will be recovered, positive beta2

TF012.exact8 <- function(x){
	#exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
	Mean <- seq(1,10,1)
	VC0   <- rep(1,10)
	DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
	VC <- VC0 * (1 + Mean)^3
	Daten <-data.frame(Mean,VC=VC,DF)
	res <- fit.vfp(Data=Daten,model.no=8,quiet=T)$Models$model8$coefficients
	checkEquals(as.numeric(res),c(1,1,3),tolerance=.Machine$double.eps^0.5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc check whether exact parameters of model 3 will be recovered, negative beta2

TF013.exact8minus <- function(x){
	#exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
	Mean <- seq(1,10,1)
	VC0   <- rep(1,10)
	DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
	VC <- VC0 * (1 -0.05*Mean)^0.5
	Daten <-data.frame(Mean,VC=VC,DF)
	res <- fit.vfp(Data=Daten,model.no=8,quiet=T)$Models$model8$coefficients
	checkEquals(as.numeric(res),c(1,-0.05,0.5),tolerance=.Machine$double.eps^0.5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  Fun101
#* **desc check whether exact parameters of model 9 will be recovered

TF014.exact9 <- function(x){
	#exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
	Mean <- seq(1,10,1)
	VC0   <- rep(1,10)
	DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
	VC <- VC0 * Mean^3
	Daten <-data.frame(Mean,VC=VC,DF)
	res <- fit.vfp(Data=Daten,model.no=9,quiet=T)$Models$model9$coefficients
	checkEquals(as.numeric(res),c(1,3),tolerance=.Machine$double.eps^0.5)
}

#* **target predictMean
#* **riskid RA04
#* **funid  Fun106
#* **desc Test whether the predictMean-function correctly handles situations where no concentrations can be found.
TF015.predictMean <- function(x)
{	
	
	cat("\n\n#####################################################################################\n\n")
	cat("\nWithin TF078.predictMean\n\n")
	print(ls())
	if(!"fit.all.models" %in% ls()) {
		library(VCA)
		data(MultiLotReproResults)
		fit.all.models  <- fit.vfp(MultiLotReproResults, model.no=1:9)
	}
	res <- predictMean(fit.all.models, model.no=6, type="cv", newdata=4.2) 
	checkEquals(res$Mean, 15.08867, tolerance=1e-6)
	checkEquals(res$LCL,  6.253166, tolerance=1e-6)
	checkEquals(res$UCL,  9272564, tolerance=1e-6)				# now upper bound found, max X-value returned with message
}

#* **target getMat.VCA
#* **riskid RA06
#* **funid  Fun201
#* **desc Test whether sequences of variance components are correctly processed in function 'getMat.VCA'
TF016.getMat.VCA_sequences <- function()
{
	data(VCAdata1, package="VCA")
	lst <- anovaVCA(y~(lot+device)/day/run, VCAdata1, by="sample")
	mat <- getMat.VCA(lst, 4:6)
	mat <- mat[order(as.numeric(sub("sample.", "", rownames(mat)))),]
	VC  <- sapply(lst, function(x) sum(x$aov.tab[4:6, "VC"]))
	DF  <- sapply(lst, function(x){
				Ci <- getMat(x, "Ci.MS")
				Ci <- Ci[3:5, 3:5]
				MS <- x$aov.tab[4:6, "MS"]
				DF <- x$aov.tab[4:6, "DF"]
				DF <- VCA:::SattDF(MS, Ci, DF, "total") 
				DF
			})
	Mean <- sapply(lst, function(x) x$Mean)
	checkEquals(mat[,"VC"],   as.numeric(VC))
	checkEquals(mat[,"Mean"], as.numeric(Mean))
	checkEquals(mat[,"DF"],   as.numeric(DF))
}


#* **target getMat.VCA
#* **riskid RA06
#* **funid  Fun201
#* **desc Are sequences of variance components correctly processed when fitting VFP-models directly on a list of VCA-objects
TF017.fit.vfp_VC_sequences <- function()
{
	data(VCAdata1, package="VCA")
	lst 	<- anovaVCA(y~(lot+device)/day/run, VCAdata1, by="sample")
	mat0 	<- getMat.VCA(lst, 4:6)
	vfp		<- fit.vfp(lst, 1, vc=4:6)
	mat1 	<- vfp$Data
	checkEquals(mat0[,"VC"],   mat1[,"VC"])
	checkEquals(mat0[,"Mean"], mat1[,"Mean"])
	checkEquals(mat0[,"DF"],   mat1[,"DF"])
}


#* **target predictMean
#* **riskid RA04
#* **funid  Fun106
#* **desc Test predictMean-function comprehensively, 7 models, 3 predictions each back and forth.

TF018.predictMean <- function(x)
{	
	rng <- range(MultiLotReproResults$Mean)
	x0  <- round(seq(rng[1]*0.025, rng[2]*.075, length.out=10), 3)
	for(i in c(1,3,4,6,7,8,9)) {
		if(i == 5)
			next
		pred <- predict(fit.all.models, model.no=i,  type="cv", newdata=x0)$Fitted
		x1	 <- predictMean(fit.all.models, model.no=i, newdata=pred, type="cv", tol=1e-6)
		#print(cbind(X0=x0, pred=round(unlist(x1$Mean), 3)))
	}
}

#* **target fit.vfp
#* **riskid RA03
#* **funid  Fun105
#* **desc extensive testing of S3 method coef for objects of class "VFP"

TF019.coef <- function(x)
{
	models <- names(fit.all.models$Models)
	
	for(i in 1:length(models)) {
		model	<- as.numeric(sub("model", "", models[i]))
		coef0 	<- as.numeric(fit.all.models$Models[[models[i]]]$coefficients)
		coef1 	<- as.numeric(coef(fit.all.models, model.no=model))
		checkEquals(coef1, coef0, tol=1e-12)
	}
}

#* **target predict.VFP
#* **riskid RA02
#* **funid Fun104
#* **desc model 6 could not be automatically selected as best model in predict.VCA

TF020.predictModel6 <- function() {

	dat <- data.frame(
			Mean=c(582.340597362797,490.631303876108,312.954949544278,238.443472329726,120.111024593649),
			VC=c(60740.9512627291,7971.39850756505,890.117016131418,41.2891014866317,932.23715614741),
			DF=1
	)
	# model 6 has lowest AIC
	fits <- try(fit.vfp(dat, 1:9))
	pred <- try(predict(fits, type="vc")$Fitted, silent=TRUE)
	checkTrue(!is(pred, "try-error"))
	checkTrue(is(pred, "numeric"))
}

#* **target predictMean
#* **riskid RA02
#* **funid Fun104
#* **desc model 6 could not be automatically selected as best model in predictMean

TF021.predictMeanModel6 <- function() {
	
	dat <- data.frame(
			Mean=c(582.340597362797,490.631303876108,312.954949544278,238.443472329726,120.111024593649),
			VC=c(60740.9512627291,7971.39850756505,890.117016131418,41.2891014866317,932.23715614741),
			DF=1
	)
	# model 6 has lowest AIC
	fits <- try(fit.vfp(dat, 1:9))
	pred <- try(predictMean(fits, type="cv", newdata=15)$Mean, silent=TRUE)
	checkTrue(!is(pred, "try-error"))
	checkTrue(is(pred, "numeric"))
}


#* **target plot.VFP
#* **riskid RA02
#* **funid Fun102
#* **desc model 6 could not be automatically selected as best model in predictMean

TF022.plotModel6 <- function() {
	
	dat <- data.frame(
			Mean=c(582.340597362797,490.631303876108,312.954949544278,238.443472329726,120.111024593649),
			VC=c(60740.9512627291,7971.39850756505,890.117016131418,41.2891014866317,932.23715614741),
			DF=1
	)
	# model 6 has lowest AIC
	fits <- try(fit.vfp(dat, 1:9))
	ret  <- try(plot(fits))
	checkTrue(!is(ret, "try-error"))
}

#* **target getMat.VCA
#* **riskid RA06
#* **funid  Fun201
#* **desc Does getMat.VCA correcly process a list of VFP-objects fitted by remlVCA
TF023.getMat.VCA.REML <- function()
{
	data(VCAdata1, package="VCA")
	lst 	<- remlVCA(y~(lot+device)/day/run, VCAdata1, by="sample")
	mat1 	<- getMat.VCA(lst)
	VCs		<- sapply(lst, function(x) x$aov.tab["total", "VC"])
	Means	<- sapply(lst, function(x) x$Mean)
	DFs		<- sapply(lst, function(x) x$aov.tab["total", "DF"])
	mat1	<- mat1[names(VCs),]
	checkEquals(as.numeric(VCs),   mat1[,"VC"])
	checkEquals(as.numeric(Means), mat1[,"Mean"])
	checkEquals(as.numeric(DFs),   mat1[,"DF"])
}

#* **target getMat.VCA
#* **riskid RA06
#* **funid  Fun201
#* **desc Does getMat.VCA correcly process a list of VFP-objects fitted by remlVCA for intermediate sums of variance components
TF024.getMat.VCA.REML_sequences <- function()
{
	data(VCAdata1, package="VCA")
	lst 	<- remlVCA(y~(lot+device)/day/run, VCAdata1, by="sample")
	mat1 	<- getMat.VCA(lst, vc=4:6)
	lst0 	<- lapply(lst, getIP.remlVCA, vc="lot:device:day")
	mat0	<- t(sapply(lst0, function(x) return(c(Mean=x$Mean, DF=x$aov.tab["total", c("DF", "VC", "SD", "CV[%]")]))))
	mat2	<- matrix(unlist(mat0), ncol=ncol(mat0))
	rownames(mat2) <- rownames(mat0)
	colnames(mat2) <- colnames(mat0)
	mat0 	<- mat2
	colnames(mat0) <- c("Mean", "DF", "VC", "SD", "CV")
	mat1	<- mat1[rownames(mat0),]
	checkEquals(as.numeric(mat0[,"VC"]),   mat1[,"VC"])
	checkEquals(as.numeric(mat0[,"Mean"]), mat1[,"Mean"])
	checkEquals(as.numeric(mat0[,"DF"]),   mat1[,"DF"])
}