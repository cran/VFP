# TODO: Add unit-test functions which can be automatically run
# 
# Author: schueta6
###############################################################################

library(VCA)

###
data(MultiLotReproResults)


fit.all.models  <- fit.vfp(MultiLotReproResults, model.no=1:9)
means			<- MultiLotReproResults$Mean 

# Tests model 6 against parameter estimates of VFP-software version 14.0

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

TF002.model7 <- function(x)
{
    ref.coef <- c(0.0780494174,0.0005268289,2.3337820976)
    ref.deviance <- 2.05935
	
    tst.coef <- as.numeric(fit.all.models$Model$model7$coefficients)
	tst.deviance <- as.numeric(fit.all.models$Model$model7$deviance)
	
	checkEquals(tst.coef, ref.coef,tolerance=1E-6)
	checkEquals(tst.deviance, ref.deviance,tolerance=1E-6)
}
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

TF004.exact1 <- function(x){
    #exact parameters should be recovered from variances which are equal to the expected values from the assumed VF. 
    Mean <- seq(1,10,1)
    VC0   <- rep(1,10)
    DF   <- rep(1,10)*10 # corresponds to 10 replicas per pointVC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
    Daten <-data.frame(Mean,VC=VC0,DF)
    res <- fit.vfp(Data=Daten,model.no=1,quiet=T)$Models$model1$coefficients
    checkEquals(as.numeric(res),c(1),tolerance=.Machine$double.eps^0.5)
}

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

# Test whether the predictMean-function correctly handles situations where no concentrations can be found.
TF015.predictMean <- function(x)
{	
	res <- predictMean(fit.all.models, model.no=6, type="cv", newdata=4.2) 
	checkEquals(res$Mean, 15.08867, tolerance=1e-6)
	checkEquals(res$LCL,  6.532276, tolerance=1e-6)
	checkEquals(res$UCL,  9272564, tolerance=1e-6)				# now upper bound found, max X-value returned with message
}

# Test whether sequences of variance components are correctly processed in function 'getMat.VCA'
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


# Test whether sequences of variance components are correctly processed when
# fitting VFP-models directly on a list of VCA-objects
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

