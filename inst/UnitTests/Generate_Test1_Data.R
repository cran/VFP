# Primitive test cases
# VC values are set to their expectation values
# The estimates calculated by the program should be exactly the parameters of the model
# 
# Author: dufeyf
###############################################################################

set.seed(1)
Mean <- seq(1,10,1)
VC0   <- rep(1,10)
DF   <- rep(1,10)*10 # corresponds to 10 replicas per point

#Model 1:
VC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
Daten1 <-data.frame(Mean,VC,DF)
save(Daten1,file="C:/TFS/R/Pkg_VFP/Main/VFP/data/Test1_Daten1.RData")
res <- fit.vfp(Data=Daten1,model=1,quiet=FALSE)
res #B1=1.003
#B1 
#1.007 
#AIC = 51.23  RSS = 1.420  Deviance = 8.573 GoF P-value= 0.4776 
x11()
plot(res,model.no=1,type="vc")

#######################################
# schueta6 2018-02-20

get.RefData <- function(model=NULL, Nsim=1e3, seed=23, ylim=NULL, DF=10, CI.method="normal", ...)
{
	stopifnot(model %in% 1:9)
	
	legend.pos <- c("bottomleft", rep("left", 4), "bottomleft", rep("left", 3))
	legend.pos <- legend.pos[model]
	
	set.seed(seed)
	Mean <- seq(1,10,1)
	VC0  <- VC1 <- rep(1,10)
	DF   <- rep(DF, 10)		# corresponds to 10 replicats per point

	sim.mat <- sim.mat2 <- matrix(NA, nrow=Nsim, ncol=length(Mean))
	colnames(sim.mat) <- Mean
	
	for(i in 1:nrow(sim.mat))
	{
		if(model == 1)
			VC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
		else if(model == 2)
		{
			VC1 <- VC0 * Mean^2
			VC 	<- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC 	<- VC * VC1
		}
		else if(model == 3)
		{
			VC1 <- VC0 * (1 + Mean^2)
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC*VC1
		}
		else if(model == 4)
		{
			VC1 <- VC0 * (1 + Mean)^2
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC * VC1
		}
		else if(model == 5)
		{
			VC1 <- VC0 * (1 + Mean^3)
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC * VC1
		}
		else if(model == 6)
		{
			DF  <- rep(1000, 10) 			# corresponds to 1000 replicats per point
			VC1 <- 1000 - 100 * Mean + Mean^3
			VC 	<- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC 	<- VC * VC1
		}
		else if(model == 7)
		{
			DF  <- rep(10,10)  				# corresponds to 1000 replicas per point
			VC1 <- VC0 * (1 + Mean^3)
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC * VC1
		}
		else if(model == 8)
		{
			VC1 <- VC0 * (1 + Mean)^3
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC * VC1
		}
		else if(model == 9)
		{
			VC1 <- VC0 * Mean^3
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC * VC1
		}
		sim.mat[i,]	<- VC
		capture.output(tmp.fit <- fit.vfp(data.frame(Mean, VC=VC, DF), model.no=model))

		predictions <- try(predict(tmp.fit), silent=TRUE)
		if(class(predictions$Fitted) == "numeric")
			sim.mat2[i,] <- predictions$Fitted			# otherwise this row will consist of NA entirely
		if(i %% 50 == 0)
			cat("\nIteration", i,"completed ...")
	}

	Q  		<- apply(sim.mat,  2, quantile, type=2, probs=c(.01, .025, .05, .1, .5, .9, .95, .975, .99), na.rm=TRUE)
	Q2 		<- apply(sim.mat2, 2, quantile, type=2, probs=c(.01, .025, .05, .1, .5, .9, .95, .975, .99), na.rm=TRUE)
	MeanVal	<- apply(sim.mat2, 2, mean, na.rm=TRUE)

#	RSS 	<- apply(sim.mat2, 1, function(x) sum((Q2["50%",]-x)^2))	# deviation from median
	RSS		<- apply(sim.mat2, 1, function(x) sum((MeanVal-x)^2))			# deviation form mean
	ind 	<- which(RSS == min(RSS, na.rm=TRUE))

	RefData <- data.frame(Mean, VC=as.numeric(sim.mat[ind,]), DF)
	
	fit <- fit.vfp(RefData, model.no=model)
	plot(fit, model.no=model, type="vc", ylim=ylim, CI.method=CI.method, ...)
	
	for(i in 1:ncol(sim.mat))
	{
		lines(rep(Mean[i], 2),  Q2[c("1%", "99%"),i], col="red", lwd=2)
		lines(rep(Mean[i], 2),  Q2[c("2.5%", "97.5%"),i], col="blue", lwd=2)
		lines(rep(Mean[i], 2),  Q2[c("5%", "95%"),i], col="cyan", lwd=2)
		lines(rep(Mean[i], 2),  Q2[c("10%", "90%"),i], col="magenta", lwd=2)
		points(rep(Mean[i], 2), rep(Q2["50%",i], 2), pch=16, col="white", lwd=2)
	}
	
	legend(	legend.pos, col=c("red", "blue", "cyan", "magenta"), lwd=2, text.font=2, 
			legend=c("[  Q1; Q99]", "[Q2.5; Q97.5]", "[  Q5; Q95]", "[ Q10; Q90]"))	
	
	res <- list(model=model,
				RefData=RefData,
				simData=sim.mat,
				simPredictions=sim.mat2,
				RefInd=ind,
				Qpred=Q2)
		
	class(res) <- "VfpRefData"
		
	return(res)
}

plot.VfpRefData <- function(obj, Q, ...)
{
	legend.pos <- c("bottomleft", rep("left", 4), "bottomleft", rep("left", 3))
	legend.pos <- legend.pos[obj$model]
	
	capture.output(fit <- fit.vfp(obj$RefData, model.no=obj$model))
	
	plot(fit, ...)
	Mean <- obj$RefData$Mean
	
	for(i in 1:ncol(obj$Qpred))
	{
		lines(rep(Mean[i], 2),  obj$Qpred[c("1%", "99%"),i], col="red", lwd=2)
		lines(rep(Mean[i], 2),  obj$Qpred[c("2.5%", "97.5%"),i], col="blue", lwd=2)
		lines(rep(Mean[i], 2),  obj$Qpred[c("5%", "95%"),i], col="cyan", lwd=2)
		lines(rep(Mean[i], 2),  obj$Qpred[c("10%", "90%"),i], col="magenta", lwd=2)
		points(rep(Mean[i], 2), rep(obj$Qpred["50%",i], 2), pch=16, col="white", lwd=2)
	}
	
	legend(	legend.pos, col=c("red", "blue", "cyan", "magenta"), lwd=2, text.font=2, 
			legend=c("[  Q1; Q99]", "[Q2.5; Q97.5]", "[  Q5; Q95]", "[ Q10; Q90]"))	
}

RefData1 <- get.RefData(model=1)
RefData2 <- get.RefData(model=2)
RefData3 <- get.RefData(model=3)
RefData4 <- get.RefData(model=4, seed=42)
RefData4.df100 <- get.RefData(model=4, seed=42)
RefData5 <- get.RefData(model=5)
RefData6 <- get.RefData(model=6)
RefData7 <- get.RefData(model=7)
RefData8 <- get.RefData(model=8)
RefData9 <- get.RefData(model=9)


# Check coverage of CI according to definition of CI, i.e. when repeating data generating process
# the CI shall contain the true value in 100x(1-alpha)\% of the cases.

get.RefData0 <- function(	model=NULL, Nsim=1e3, seed=23, ylim=NULL, DF=10, 
							Type=c("one-sided", "two-sided"), K=2, quiet=FALSE, ...)
{
	stopifnot(model %in% 1:10)
	
	stopifnot(all(Type %in% c("one-sided", "two-sided")))

	legend.pos <- c("bottomleft", rep("left", 4), "bottomleft", rep("left", 3))
	legend.pos <- legend.pos[model]
	
	set.seed(seed)
	Mean <- seq(1,10,1)
	VC0  <- VC1 <- rep(1,10)
	CV0  <- c(40, 15, 10, rep(5, 7))
	DF   <- rep(DF, 10)		# corresponds to 10 replicats per point
	
	In.norm.ts <- In.norm.os <- In.t.ts <- In.t.os <- In.chisq.ts <- In.chisq.os <- 0
	Out.norm.ts <- Out.norm.os <- Out.t.ts <- Out.t.os <- Out.chisq.ts <- Out.chisq.os <- 0
	
	x.out.os <- x.out.ts <- y.out.os <- y.out.ts <- NULL
	
	for(i in 1:Nsim)
	{
		if(model == 1)
			VC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
		else if(model == 2)
		{
			VC1 <- VC0 * Mean^2
			VC 	<- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC 	<- VC * VC1
		}
		else if(model == 3)
		{
			VC1 <- VC0 * (1 + Mean^2)
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC*VC1
		}
		else if(model == 4)
		{
			VC1 <- VC0 * (1 + Mean)^2
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC * VC1
		}
		else if(model == 5)
		{
			VC1 <- VC0 * (1 + Mean^3)
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC * VC1
		}
		else if(model == 6)
		{
			DF  <- rep(1000, 10) 			# corresponds to 1000 replicats per point
			VC1 <- 1000 - 100 * Mean + Mean^3
			VC 	<- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC 	<- VC * VC1
		}
		else if(model == 7)
		{
			DF  <- rep(10,10)  				# corresponds to 1000 replicas per point
			VC1 <- VC0 * (1 + Mean^3)
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC * VC1
		}
		else if(model == 8)
		{
			VC1 <- VC0 * (1 + Mean)^3
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC * VC1
		}
		else if(model == 9)
		{
			VC1 <- VC0 * Mean^3
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC * VC1
		}
		else if(model == 10)
		{
			VC1 <- VC0 * Mean^3
			#VC1 <- (CV0*Mean/100)^2
			VC  <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x, 1) / x)}))
			VC  <- VC * VC1
		}

		tmp.Data <<- data.frame(Mean, VC=VC, DF)
		capture.output(tmp.fit <- try( fit.vfp(tmp.Data, model.no=model, K=K), silent=TRUE ))

		if(class(tmp.fit) == "try-error")
			next								# proceed with next iteration in case of errors
#plot(tmp.fit, type="cv")	
#points(Mean, 100*sqrt(VC1)/Mean, pch=17, col="cyan")
#scan()
		predictions.norm 	<- try(predict(tmp.fit, CI.method="normal"), silent=TRUE)
		predictions.t 		<- try(predict(tmp.fit, CI.method="t"), 	 silent=TRUE)
		predictions.chisq 	<- try(predict(tmp.fit, CI.method="chisq"),  silent=TRUE)

		if(class(predictions.norm$Fitted) == "numeric")
		{
			preds <- predictions.norm
			
			if("two-sided" %in% Type)
				tmpIn.ts <- length(which(preds$LCL <= VC1 & preds$UCL >= VC1))
			if("one-sided" %in% Type) 
				tmpIn.os <- length(which(preds$UCL >= VC1))
			In.norm.ts 	<- In.norm.ts  + tmpIn.ts
			Out.norm.ts	<- Out.norm.ts + length(VC1) - tmpIn.ts 
			In.norm.os 	<- In.norm.os  + tmpIn.os
			Out.norm.os	<- Out.norm.os + length(VC1) - tmpIn.os 
		}
		if(class(predictions.t$Fitted) == "numeric")
		{
			preds 	<- predictions.t
			
			if("two-sided" %in% Type)
				tmpIn.ts <- length(which(preds$LCL <= VC1 & preds$UCL >= VC1))
			if("one-sided" %in% Type)  
				tmpIn.os <- length(which(preds$UCL >= VC1))
			In.t.ts 	<- In.t.ts  + tmpIn.ts
			Out.t.ts 	<- Out.t.ts + length(VC1) - tmpIn.ts 
			In.t.os 	<- In.t.os  + tmpIn.os
			Out.t.os 	<- Out.t.os + length(VC1) - tmpIn.os 
		}
		if(class(predictions.chisq$Fitted) == "numeric")
		{
			preds <- predictions.chisq
			
			if("two-sided" %in% Type)
			{
				indIn.ts <- which(preds$LCL <= VC1 & preds$UCL >= VC1)
				tmpIn.ts <- length(indIn.ts)
				if(length(indIn.ts) > 0)
				{
					x.out.ts <- c(x.out.ts, Mean[-indIn.ts])
					y.out.ts <- c(y.out.ts,  VC1[-indIn.ts]*100/Mean[-indIn.ts])
				}
			}
			if("one-sided" %in% Type) 
			{
				indIn.os <- which(preds$UCL >= VC1)
				tmpIn.os <- length(indIn.os)
				if(length(indIn.os) > 0)
				{
					x.out.os <- c(x.out.os, Mean[-indIn.os])
					y.out.os <- c(y.out.os,  VC1[-indIn.os]*100/Mean[-indIn.os])
				}
			}
	
			In.chisq.ts 	<- In.chisq.ts  + tmpIn.ts
			Out.chisq.ts 	<- Out.chisq.ts + length(VC1) - tmpIn.ts 
			In.chisq.os 	<- In.chisq.os  + tmpIn.os
			Out.chisq.os 	<- Out.chisq.os + length(VC1) - tmpIn.os 
		}
		if(i %% 50 == 0 && !quiet)
			cat("\nIteration", i,"completed ...")
	}
#print(data.frame(X.Out.TS=x.out.ts, Y.Out.TS=y.out.ts))
#print(data.frame(X.Out.OS=x.out.os, Y.Out.OS=y.out.os))

plot(tmp.fit, type="cv")
points(x.out.ts, y.out.ts, pch=16, col="red", cex=1.25)
points(x.out.os, y.out.os, pch=17, col="cyan", cex=.75)
legend("top", pch=c(16, 17), col=c("red", "blue"), legend=c("Outside CI Two-Sided", "Outside CI One-Sided"))
	
	Coverage.norm.ts 	<- In.norm.ts/(In.norm.ts + Out.norm.ts)
	Coverage.norm.os 	<- In.norm.os/(In.norm.os + Out.norm.os)
	Coverage.t.ts		<- In.t.ts/(In.t.ts + Out.t.ts)
	Coverage.t.os		<- In.t.os/(In.t.os + Out.t.os)
	Coverage.chisq.ts	<- In.chisq.ts/(In.chisq.ts + Out.chisq.ts)
	Coverage.chisq.os	<- In.chisq.os/(In.chisq.os + Out.chisq.os)
	
	res <- list(model=model,
				Coverage.norm.ts	= Coverage.norm.ts,
				Coverage.norm.os 	= Coverage.norm.os,
				Coverage.t.ts    	= Coverage.t.ts,
				Coverage.t.os    	= Coverage.t.os,
				Coverage.chisq.ts	= Coverage.chisq.ts,
				Coverage.chisq.os	= Coverage.chisq.os
				)
	
	return(res)
}

RefData1.0  <- get.RefData0(model=1,  Nsim=2e2)
RefData2.0  <- get.RefData0(model=2,  Nsim=2e4)
RefData3.0  <- get.RefData0(model=3,  Nsim=2e4)
RefData4.0  <- get.RefData0(model=4,  Nsim=2e4)
RefData5.0  <- get.RefData0(model=5,  Nsim=2e4, K=3)
RefData6.0  <- get.RefData0(model=6,  Nsim=2e4)
RefData7.0  <- get.RefData0(model=7,  Nsim=2e4)
RefData8.0  <- get.RefData0(model=8,  Nsim=2e4)
RefData9.0  <- get.RefData0(model=9,  Nsim=2e4)
RefData10.0 <- get.RefData0(model=10, Nsim=2e4)

#RefDat1 <- RefData1
#save(RefDat1, file="E:/TFS/R/Pkg_VFP/Main/VFP/data/RefData1.RData")
#RefDat2 <- RefData2
#save(RefDat2, file="E:/TFS/R/Pkg_VFP/Main/VFP/data/RefData2.RData")
#RefDat3 <- RefData3
#save(RefDat3, file="E:/TFS/R/Pkg_VFP/Main/VFP/data/RefData3.RData")
#RefDat4 <- RefData4
#save(RefDat4, file="E:/TFS/R/Pkg_VFP/Main/VFP/data/RefData4.RData")
#RefDat5 <- RefData5
#save(RefDat5, file="E:/TFS/R/Pkg_VFP/Main/VFP/data/RefData5.RData")
#RefDat6 <- RefData6
#save(RefDat6, file="E:/TFS/R/Pkg_VFP/Main/VFP/data/RefData6.RData")
#RefDat7 <- RefData7
#save(RefDat7, file="E:/TFS/R/Pkg_VFP/Main/VFP/data/RefData7.RData")
#RefDat8 <- RefData8
#save(RefDat8, file="E:/TFS/R/Pkg_VFP/Main/VFP/data/RefData8.RData")
#RefDat9 <- RefData9
#save(RefDat9, file="E:/TFS/R/Pkg_VFP/Main/VFP/data/RefData9.RData")

##Model 2:
#VC1 <- VC0*Mean^2
#VC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
#VC <-VC*VC1
#Daten2 <-data.frame(Mean,VC,DF)
#save(Daten2,file="C:/TFS/R/Pkg_VFP/Main/VFP/data/Test1_Daten2.RData")
#res <- fit.vfp(Data=Daten2,model=2,quiet=FALSE)
#res
##0.9108 
##AIC = 348.1  RSS = 5456  Deviance = 9.595 GoF P-value= 0.3842 
#x11()
#plot(res,model.no=2,type="vc")
#
##Model 3:
#VC1 <- VC0*(1+Mean^2)
#VC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
#VC <-VC*VC1
#Daten3 <-data.frame(Mean,VC,DF)
#save(Daten3,file="C:/TFS/R/Pkg_VFP/Main/VFP/data/Test1_Daten3.RData")
#res <- fit.vfp(Data=Daten3,model=3,quiet=FALSE)
#res
##B1     B2 
##1.8160 0.7681 
##AIC = 330.7  RSS = 3024  Deviance = 5.344 GoF P-value= 0.7203 
#x11()
#plot(res,model.no=3,type="vc")
#
##Model 4:
#VC1 <- VC0*(1+Mean)^2
#VC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
#VC <-VC*VC1
#Daten4 <-data.frame(Mean,VC,DF)
#save(Daten4,file="C:/TFS/R/Pkg_VFP/Main/VFP/data/Test1_Daten4.RData")
#res <- fit.vfp(Data=Daten4,model=4,quiet=FALSE)
#res
##beta1  beta2 
##1.4590 0.8475 
##AIC = 376.4  RSS = 3008  Deviance = 5.480 GoF P-value= 0.7053
#x11()
#plot(res,model.no=4,type="vc")
#
##Model 5:
#VC1 <- VC0*(1+Mean^3)
#VC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
#VC <-VC*VC1
#Daten5 <-data.frame(Mean,VC,DF)
#save(Daten5,file="C:/TFS/R/Pkg_VFP/Main/VFP/data/Test1_Daten5.RData")
#res <- fit.vfp(Data=Daten5,model=5,K=3,quiet=FALSE)
#res
##B1    B2 
##0.524 0.972 
##AIC = 458.0  RSS = 73440  Deviance = 2.856 GoF P-value= 0.943
#x11()
#plot(res,model.no=5,type="vc")
#
## Model 6: 
#DF   <- rep(1,10)*1000 # corresponds to 1000 replicas per point
#VC1   <- 1000-100*Mean+ Mean^3
#VC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
#VC <-VC*VC1
#Daten6 <-data.frame(Mean,VC,DF)
#save(Daten6,file="C:/TFS/R/Pkg_VFP/Main/VFP/data/Test1_Daten6.RData")
#res <- fit.vfp(Data=Daten6,model=6,quiet=FALSE)
#res
##beta1    beta2    beta3        J 
##976.800 -100.400    2.954    2.512 
##AIC = 47242  RSS = 6652  Deviance = 6.730 GoF P-value= 0.3465 
#x11()
#plot(res,model.no=6,type="vc")
#
##Model 7:
#DF   <- rep(1,10)*10 # corresponds to 1000 replicas per point
#VC1 <- VC0*(1+Mean^3)
#VC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
#VC <-VC*VC1
#Daten7 <-data.frame(Mean,VC,DF)
#save(Daten7,file="C:/TFS/R/Pkg_VFP/Main/VFP/data/Test1_Daten7.RData")
#res <- fit.vfp(Data=Daten7,model=7,quiet=FALSE)
#res
##beta1    beta2        J 
##"1.246" "0.6709"  "3.270" 
##AIC = 460.3  RSS = 57859  Deviance = 2.256 GoF P-value= 0.9443 
#x11()
#plot(res,model.no=7,type="vc")
#
##Model 8:
#VC1 <- VC0*(1+Mean)^3
#VC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
#VC <-VC*VC1
#Daten8 <-data.frame(Mean,VC,DF)
#save(Daten8,file="C:/TFS/R/Pkg_VFP/Main/VFP/data/Test1_Daten8.RData")
#res <- fit.vfp(Data=Daten8,model=8,quiet=FALSE)
#res
##beta1     beta2         J 
##2.396e-07 2.258e+00 2.296e+00 
##AIC = 577.3  RSS = 433349  Deviance = 6.637 GoF P-value= 0.4677 
#x11()
#plot(res,model.no=8,type="vc")
##Model 9:
#VC1 <- VC0*Mean^3
#VC <- as.numeric(lapply(DF, function(x) {return(rchisq(df=x,1)/x)}))
#VC <-VC*VC1
#Daten9 <-data.frame(Mean,VC,DF)
#save(Daten9,file="C:/TFS/R/Pkg_VFP/Main/VFP/data/Test1_Daten9.RData")
#res <- fit.vfp(Data=Daten9,model=9,quiet=FALSE)
#res
##beta1       J 
##"1.010" "3.083" 
##AIC = 521.6  RSS = 327190  Deviance = 9.080 GoF P-value= 0.3356 
#x11()
#plot(res,model.no=9,type="vc")
