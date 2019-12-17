## ----global_options, echo=FALSE, eval=TRUE------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=7, fig.align='center', fig.path='figures/',
                      echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE)

## ----Precision_Table, echo=FALSE----------------------------------------------
library(VFP)
library(VCA)
library(knitr)
# CLSI EP05-A3 example data
data(CA19_9)
# fit VCA-model
VCA.CA19_9 <- anovaVCA(result~site/day, CA19_9, by="sample")
# create VCA-result table on CV-scale
CV.tab <- t(sapply(VCA.CA19_9, function(x) round(x$aov.tab[, "CV[%]"], 1)))
CV.tab <- cbind(sapply(VCA.CA19_9, function(x) signif(x$Mean, 4)), CV.tab)
colnames(CV.tab) <- c("Mean", "Reproducibility %CV", "Between-Site %CV", "Between-Day %CV", "Repeatability %CV")
kable(CV.tab, caption="Precision performance table as usually included in package insert.")

## ----Precision_Profile, echo=FALSE--------------------------------------------
VFP.CA19_9 <- fit.vfp(VCA.CA19_9, model.no=4)
plot(VFP.CA19_9, type="cv", ylim=c(0, 12))

## ----perform_VCA_1, echo=TRUE-------------------------------------------------
library(VFP)
library(VCA)
# CLSI EP05-A3 example data
data(CA19_9)

## ----perform_VCA_2, echo=TRUE-------------------------------------------------
VCA.CA19_9 <- anovaVCA(result~site/day, CA19_9, by="sample")

## ----perform_VCA_3, echo=TRUE-------------------------------------------------
class(VCA.CA19_9)
sapply(VCA.CA19_9, class)
print(VCA.CA19_9[[1]], digits=4)

## ----extract_Info_1, echo=TRUE------------------------------------------------
mat.total <- getMat.VCA(VCA.CA19_9)
print(mat.total)

## ----extract_Info_2, echo=TRUE------------------------------------------------
mat.rep <- getMat.VCA(VCA.CA19_9, "error")
print(mat.rep)

## ----extract_Info_3, echo=TRUE------------------------------------------------
mat.ip <- getMat.VCA(VCA.CA19_9, 3:4)
print(mat.ip)

## ----fit_VFP_model_1, echo=TRUE-----------------------------------------------
 tot.m1 <- fit.vfp(mat.total, model.no=1)

## ----fit_VFP_model_2, echo=TRUE-----------------------------------------------
tot.m1

## ----fit_VFP_model_3, echo=TRUE-----------------------------------------------
 tot.all <- fit.vfp(mat.total, 1:10)
 # 'summary' presents more details for multi-model objects 
 summary(tot.all)

## ----plot_VFP_model_VC, echo=TRUE---------------------------------------------
# not selecting a specific model automatically chooses the one with lowest AIC
plot(tot.all)

## ----plot_VFP_model_CV, echo=TRUE---------------------------------------------
# selecting one specific model is easy
plot(tot.all, model.no=7, type="cv", ylim=c(0, 10))

## ----CLSI_EP05_Example_1, echo=TRUE-------------------------------------------
plot(	tot.all, model.no=8, mar=c(5.1, 5, 4.1, 8), 
		type="cv", ci.type="none", Model=FALSE,
		Line=list(col="blue", lwd=3), 
		Points=list(pch=15, col="blue", cex=1.5),  
		xlim=c(10, 450), ylim=c(0,10),
		Xlabel=list(text="CA19-9, kU/L (LogScale) - 3 Patient Pools, 3 QC Materials",
				cex=1.25), Title=NULL,
		Ylabel=list(text="% CV", cex=1.25, line=3),
		Grid=NULL, Crit=NULL, log="x")

# We now add the precision profile for intermediate precision
# to the existing plot (add=TRUE).

mod8.ip <- fit.vfp(mat.ip, 8)
plot(mod8.ip, type="cv", add=TRUE, ci.type="none",
	 Points=list(pch=16, col="deepskyblue", cex=1.5),
	 Line=list(col="deepskyblue", lwd=3), log="x")

# Now, add the precision profile of repeatability.

mod8.rep <- fit.vfp(mat.rep, 8)
plot(mod8.rep, type="cv", add=TRUE, ci.type="none",
		Points=list(pch=17, col="darkorchid3", cex=1.5),
		Line=list(col="darkorchid3", lwd=3), log="x")

# Finally, add a legend in the right margin.

legend.rm( x="center", pch=15:17, col=c("blue", "deepskyblue", "darkorchid3"),
		cex=1, legend=c("Reproducibility", "Within-Lab", "Repeatability"),
		box.lty=0)

## ----CLSI_EP05_Example_2, echo=TRUE-------------------------------------------
plot(mod8.rep, BG="darkgray", 
		Points=list(pch=17, cex=1.5, col="blue"), Line=list(col="blue"),
		Grid=list(x=seq(0, 450, 50), y=seq(0, 110, 10), col="white"),
		Xlabel=list(cex=1.5, text="CA19-9 [U/mL]", col="blue"),
		Ylabel=list(cex=1.5, text="Repeatability on Variance-Scale", col="blue"),
		Crit=NULL)		

## ----beautified_with_prediction_1, echo=TRUE----------------------------------
plot(mod8.rep, type="cv", ylim=c(0, 8), Prediction=5)

## ----beautified_with_prediction_2, echo=TRUE----------------------------------
plot(mod8.rep, type="cv", ylim=c(0, 8), xlim=c(0, 100), Prediction=5, Pred.CI=TRUE)

## ----beautified_with_prediction_3, echo=TRUE----------------------------------
plot(mod8.rep, type="cv", ylim=c(0, 8), xlim=c(0, 100), Prediction=list(x=50), Pred.CI=TRUE)

## ----deriveCx_examples_1, echo=TRUE-------------------------------------------
deriveCx(mod8.rep, cutoff=40, start=35, Cx=0.05)

## ----deriveCx_examples_2, echo=TRUE-------------------------------------------
deriveCx(mod8.rep, cutoff=40, start=35, Cx=0.05, plot=TRUE)

## ----deriveCx_examples_3, echo=TRUE-------------------------------------------
deriveCx(mod8.rep, cutoff=40, start=35, Cx=0.95, plot=TRUE)

## ----deriveCx_examples_4, echo=TRUE-------------------------------------------
res <- precisionPlot(mod8.rep, cutoff=c(35, 40), prob=c(.05, .95), alpha2=.5) 

## ----deriveCx_examples_5, echo=TRUE-------------------------------------------
print(str(res))
head(res[[1]])

