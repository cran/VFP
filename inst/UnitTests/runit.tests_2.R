cat("\n\n###############################################################################\n\n")
cat("Executing unit-tests defined in runit.tests_2.R\n\n")

library(RUnit)
library(VFP)



########################   Bla99T2.Rdata   ################################
load("./data/Bla99T2.Rdata")
fit.all.models.Bla99T2  <- fit.vfp(Bla99T2, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF001.Bla99T2_model1 <- function(x){
  ref.coef <- c(45.755959596) 
  ref.deviance <- 14.07743
  
  tst.coef <- as.numeric(fit.all.models.Bla99T2$Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T2$Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF002.Bla99T2_model2 <- function(x){
  ref.coef <- c(0.0051135422) 
  ref.deviance <- 10.128
  
  tst.coef <- as.numeric(fit.all.models.Bla99T2$Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T2$Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 0.001)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF003.Bla99T2_model3 <- function(x){
  ref.coef <- c(-0.65988688, 0.0051945091) 
  ref.deviance <- 10.12739
  
  tst.coef <- as.numeric(fit.all.models.Bla99T2$model3$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T2$Model$model3$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF004.Bla99T2_model4 <- function(x){
  ref.coef <- c(0.167871539, 0.069674699) 
  ref.deviance <- 10.12552
  
  tst.coef <- as.numeric(fit.all.models.Bla99T2$Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T2$Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF005.Bla99T2_model7 <- function(x){
  er.max <- 1.24
  ref.coef <- c(-1160.060918, 910.3160206, 0.0620513) 
  ref.deviance <- 9.68988
  
  tst.coef <- as.numeric(fit.all.models.Bla99T2$Model$model7$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T2$Model$model7$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkTrue(er < er.max)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF006.Bla99T2_model8 <- function(x){
  er.max <- 1.24
  ref.coef <- c(-219.4576215, 4.261460506, 0.73827724) 
  ref.deviance <- 9.69371
  
  tst.coef <- as.numeric(fit.all.models.Bla99T2$Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T2$Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkTrue(er < er.max)
}

########################   Bla99T3.Rdata   ################################
load("./data/Bla99T3.Rdata")
fit.all.models.Bla99T3  <- fit.vfp(Bla99T3, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF007.Bla99T3_model1 <- function(x){
  ref.coef <- c(0.0038102273) 
  ref.deviance <- 55.73716
  
  tst.coef <- as.numeric(fit.all.models.Bla99T3$Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T3$Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}


#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF008.Bla99T3_model2 <- function(x){
  ref.coef <- c(0.0008287602) 
  ref.deviance <- 68.79862
  
  tst.coef <- as.numeric(fit.all.models.Bla99T3$Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T3$Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF009.Bla99T3_model3 <- function(x){
  ref.coef <- c(0.0021892012, 0.0001768916) 
  ref.deviance <- 53.4067
  
  tst.coef <- as.numeric(fit.all.models.Bla99T3$Model$model3$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T3$Model$model3$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF010.Bla99T3_model4 <- function(x){
  ref.coef <- c(0.0406663558, 0.0072922437) 
  ref.deviance <- 53.60044
  
  tst.coef <- as.numeric(fit.all.models.Bla99T3$Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T3$Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling
TF011.Bla99T3_model7 <- function(x){
  ref.coef <- c(0.0015066196, 0.0008165254, 1) 
  ref.deviance <- 53.71046
  
  tst.coef <- as.numeric(fit.all.models.Bla99T3$Model$model7$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T3$Model$model7$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = .17)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF012.Bla99T3_model8 <- function(x){
  ref.coef <- c(0.6951111279, 0.0102846776, 17.3648831) 
  ref.deviance <- 53.55989
  
  tst.coef <- as.numeric(fit.all.models.Bla99T3$Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Bla99T3$Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 0.001)
}

########################   Haw02Cyc.Rdata   ################################
load("./data/Haw02Cyc.Rdata")
fit.all.models.Haw02Cyc  <- fit.vfp(Haw02Cyc, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF013.Haw02Cyc_model1 <- function(x){
  ref.coef <- c(2115.1875) 
  ref.deviance <- 97.65446
  
  tst.coef <- as.numeric(fit.all.models.Haw02Cyc$Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Cyc$Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF014.Haw02Cyc_model2 <- function(x){
  ref.coef <- c(0.0247577331) 
  ref.deviance <- 61.99071
  
  tst.coef <- as.numeric(fit.all.models.Haw02Cyc$Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Cyc$Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF015.Haw02Cyc_model3 <- function(x){
  ref.coef <- c(-30.35831262, 0.0263252929) 
  ref.deviance <- 61.09947
  
  tst.coef <- as.numeric(fit.all.models.Haw02Cyc$Model$model3$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Cyc$Model$model3$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF016.Haw02Cyc_model4 <- function(x){
  ref.coef <- c(1.2732010329, 0.1501886939) 
  ref.deviance <- 61.96624
  
  tst.coef <- as.numeric(fit.all.models.Haw02Cyc$Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Cyc$Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF017.Haw02Cyc_model7 <- function(x){
  er.max <- 2.212	
	
  ref.coef <- c(-65.98056356, 0.2069745554, 1.62071241) 
  ref.deviance <- 60.02054
  
  tst.coef <- as.numeric(fit.all.models.Haw02Cyc$Model$model7$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Cyc$Model$model7$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkTrue(er < er.max)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF018.Haw02Cyc_model8 <- function(x){
  er.max <- 2.13
  ref.coef <- c(-22.68001814, 0.6980432357, 1.46449996) 
  ref.deviance <- 60.09694
  
  tst.coef <- as.numeric(fit.all.models.Haw02Cyc$Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Cyc$Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkTrue(er < er.max)
}


########################   Haw02Dig.Rdata   ################################
load("./data/Haw02Dig.Rdata")
fit.all.models.Haw02Dig  <- fit.vfp(Haw02Dig, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF019.Haw02Dig_model1 <- function(x){
  ref.coef <- c(0.0414552239) 
  ref.deviance <- 263.66121
  
  tst.coef <- as.numeric(fit.all.models.Haw02Dig$Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Dig$Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF020.Haw02Dig_model2 <- function(x){
  ref.coef <- c(0.074190407) 
  ref.deviance <- 396.55144
  
  tst.coef <- as.numeric(fit.all.models.Haw02Dig$Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Dig$Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF021.Haw02Dig_model3 <- function(x){
  ref.coef <- c(0.0059156862, 0.0059309579) 
  ref.deviance <- 178.94931
  
  tst.coef <- as.numeric(fit.all.models.Haw02Dig$Model$model3$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Dig$Model$model3$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF022.Haw02Dig_model4 <- function(x){
  ref.coef <- c(0.0650495966, 0.0588691986) 
  ref.deviance <- 189.15385
  
  tst.coef <- as.numeric(fit.all.models.Haw02Dig$Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Dig$Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF023.Haw02Dig_model7 <- function(x){
  ref.coef <- c(0.0060378902, 0.0002940633, 4.91478336) 
  ref.deviance <- 159.70102
  
  tst.coef <- as.numeric(fit.all.models.Haw02Dig$Model$model7$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Dig$Model$model7$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF024.Haw02Dig_model8 <- function(x){
  ref.coef <- c(3.9402060761, -0.62299021, -3.877375) 
  ref.deviance <- 173.6397
  
  tst.coef <- as.numeric(fit.all.models.Haw02Dig$Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02Dig$Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}


########################   Haw02E2.Rdata   ################################
load("./data/Haw02E2.Rdata")
fit.all.models.Haw02E2  <- fit.vfp(Haw02E2, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF025.Haw02E2_model1 <- function(x){
  ref.coef <- c(149616.15493) 
  ref.deviance <- 586.51188
  
  tst.coef <- as.numeric(fit.all.models.Haw02E2$Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02E2$Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF026.Haw02E2_model2 <- function(x){
  ref.coef <- c(0.1384203063) 
  ref.deviance <- 375.46942
  
  tst.coef <- as.numeric(fit.all.models.Haw02E2$Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02E2$Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF027.Haw02E2_model3 <- function(x){
  ref.coef <- c(1088.5142822, 0.0120172645) 
  ref.deviance <- 225.16906
  
  tst.coef <- as.numeric(fit.all.models.Haw02E2$Model$model3$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02E2$Model$model3$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF028.Haw02E2_model4 <- function(x){
  ref.coef <- c(20.9764583, 0.1031450534) 
  ref.deviance <- 213.09684
  
  tst.coef <- as.numeric(fit.all.models.Haw02E2$Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02E2$Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF029.Haw02E2_model8 <- function(x){
  ref.coef <- c(25.155070487, 0.4431519728, 1.61745554) 
  ref.deviance <- 210.86539
  
  tst.coef <- as.numeric(fit.all.models.Haw02E2$Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Haw02E2$Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

########################   Haw02VitD.Rdata   ################################
load("./data/Haw02VitD.Rdata")
fit.all.modelsHaw02VitD  <- fit.vfp(Haw02VitD, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF030.Haw02VitD_model1 <- function(x){
  ref.coef <- c(10.918787879) 
  ref.deviance <- 50.71256
  
  tst.coef <- as.numeric(fit.all.modelsHaw02VitD$Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.modelsHaw02VitD$Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF031.Haw02VitD_model2 <- function(x){
  ref.coef <- c(0.0129357041) 
  ref.deviance <- 42.24244
  
  tst.coef <- as.numeric(fit.all.modelsHaw02VitD$Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.modelsHaw02VitD$Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF032.Haw02VitD_model3 <- function(x){
  ref.coef <- c(1.4410526661, 0.005558244) 
  ref.deviance <- 33.23147
  
  tst.coef <- as.numeric(fit.all.modelsHaw02VitD$Model$model3$coefficients)
  tst.deviance <- as.numeric(fit.all.modelsHaw02VitD$Model$model3$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF033.Haw02VitD_model4 <- function(x){
  ref.coef <- c(1.0254320388, 0.0499417667) 
  ref.deviance <- 32.389
  
  tst.coef <- as.numeric(fit.all.modelsHaw02VitD$Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.modelsHaw02VitD$Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF034.Haw02VitD_model7 <- function(x){
  ref.coef <- c(0.6126662286, 0.091540333, 1.24465804) 
  ref.deviance <- 31.40406
  
  tst.coef <- as.numeric(fit.all.modelsHaw02VitD$Model$model7$coefficients)
  tst.deviance <- as.numeric(fit.all.modelsHaw02VitD$Model$model7$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF035.Haw02VitD_model8 <- function(x){
  ref.coef <- c(0.4715051948, 0.1464954057, 1.2265934) 
  ref.deviance <- 31.44783
  
  tst.coef <- as.numeric(fit.all.modelsHaw02VitD$Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.modelsHaw02VitD$Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

########################   Sad19aTSH.Rdata   ################################
load("./data/Sad19aTSH.Rdata")
fit.all.models.Sad19aTSH  <- fit.vfp(Sad19aTSH, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF036.Sad19aTSH_model1 <- function(x){
  ref.coef <- c(0.087833552) 
  ref.deviance <- 905.74602
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH $Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH $Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF037.Sad19aTSH_model2 <- function(x){
  ref.coef <- c(0.0071021411) 
  ref.deviance <- 303.36766
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH $Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH $Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF038.Sad19aTSH_model4 <- function(x){
  ref.coef <- c(0.0022973721, 0.0267860427) 
  ref.deviance <- 173.60248
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH $Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH $Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF039.Sad19aTSH_model8 <- function(x){
  ref.coef <- c(0.0158803534, 0.0439737666, 2.82998506) 
  ref.deviance <- 152.99512
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH $Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH $Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}


########################   Sad19aTSH1.Rdata   ################################
load("./data/Sad19aTSH1.Rdata")
fit.all.models.Sad19aTSH1  <- fit.vfp(Sad19aTSH1, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF040.Sad19aTSH1_model1 <- function(x){
  ref.coef <- c(1.0807775473) 
  ref.deviance <- 801.9527
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH1$Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH1$Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF041.Sad19aTSH1_model2 <- function(x){
  ref.coef <- c(0.0219208018) 
  ref.deviance <- 50.42702
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH1$Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH1$Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF042.Sad19aTSH1_model3 <- function(x){
  ref.coef <- c(0.0000096947, 0.0177853337) 
  ref.deviance <- 46.1748
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH1$Model$model3$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH1$Model$model3$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF043.Sad19aTSH1_model4 <- function(x){
  ref.coef <- c(0.0011837363, 0.1306693436) 
  ref.deviance <- 46.30418
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH1$Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH1$Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF044.Sad19aTSH1_model8 <- function(x){
  ref.coef <- c(0.001647968, 0.1337031117, 2.03893835) 
  ref.deviance <- 46.17093
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH1$Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH1$Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}


########################   Sad19aTSH2.Rdata   ################################
load("./data/Sad19aTSH2.Rdata")
fit.all.models.Sad19aTSH2  <- fit.vfp(Sad19aTSH2, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF045.Sad19aTSH2_model1 <- function(x){
  ref.coef <- c(0.0882737295) 
  ref.deviance <- 560.74508
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH2$Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH2$Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF046.Sad19aTSH2_model2 <- function(x){
  ref.coef <- c(0.0714091362) 
  ref.deviance <- 285.14888
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH2$Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH2$Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF047.Sad19aTSH2_model3 <- function(x){
  ref.coef <- c(0.0003018872, 0.0010140696) 
  ref.deviance <- 63.58781
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH2$Model$model3$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH2$Model$model3$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF048.Sad19aTSH2_model4 <- function(x){
  ref.coef <- c(0.0151241403, 0.0291928309) 
  ref.deviance <- 69.46337
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH2$Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH2$Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF049.Sad19aTSH2_model8 <- function(x){
  ref.coef <- c(0.1018231661, 0.041224666, 3.57616868) 
  ref.deviance <- 57.28004
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH2$Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH2$Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}


########################   Sad19aTSH3.Rdata   ################################
load("./data/Sad19aTSH3.Rdata")
fit.all.models.Sad19aTSH3  <- fit.vfp(Sad19aTSH3, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF050.Sad19aTSH3_model1 <- function(x){
  ref.coef <- c(0.9781874849) 
  ref.deviance <- 708.25334
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH3$Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH3$Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF051.Sad19aTSH3_model2 <- function(x){
  ref.coef <- c(0.0749990176) 
  ref.deviance <- 122.36382
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH3$Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH3$Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF052.Sad19aTSH3_model4 <- function(x){
  ref.coef <- c(0.0098485209, 0.1279792569) 
  ref.deviance <- 56.1549
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH3$Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH3$Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF053.Sad19aTSH3_model6 <- function(x){
  ref.coef <- c(0.000512115, -0.0063361, 0.0228917357, 2.04861637) 
  ref.deviance <- 21.59875
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH3$Model$model6$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH3$Model$model6$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF054.Sad19aTSH3_model7 <- function(x){
  ref.coef <- c(0.0002339351, 0.0105866197, 2.44566686) 
  ref.deviance <- 36.6157
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH3$Model$model7$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH3$Model$model7$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF055.Sad19aTSH3_model8 <- function(x){
  ref.coef <- c(0.0362277021, 0.144508591, 2.65060704) 
  ref.deviance <- 44.77763
  
  tst.coef <- as.numeric(fit.all.models.Sad19aTSH3$Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.models.Sad19aTSH3$Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}


########################   sPCaEP5bR.Rdata   ################################
load("./data/sPCaEP5bR.Rdata")
fit.all.models.sPCaEP5bR  <- fit.vfp(sPCaEP5bR, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF056.sPCaEP5bR_model1 <- function(x){
  ref.coef <- c(0.0647434429) 
  ref.deviance <- 13.22457
  
  tst.coef <- as.numeric(fit.all.models.sPCaEP5bR$Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.models.sPCaEP5bR$Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF057.sPCaEP5bR_model2 <- function(x){
  ref.coef <- c(0.0076510644) 
  ref.deviance <- 1.91713
  
  tst.coef <- as.numeric(fit.all.models.sPCaEP5bR$Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.models.sPCaEP5bR$Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF058.sPCaEP5bR_model3 <- function(x){
  ref.coef <- c(0.0005025073, 0.0037223301) 
  ref.deviance <- 0.17905
  
  tst.coef <- as.numeric(fit.all.models.sPCaEP5bR$Model$model3$coefficients)
  tst.deviance <- as.numeric(fit.all.models.sPCaEP5bR$Model$model3$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF059.sPCaEP5bR_model4 <- function(x){
  ref.coef <- c(0.0143831119, 0.0545680885) 
  ref.deviance <- 0.03352
  
  tst.coef <- as.numeric(fit.all.models.sPCaEP5bR$Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.models.sPCaEP5bR$Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF060.sPCaEP5bR_model8 <- function(x){
  ref.coef <- c(0.0079232277, 0.0489715118, 1.83243937) 
  ref.deviance <- 0.01172
  
  tst.coef <- as.numeric(fit.all.models.sPCaEP5bR$Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.models.sPCaEP5bR$Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

########################   sPCaEP5bT.Rdata   ################################
load("./data/sPCaEP5bT.Rdata")
fit.all.models.sPCaEP5bT  <- fit.vfp(sPCaEP5bT, model.no=1:8)

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF061.sPCaEP5bT_model1 <- function(x){
  ref.coef <- c(0.1269853629) 
  ref.deviance <- 13.508
  
  tst.coef <- as.numeric(fit.all.models.sPCaEP5bT $Model$model1$coefficients)
  tst.deviance <- as.numeric(fit.all.models.sPCaEP5bT $Model$model1$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF062.sPCaEP5bT_model2 <- function(x){
  ref.coef <- c(0.0137888083) 
  ref.deviance <- 1.60816
  
  tst.coef <- as.numeric(fit.all.models.sPCaEP5bT $Model$model2$coefficients)
  tst.deviance <- as.numeric(fit.all.models.sPCaEP5bT $Model$model2$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF063.sPCaEP5bT_model3 <- function(x){
  ref.coef <- c(0.0007774, 0.0074151281) 
  ref.deviance <- 0.13659
  
  tst.coef <- as.numeric(fit.all.models.sPCaEP5bT $Model$model3$coefficients)
  tst.deviance <- as.numeric(fit.all.models.sPCaEP5bT $Model$model3$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF064.sPCaEP5bT_model4 <- function(x){
  ref.coef <- c(0.0174274044, 0.0777798879) 
  ref.deviance <- 0.02702
  
  tst.coef <- as.numeric(fit.all.models.sPCaEP5bT $Model$model4$coefficients)
  tst.deviance <- as.numeric(fit.all.models.sPCaEP5bT $Model$model4$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-5)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF065.sPCaEP5bT_model7 <- function(x){
  ref.coef <- c(0.0004561167, 0.0096303292, 1.75366787) 
  ref.deviance <- 0.00225
  
  tst.coef <- as.numeric(fit.all.models.sPCaEP5bT $Model$model7$coefficients)
  tst.deviance <- as.numeric(fit.all.models.sPCaEP5bT $Model$model7$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

#* **target fit.vfp
#* **riskid RA01
#* **funid  FUN101
#* **desc 	Freely available test data in the field of precision profiling

TF066.sPCaEP5bT_model8 <- function(x){
  ref.coef <- c(0.0099364899, 0.0720222881, 1.84331304) 
  ref.deviance <- 0.00534
  
  tst.coef <- as.numeric(fit.all.models.sPCaEP5bT $Model$model8$coefficients)
  tst.deviance <- as.numeric(fit.all.models.sPCaEP5bT $Model$model8$deviance)
  
  er <- exp((tst.deviance - ref.deviance)/2)
  checkEquals(er, 1, tolerance = 1E-6)
}

##########################################################################################################





