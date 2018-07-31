# TODO: Add comment
# 
# Author: schueta6
###############################################################################



library(VFP)
library(RUnit)

options(warn=1)

# test function regexpr fits to string "TFxyz" which are used as identifiers for easier referencing

testSuite <- defineTestSuite(
								name="VFP", dirs=".",
								testFileRegexp="runit.*\\.R$",
								testFuncRegexp = "^TF[[:digit:]]{3}.+",					# use custom regexpr for test functions
								rngKind="default",
								rngNormalKind="default")

testData <- runTestSuite(testSuite, verbose=0L)

sInfo <- sessionInfo()
cat("Test Summary Report R-Package VFP", paste("V", sInfo$otherPkgs[["VFP"]]$Version, sep=""), file="./VFP_UnitTest_Protocol.txt", append=FALSE)
cat("\n-------------------------------------", file="./VFP_UnitTest_Protocol.txt", append=TRUE )
cat("\n\n\n1) Package Description:", file="./VFP_UnitTest_Protocol.txt", append=TRUE)
cat("\n-----------------------\n\n", file="./VFP_UnitTest_Protocol.txt", append=TRUE)
capture.output(print(sInfo$otherPkgs[["VFP"]]), file="./VFP_UnitTest_Protocol.txt", append=TRUE)
cat("\n\n\n2) Test Environment:", file="./VFP_UnitTest_Protocol.txt", append=TRUE)
cat("\n--------------------\n", file="./VFP_UnitTest_Protocol.txt", append=TRUE)
sinfo <- Sys.info()
snam <- names(sinfo)
for(i in 1:length(sinfo))
{
	cat(paste("\n", snam[i], paste(rep(" ", 20-nchar(snam[i])), collapse=""), sep=""),":\t", sinfo[i], file="./VFP_UnitTest_Protocol.txt", append=TRUE)
}

cat("\n\n\n\n3) Test Protocol:", file="./VFP_UnitTest_Protocol.txt", append=TRUE)
cat("\n-----------------\n\n", file="./VFP_UnitTest_Protocol.txt", append=TRUE)


cat("\n\n\n")
printTextProtocol(testData, showDetails=FALSE)
capture.output(printTextProtocol(testData, showDetails=TRUE), file="./VFP_UnitTest_Protocol.txt", append=TRUE)
printHTMLProtocol(testData, file="./VFP_UnitTest_Protocol.html")

options(warn=0)

shell.exec("VFP_UnitTest_Protocol.html")
