###############################################################################
#
# TODO: Add comment
# 
# Author: schueta6
#
###############################################################################


#' Compare Two Graphics-Files Pixel-Based With Optional User-Interaction.
#' 
#' This function compares two graphics files based on R-package magickt. It allows to
#' specify an allowed deviation as fraction and has the option to let the user decide
#' whether a detected difference is tolerable or not. This is necessary as sometimes
#' there are difference in plots if they were generated with two different R-versions.
#' 
#' @param reference          	(character) path to the reference image file
#' @param test                	(character) path to the image file to be compared
#' @param dev.allowed        	(numeric) value within [0;1] indicating the proportion of 
#'                            	pixels tolerable to still accept the comparison
#' @param user.check        	(logical) TRUE = in case of non-identical image files the
#'                             	user has to possiblity to visually inspect the difference
#'                             	and decide whether or not the check is successful
#' @param quiet                	(logical) TRUE = informative output is allowed
#' @param dir               	(character) path to the directory to be used, e.g. where
#'                            	 the temporary comparison result file is stored
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @examples 
#' \dontrun{
#' # example borrowed from https://www.stat.auckland.ac.nz/~paul/Reports/QA/gdiff/gdiff.html#tolerance
#' # create two image files to compare
#' library(grid)
#' image1 <- function() {
#'     grid.rect()
#'     grid.text("same", 1/3, 2/3)
#' }
#' image2 <- function() {
#'     grid.rect()
#'     grid.text(c("same", "different"), 1:2/3, 2:1/3)
#' }
#' ## Generate control image
#' png("control-image.png", width=100, height=100)
#' image1()
#' dev.off()
#' ## Generate test image
#' png("test-image.png", width=100, height=100)
#' image2()
#' dev.off()
#' # perform test without user-assessment
#' gtest("control-image.png", "test-image.png")
#' # with user-assessment
#' gtest("control-image.png", "test-image.png", user.check=TRUE)
#' # clean-up afterwards
#' unlink("control-image.png", force=TRUE)
#' unlink("test-image.png", force=TRUE)
#' }

gtest <- function(reference=NULL, test=NULL, dev.allowed=0, user.check=FALSE, quiet=FALSE, dir=".") {
	### load required packages
	library(magick)
	library(png)
	stopifnot(file.exists(reference))
	stopifnot(file.exists(test))
	stopifnot(is.numeric(dev.allowed))
	stopifnot(dev.allowed >= 0 && dev.allowed <= 1)
	robj     <- image_read(reference)
	rinfo    <- image_info(robj)
	rinfo    <- rinfo[-which(names(rinfo) == "filesize")]
	tobj     <- image_read(test)
	tinfo    <- image_info(tobj)
	tinfo    <- tinfo[-which(names(tinfo) == "filesize")]
	# check meta-data of both graphics files, return failure if no user-check was allowed
	if(!identical(rinfo, tinfo) && !user.check) {
		if(!quiet)
			cat(paste0("\nMeta-data differ for '", reference, "' (ref) and '", test, "' (test)"))
		return(FALSE)
	}
	dev.pix <- 0                        # allowed number of deviating pixels
	if(dev.allowed > 0) {                # derive number of pixels allowed to be different
		dev.pix <- ceiling(dev.allowed * rinfo$width * rinfo$height)
	}
	# start comparison on pixel-level
	cobj <- image_compare(robj, tobj, metric="AE")
	difr <- attr(cobj, "distortion")    # number of pixel not identical
	temp <- paste0(tempfile(tmpdir=dir), ".png")
	on.exit(unlink(temp, force=TRUE))    # delete temp file on exit
	cmpr <- image_write(cobj, temp)
	if(difr > dev.pix) {
		if(user.check) {
			img <- readPNG(temp)
			grid::grid.raster(img)
			cat("\nAre detected differences tolerable [y/n]?: ")
			dec <- scan(what="character", nmax=1)
			dev.off()
			if(tolower(dec) == "y") {
				
				return(TRUE)
			} else
				return(FALSE)
		} else {
			return(FALSE)
		}
	}
	ret <- TRUE
	attr(ret, "PixelDifference") <- c(Detected=difr, Allowed=dev.pix)
	return(ret)
}
