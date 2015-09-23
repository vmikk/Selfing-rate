# Calculate accuracy of COLONY simulation
# License: GPL-2 
# Author: Vladimir Mikryukov, June 2015


library(plyr)
library(xlsx)

## Set working directory
# setwd("...")

# Working directory should contain *.Accuracy files from COLONY simulations
# Example file: COLONY_simulation_Accuracy.Accuracy


####################################
####################################	# Load data
####################################

# Read data
filenames <- list.files(pattern="*.Accuracy$", full.names=TRUE)			# results from analysis

# parse COLONY output
read.accuracy <- function(x){	# x = "*.Accuracy" file
	# parse data	
	saf <- read.table(x, blank.lines.skip=F, skip = 1, nrows = 3)		# Sibship Accuracy (FL/PLS/FPLS method)
	sap <- read.table(x, blank.lines.skip=F, skip = 7, nrows = 3)		# Sibship Accuracy (Pairwise method)
	paf <- read.table(x, blank.lines.skip=F, skip = 13, nrows = 2)		# Parentage Accuracy (FL/PLS/FPLS method)
	pap <- read.table(x, blank.lines.skip=F, skip = 18, nrows = 2)		# Parentage Accuracy (Pairwise method)
	so <- read.table(x, skip = 23, comment.char="")						# Self-outbred Individual Accuracy (FL/PLS/FPLS method)

	rownames(saf) <- rownames(sap) <- gsub(pattern = "\\(True)", replacement = "", x = rownames(saf))

	rownames(so) <- gsub(pattern = "#", replacement = "", x = rownames(so))
	rownames(so) <- gsub(pattern = "\\(", replacement = "", x = rownames(so))
	rownames(so) <- gsub(pattern = ")", replacement = "", x = rownames(so))

	rename.col <- function(z){
		colnames(z) <- rownames(z)
		return(z)
	}

	saf <- rename.col(saf)
	sap <- rename.col(sap)
	paf <- rename.col(paf)
	pap <- rename.col(pap)
	so <- rename.col(so)

	res <- list(saf, sap, paf, pap, so)
	names(res) <- c("saf", "sap", "paf", "pap", "so")
	return(res)
}


# read all accuracy files
acc <- lapply(filenames, read.accuracy)
	nms <- gsub(pattern = ".Accuracy", replacement = "", x = filenames)
	nms <- gsub(pattern = "./", replacement = "", x = nms)
	names(acc) <- nms



####################################
####################################	# Estimate accuracy
####################################

# Acuracy for selfing-outcrossing part of the output
self.accuracy <- function(x){		# x = accuracy list for one meadow
	so <- x$so  					# extract selfing-outcrossing results only (Self-outbred Individual Accuracy)
	TP <- so["Self", "Self"]				# True positives
	FP <- so["Out", "Self"]					# False positives
	FN <- so["Self", "Out"]					# False negatives
	TN <- so["Out", "Out"]					# True negatives
	pos <- sum(so["Self", ]	)				# P (total positives)
	neg <- sum(so["Out", ])					# N (total negatives)
	res <- data.frame(
		TotSelf = pos,						# Total number of 'real' selfers
		TP.rate = TP / pos,					# True positive rate = TP / P = Sensitivity = recall
		FP.rate = FP / neg,					# False positive rate = FP / N
		Precision = TP / (TP + FP),			# Precision = positive predictive value
		Accuracy = (TP + TN) / (pos + neg),	# Accuracy
		Specificity = TN / (FP + TN)		# Specificity = 1 - FP.rate
		)
	res$F.measure <- with(res, 2 / (1/Precision + 1/TP.rate))		# F-measure
	return(res)
}



# Batch calculation of accuracy
acc.Slf <- ldply(.data = acc, .fun = self.accuracy, .id = "SimulationNum")

write.xlsx(acc.Slf, file="Meadows_SimulAccuracy.xlsx", sheetName="Selfers", append=TRUE)
