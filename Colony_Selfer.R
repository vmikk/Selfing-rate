## Parse relsults from COLONY simulation
## Selfing probability results are in the output file "*.Selfer"
# License: GPL-2 
# Author: Vladimir Mikryukov, June 2015


## Set working directory
# setwd("...")


datt <- read.csv("tst.Selfer", as.is = T)
	# OffspringID
	# Probability
# Example: offspring ID of ‘M2F3C4’ indicates that the offspring is
# the 4th child from the mating between the 2nd father and the 3rd mother
# For monoecious species males and females are the same set of individuals and are indicated by the same letter ‘M’.

# Split offspring ID
ID.extract <- function(x, idname = "OffspringID"){
	id <- x[, which(colnames(x) %in% idname)]

	data.frame(
	M = regmatches(x = id, m=regexpr(pattern = ".\\d{1,2}", text = id)),						# first parent
	F = substring(regmatches(x = id, m=regexpr(pattern = ".[M,m]\\d{1,2}", text = id)), 2),		# second parent
	C = regmatches(x = id, m=regexpr(pattern = "C\\d{1,2}", text = id)),						# child ID
	stringsAsFactors = FALSE)
}

# Extract only selfers (based on simulation name)
get.selfers <- function(x){
	x[with(x, which(M == F)), ]			# real selfers
}



get.selfers( ID.extract(datt, idname="Offspring") )

