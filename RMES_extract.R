# Functions to extract results from RMES output (David et al. Molecular Ecology 2007, DOI: 10.1111/j.1365-294X.2007.03330.x) 
# License: GPL-2 
# Author: Vladimir Mikryukov, June 2015

library(plyr)
library(xlsx)

## Set working directory
# setwd("...")

# Load the file with output copied from RMES results window
res <- readLines(file("RMES_default_output.txt"))


# Find the row with separators between the populations
pop.range <- grep(x = res, pattern = "*******************************", fixed = T)

# add the last line number
pop.range <- c(pop.range, length(res))

# split by populations
res <- split(res, cut(seq_along(res), breaks = pop.range, labels = FALSE))

# assign population names to the elements of list
extract.population <- function(x){	# from line 1
	gsub(pattern = "population :", replacement = "", x = x[1])
}
names(res) <- laply(.data = res, .fun = extract.population)


# extract selfing estimate and its SD
extract.selfing <- function(x){

	# line 5: g2 = estimator of the two-locus heterozygosity desequilibrium, over all pairs of loci,
	# 	with associated approximate bias and sampling standard deviation (SD).

	# line 6: s(g2) = the value of the selfing rate s deduced from g2 under the assumption of inbreeding and linkage equilibrium,
	# 	with associated bias and SD.

	# line 7: p-value for the null hypothesis s=g2=0, obtained by resampling genotypes
	# 	(random reassortment of single-locus heterozygosities among individuals within the population). 


	# extract g2
	g2 <- unlist(strsplit(x[5], split = ";"))
	
	# extract s
	sg2 <- unlist(strsplit(x[6], split = ";"))

	# extract p-value
	pv <- unlist(strsplit(x = x[7], split = ") = "))
	pv <- unlist(strsplit(x = pv[2], split = " based"))


	res <- data.frame(g2 = as.numeric(unlist(strsplit(g2[1], split = "g2="))[2]),
					g2.sd = as.numeric(unlist(strsplit(g2[2], split = "sd="))[2]),
					g2.bias = as.numeric(unlist(strsplit(g2[3], split = "expected bias ="))[2]),
					s = as.numeric(unlist(strsplit(sg2[1], split = ")="))[2]),
					s.sd = as.numeric(unlist(strsplit(sg2[2], split = "sd="))[2]),
					s.bias = as.numeric(unlist(strsplit(sg2[3], split = "expected bias ="))[2]),
					p = pv[1])
	return(res)
}


# Extract results for each population
RMES.results <- ldply(.data = res, .fun = extract.selfing, .id = "Population")


# Export result table
write.xlsx(RMES.results, file="RMES_results.xlsx", sheetName="res", append=TRUE)

