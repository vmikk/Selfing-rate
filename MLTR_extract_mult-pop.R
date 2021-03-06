## Functions to parse MLTR results (multiple populations)
# License: GPL-2 
# Author: Vladimir Mikryukov, June 2015

# tm = the multilocus population outcrossing rate
# ts = the (minimum variance) singlelocus population outcrossing rate
# F  = the (minimum variance) singlelocus inbreeding coefficient of maternal parents
# rp = the correlation of paternity (fraction of siblings that share the same father) --->  the extent of single- vs. multiple paternity in progeny arrays 
# rs = the correlation of selfing among families (normalized variance of selfing)



library(plyr)
library(xlsx)


## Set working directory
# setwd("...")

# Working directory should contain *.out files from MLTR analyses (population or group level)
# Example file - MLTR_sample2.out


filenames <- list.files(pattern="*.out$", full.names=TRUE)  # results from analysis
res <- lapply(filenames, readLines)
	meads <- gsub(pattern = ".out", replacement = "", x = filenames)
	meads <- gsub(pattern = "./", replacement = "", x = meads)
	names(res) <- meads
	rm(filenames)


# Function to remove leading and trailing whitespaces of a string
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# section containing estimates
extract.estimates <- function(x){
	l.strart <- grep(pattern = "            ---------------------------------------", x = x) + 2
	l.finish <- grep(pattern = "vs. group:", x = x)[1] - 1
	mltr <- x[l.strart : l.finish]
	return(mltr)
}

res.ests <- llply(.data = res, .fun = extract.estimates)


# split this section by populations (groups)
splt.group <- function(zz){
	ll <- c(grep(pattern = "Group:", x = zz), length(zz))	# line numbers for splitting
	ll <- ll-1

	# split by groups
	zz.split <- split(zz, cut(seq_along(zz), breaks = ll, labels = FALSE))

	# assign group names to the elements of a resulting list
	gr.name <- function(g){
		gr <- g[ grep(pattern = "Group:", x = g) ]
		gr <- gsub(pattern = "Group:", replacement = "", x = gr)
		gr <- trim(gr)
		return(gr)
	}
	names(zz.split) <- laply(.data = zz.split, .fun = gr.name)

	return(zz.split)
}

# split each out file by groups
res.ests <- llply(.data = res.ests, .fun = splt.group)


# Extract results for single population
extract.mltr <- function(x){
	res <- list()
		# res$pf <- x[ grep("Parental F estimate", x) ]
		res$tm <- x[ grep("Multilocus t estimate", x) ]
		res$ts <- x[ grep("Singlelocus t estimate", x) ]
		res$tms <- x[ grep("Difference tm-ts", x) ]
		res$ct <- x[ grep("Correlation of t", x) ]
		res$rpm <- x[ grep("Multilocus  correlation of p estimate", x) ]
		res$rps <- x[ grep("Singlelocus correlation of p estimate", x) ]
		res$drp <- x[ grep("Difference \\[rp", x) ]
		res$rs <- x[ grep("Correlation of s", x) ]

	splt <- function(z){ unlist(strsplit(x = z, split = "=")) }
	res <- llply(.data = res, .fun = splt)

	# remove leading and trailing whitespaces of a string
	res <- llply(.data = res, .fun = function(x){ aaply(.data = x, .margins = 1, .fun = trim) })

	# convert to data.frame
	res <- do.call(rbind, res)

	# split estimate and SD
	tmp <- do.call(rbind, aaply(.data = res[,2], .margins = 1, .fun = strsplit, split = " "))

	# remove parenthesis 
	sdd <- gsub(pattern = "\\(", replacement = "", x = tmp[,3])
	sdd <- gsub(pattern = "\\)", replacement = "", x = sdd)

	ok <- data.frame(
		Parameter = res[,1],
		Estimate = as.numeric(tmp[,1]),
		SD = as.numeric(sdd),
		stringsAsFactors = FALSE)

	# remove "(SD)" from parameter names
	ok$Parameter <- gsub(pattern = " \\(SD\\)", replacement = "", x = ok$Parameter)

	return(ok)
}
# extract.mltr(res.ests[[1]][[1]])		# Example


# extract results from each output file
mltr.OK <- ldply(.data = res.ests,
			.fun = function(x){ ldply(.data = x, .fun = extract.mltr, .id = "Group") },
			.id = "Out.file"
			)

write.xlsx(mltr.OK, file="MLTR_Results.xlsx", sheetName="mltr.by.group", append=TRUE)

