## Functions to prepare input files for COLONY simulation
# License: GPL-2 
# Author: Vladimir Mikryukov, June 2015


library(plyr)
set.seed(14789)


## Set working directory
# setwd("...")

# Load functions to create mating matrix (mating.matrix, fill.vector, vect.add)
source("Colony_PrepareRandomMatingMatrix.R")

## Set directory with input data
## E.g., .../examples/COLONY_simulation_inp/
# setwd("...")

## Directory with input data should contain
# Design matrix ("COLONY_simulation_Descr.txt")
# Genetic markers information for each population ("Markers_<PopulationName>.txt")
# Allele frequency data for each population ("<PopulationName>_AlleleFreq.txt")
## Population names should be the same as in design matrix ('Meadow' column)


######################################
###################################### # Read data
######################################

## Data design (offspring number, known mothers, etc.)
design <- read.delim("COLONY_simulation_Descr.txt")
 # $ Meadow 			Meadow and pollution zone name
 # $ Offsp 				Total number of known offspring ( sum(M1..M15) )
 # $ Mat 				Number of mothers with know progeny ( M1..M15 > 0)
 # $ Cand 				Candidate parental genotypes (always 15 in our design)
 # $ M1..M15			offspring number per Mother1, Mother2 ...


## Number of alleles per locus and error rates
marker.files <- list.files(pattern="Markers_", full.names=TRUE)
markers <- lapply(marker.files, read.table, header=T)
	tmp <- gsub(pattern = "./Markers_", replacement = "", x = marker.files)
	tmp <- gsub(pattern = ".txt", replacement = "", x = tmp)
	names(markers) <- tmp
	rm(tmp)

## Allele frequency data
allele.files <- list.files(pattern="_AlleleFreq", full.names=TRUE)
alleles <- lapply(allele.files, readLines)
	tmp <- gsub(pattern = "_AlleleFreq.txt", replacement = "", x = allele.files)
	tmp <- gsub(pattern = "./", replacement = "", x = tmp)
	names(alleles) <- tmp
	rm(tmp)


# Combine data by meadow
comb.datt <- function(design, alleles, markers){
	res <- list()
	for(i in names(alleles)){
		res[[i]] <- list()
		res[[i]]$Design <- design[which(design$Meadow == i), ]
		res[[i]]$AlleleFreq <- alleles[[i]]
		res[[i]]$NumAlleles <- as.integer(markers[[i]][2,])		# allele number per locus
		res[[i]]$DropRate <- markers[[i]][3,]					# allele dropout rate
		res[[i]]$ErrRate <- markers[[i]][4,]					# allele other error rate
	}
return(res)
}

datt <- comb.datt(design, alleles, markers)						# Combined data


#############################################################
#############################################################	Function to create COLONY input files
#############################################################

## Generate COLONY input [input3.Par & ProjectInformation.Txt]
gen.input <- function(outname = "BUF_mead1", n.reps = 5, self.rate = 0.20, mm.dim = 15,
				drp.rate = c(0,0,0,0,0), err.rate = c(0,0,0,0,0), allele.n = c(4,4,4,4,4),
				mm, 			# mating matrix
				allele.freq){


dir.create(outname)							# create sub-folder
# shell(paste('mkdir', outname, sep=" "))

	#######################################
	#######################################	input3.Par
	#######################################

fn <- paste(file.path(getwd(), outname), "/", "input3.Par", sep="")		# input file name with location

fp1 <- paste(outname, "                      !Output file name")
fp2 <- paste(n.reps, "                       !Number of replicates")
fp3 <- "0                              !0/1/2=Pair-likelihood Score(PLS)/Full likelihood(FL)/FL-PLS combined (FPLS) method"
fp4 <- "1                              !0/1/2/3 for low/medium/high/very high precision"
fp5 <- paste("1", self.rate, "                        !2/1=Dioecious/Monoecious,Selfing rate for monoecious")
fp6 <- "1                              !Number of mating structures"
fp7 <- paste(mm.dim, mm.dim, "                      !#dads & mums in a mating structure")
fp8 <- mm
fp9 <- ""													# empty line
fp10 <- matrix(data = 0, nrow = mm.dim, ncol = mm.dim)		# !#full siblings of known dad and known mum
fp11 <- ""													# empty line
fp12 <- matrix(data = 0, nrow = mm.dim, ncol = mm.dim)		# !#full siblings of known dad and unknown mum
fp13 <- ""													# empty line
fp14 <- mm 													# !#full siblings of unknown dad and known mum == same as MATING MATRIX
fp15 <- ""													# empty line
fp16 <- rep(1, mm.dim) 										# !Dads included (1) in the candidate list
fp17 <- rep(1, mm.dim) 										# !Mums included (1) in the candidate list
fp18 <- ""													# empty line
fp19 <- paste(0, 0, "                           !#candidate males & females")
fp20 <- paste(0.10, 0.90, "                     !Assumed prbs of fathers & mothers included in candidates")
fp21 <- paste(5, "                              !Number of Loci")
fp22 <- paste(0, "                              !Prob. of missing genotypes")
fp23 <- paste(paste(drp.rate, collapse=" "), "                       !Dropout rates")
fp24 <- paste(paste(err.rate, collapse=" "), "                       !Other error rates")  
fp25 <- paste(paste(rep(0,5), collapse=" "), "                       !Codominant/Dominant (0/1) markers")
fp26 <- paste(paste(allele.n, collapse=" "), "                       !#alleles/locus, must be=2 for dominant marker")
fp27 <- paste(3, "                       !0/1/2/3=Uniform/Equal/Triangular/other allele freq. distr.")
fp28 <- allele.freq
fp29 <- ""													# empty line
fp30 <- paste("2                              !1/n=HaploDiploid/n-ploid species")
fp31 <- paste("0  0                           !1/0 =Mono/Polygamy for males & females")
fp32 <- paste("1234                           !Seed for random number generator")
fp33 <- paste("1  1  1                        !I,R,R : I=0,1,2,3 for No,weak,medium,strong sibship size prior R,R=mean paternal & maternal sibship size")
fp34 <- paste("0                              !B, 0/1=Clone inference =No/Yes")
fp35 <- paste("1                              !B, 0/1=Scale full sibship=No/Yes")
fp36 <- paste("0                              !1/0 (Y/N) for known allele frequency")
fp37 <- paste("0                              !1/0 for updating allele freq. or not")
fp38 <- paste("1                              !#replicate runs")
fp39 <- paste("2                              !0/1/2/3/4=VeryShort/Short/Medium/Long/VeryLong run")
fp40 <- paste("-1                             !Map length in Morgans. <0 for infinite length")
fp41 <- paste("0                              !Inbreeding coefficient of parents")
fp42 <- paste("1                              !0/1=N/Y for allowing inbreeding in Colony")
fp43 <- paste("1                              !0/1 for Windows DOS/GUI run, parameter in Colony")
fp44 <- paste("1                              !#iterates/#second for Windows DOS/GUI run, parameter in Colony")



wrt.matr <- function(x){
	write.table(x = x, file = fn, append = T, quote = F, row.names = F, col.names = F)
}

wrt.allele <- function(x){
	a_ply(.data = x, .margins = 1, .fun = write, file = fn, append=TRUE, ncolumns=1000)
}

write(x = fp1, file = fn, append=TRUE, ncolumns=1000)
write(x = fp2, file = fn, append=TRUE, ncolumns=1000)
write(x = fp3, file = fn, append=TRUE, ncolumns=1000)
write(x = fp4, file = fn, append=TRUE, ncolumns=1000)
write(x = fp5, file = fn, append=TRUE, ncolumns=1000)
write(x = fp6, file = fn, append=TRUE, ncolumns=1000)
write(x = fp7, file = fn, append=TRUE, ncolumns=1000)
wrt.matr(fp8) 															# mating matrix
write(x = fp9, file = fn, append=TRUE, ncolumns=1000)
wrt.matr(fp10) 															# mating matrix
write(x = fp11, file = fn, append=TRUE, ncolumns=1000)
wrt.matr(fp12) 															# mating matrix
write(x = fp13, file = fn, append=TRUE, ncolumns=1000)
wrt.matr(fp14) 															# mating matrix
write(x = fp15, file = fn, append=TRUE, ncolumns=1000)
write(x = fp16, file = fn, append=TRUE, ncolumns=1000)
write(x = fp17, file = fn, append=TRUE, ncolumns=1000)
write(x = fp18, file = fn, append=TRUE, ncolumns=1000)
write(x = fp19, file = fn, append=TRUE, ncolumns=1000)
write(x = fp20, file = fn, append=TRUE, ncolumns=1000)
write(x = fp21, file = fn, append=TRUE, ncolumns=1000)
write(x = fp22, file = fn, append=TRUE, ncolumns=1000)
write(x = fp23, file = fn, append=TRUE, ncolumns=1000)
write(x = fp24, file = fn, append=TRUE, ncolumns=1000)
write(x = fp25, file = fn, append=TRUE, ncolumns=1000)
write(x = fp26, file = fn, append=TRUE, ncolumns=1000)
write(x = fp27, file = fn, append=TRUE, ncolumns=1000)
wrt.allele(fp28)														# allele frequency
write(x = fp29, file = fn, append=TRUE, ncolumns=1000)
write(x = fp30, file = fn, append=TRUE, ncolumns=1000)
write(x = fp31, file = fn, append=TRUE, ncolumns=1000)
write(x = fp32, file = fn, append=TRUE, ncolumns=1000)
write(x = fp33, file = fn, append=TRUE, ncolumns=1000)
write(x = fp34, file = fn, append=TRUE, ncolumns=1000)
write(x = fp35, file = fn, append=TRUE, ncolumns=1000)
write(x = fp36, file = fn, append=TRUE, ncolumns=1000)
write(x = fp37, file = fn, append=TRUE, ncolumns=1000)
write(x = fp38, file = fn, append=TRUE, ncolumns=1000)
write(x = fp39, file = fn, append=TRUE, ncolumns=1000)
write(x = fp40, file = fn, append=TRUE, ncolumns=1000)
write(x = fp41, file = fn, append=TRUE, ncolumns=1000)
write(x = fp42, file = fn, append=TRUE, ncolumns=1000)
write(x = fp43, file = fn, append=TRUE, ncolumns=1000)
write(x = fp44, file = fn, append=TRUE, ncolumns=1000)


	#######################################
	#######################################	ProjectInformation.Txt
	#######################################

pn <- paste(file.path(getwd(), outname), "/", "ProjectInformation.Txt", sep="")		# input file name with location

pi1 <- paste("Output file path & name : 'C:\\Programs\\Colony\\", outname, "\\", outname, "'\n", sep="")
pi2 <- "Number of loci : 5"
pi3 <- paste("Number of offspring in the sample :",  sum(mm))
pi4 <- "Outbreeding (0) or inbreeding (1) model : 1"
pi5 <- paste("Number of male candidates :", mm.dim)
pi6 <- paste("Number of female candidates :", mm.dim)
pi7 <- "Number of known paternal sibships : 0"
pi8 <- paste("Number of known maternal sibships :",  length(which(colSums(mm) > 0)))
pi9 <- "Number of offspring with excluded fathers : 0"
pi10 <- "Number of offspring with excluded mothers : 0"
pi11 <- "Male mating system : Polygamy"
pi12 <- "Female mating system : Polygamy"
pi13 <- "Number of threads : 1"
pi14 <- "Number of Excluded Paternal Sibships : 0"
pi15 <- "Number of Excluded Maternal Sibships : 0"
pi16 <- "Dioecious (2) or monoecious (1) : 1"
pi17 <- "Seed for random number generator : 1234"
pi18 <- "Allele frequency : No updating by accounting for the inferred relationship"
pi19 <- "Species : Diploid"
pi20 <- "Sibship size prior : Yes"
pi21 <- "paternal & maternal sibship sizes : 1  1"
pi22 <- "Known population allele frequency : No"
pi23 <- "Number of run : 1"
pi24 <- "Length of run : Medium"
pi25 <- "Monitor intermiediate results by : Every 1 second"
pi26 <- "Prob. a dad is included in the male candidates : 0.10"
pi27 <- "Prob. a mum is included in the female candidates : 0.90"
pi28 <- paste("Project data input produced :", Sys.Date())

cat(pi1, file = pn, append = TRUE)
write(x = pi2, file = pn, append=TRUE, ncolumns=1000)
write(x = pi3, file = pn, append=TRUE, ncolumns=1000)
write(x = pi4, file = pn, append=TRUE, ncolumns=1000)
write(x = pi5, file = pn, append=TRUE, ncolumns=1000)
write(x = pi6, file = pn, append=TRUE, ncolumns=1000)
write(x = pi7, file = pn, append=TRUE, ncolumns=1000)
write(x = pi8, file = pn, append=TRUE, ncolumns=1000)
write(x = pi9, file = pn, append=TRUE, ncolumns=1000)
write(x = pi10, file = pn, append=TRUE, ncolumns=1000)
write(x = pi11, file = pn, append=TRUE, ncolumns=1000)
write(x = pi12, file = pn, append=TRUE, ncolumns=1000)
write(x = pi13, file = pn, append=TRUE, ncolumns=1000)
write(x = pi14, file = pn, append=TRUE, ncolumns=1000)
write(x = pi15, file = pn, append=TRUE, ncolumns=1000)
write(x = pi16, file = pn, append=TRUE, ncolumns=1000)
write(x = pi17, file = pn, append=TRUE, ncolumns=1000)
write(x = pi18, file = pn, append=TRUE, ncolumns=1000)
write(x = pi19, file = pn, append=TRUE, ncolumns=1000)
write(x = pi20, file = pn, append=TRUE, ncolumns=1000)
write(x = pi21, file = pn, append=TRUE, ncolumns=1000)
write(x = pi22, file = pn, append=TRUE, ncolumns=1000)
write(x = pi23, file = pn, append=TRUE, ncolumns=1000)
write(x = pi24, file = pn, append=TRUE, ncolumns=1000)
write(x = pi25, file = pn, append=TRUE, ncolumns=1000)
write(x = pi26, file = pn, append=TRUE, ncolumns=1000)
write(x = pi27, file = pn, append=TRUE, ncolumns=1000)
write(x = pi28, file = pn, append=TRUE, ncolumns=1000)
}



#############################################################
#############################################################	Generate input files
#############################################################

# Function used by plyr to create COLONY input for each meadow (element in a 'datt' list)
colony.input <- function(x, suffix="", self=0.2){
	
	outname <- as.character(x$Design$Meadow)	# which meadows is it?
	if(nchar(suffix) > 0){ outname <- paste(outname, suffix, sep="") }
	n <- x$Design$Cand 							# known mother genotypes

	# Extract know family structure (known offspring number per mother)
	moth.offsp <- x$Design[1, which(colnames(x$Design) %in% paste("M", 1:n, sep=""))]

	# Generate mating matrix
	mm <- mating.matrix(n = n, s = self, cs = moth.offsp)	# function from 'Prepare_Mating Matrix_Random.R' source

	gen.input(
		outname = outname,			 		# meadow name
		n.reps = 5,							# number of replicated (for COLONY)
		self.rate = self,					# selfing rate
		mm.dim = n, 						# the number of candidate parents
		drp.rate = c(0,0,0,0,0), 			# dropout rate
		err.rate = x$DropRate, 				# error rate
		allele.n = x$NumAlleles,			# number of alleles per locus
		mm = mm,							# mating matrix
		allele.freq = x$AlleleFreq 			# distribution of allele frequencies at each locus 
	)

	return(mm)		# return mating matrix
}


# Store mating matrix for each meadow and each replication
MatingMatrices <- list()
	MatingMatrices$repl1 <- llply(.data = datt, .fun = colony.input, suffix=".repl1")
	MatingMatrices$repl2 <- llply(.data = datt, .fun = colony.input, suffix=".repl2")
	MatingMatrices$repl3 <- llply(.data = datt, .fun = colony.input, suffix=".repl3")
	MatingMatrices$repl4 <- llply(.data = datt, .fun = colony.input, suffix=".repl4")
	MatingMatrices$repl5 <- llply(.data = datt, .fun = colony.input, suffix=".repl5")


## check expected selfing rate from mating matrices
# selfrate <- function(x){ sum(diag(x)) / sum(x) }
# ldply(.data = MatingMatrices, .fun = function(x) { ldply(.data = x, .fun = selfrate, .id = "Meadow") }, .id="Replicate")


