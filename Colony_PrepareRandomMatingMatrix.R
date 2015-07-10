## Functions to prepare random selfing matrix for COLONY
## based on the selfing rate and number of known offspring per mother
# License: GPL-2 
# Author: Vladimir Mikryukov, June 2015


##########################
##########################	Functions
##########################

# Spread the number of observations (tot) across the vector of known length (len)
fill.vector <- function(len, tot){
	vv <- rep(0, len)
	cc <- tot 			# counter of remainig observations
	while(cc > 0){
		tmp <- rpois(n = len, lambda = 0.3) 	# generate random counts
		vv <- vv + tmp							# fill vector
		cc <- cc - sum(tmp)
		rm(tmp)
	}

	dlt <- tot - sum(vv)
	if(dlt > 0){								# if too less was created
		while(dlt > 0){
			tmp <- sample(x = which(vv > 0), size=1)
			vv[tmp] <- vv[tmp] + 1
			dlt <- tot - sum(vv)
			rm(tmp)
		}
	}
	if(dlt < 0){								# if too much was created
		while(dlt < 0){
			tmp <- sample(x = which(vv > 0), size=1)
			vv[tmp] <- vv[tmp] - 1
			dlt <- tot - sum(vv)
			rm(tmp)
		}
	}

	return(vv)
}
# fill.vector(5, 15) 	# Example


# Spread the number of observations (tot) across the supplied vector keeping non-zero elements intact
# tot = summ of the resulting vector (including intact elements)
vect.add <- function(vv, tot){
	cc <- tot - sum(vv)		# counter of remainig observations
	ff <- which(vv == 0)	# this elements of vector may be altered
	while(cc > 0){
		tmp <- sample(x = ff, size=1)
		vv[tmp] <- vv[tmp] + 1
		cc <- cc - 1
		rm(tmp)
	}
	return(vv)
}
# vect.add(c(0,0,0,1,0), 10)		# Example



# Create mating matrix
mating.matrix <- function(n = 15, s = 0.3, cs){
	# n = dimenstions of mating matrix (for monoecious plants number of males = number of females)
	# s = desired rate of selfers (diagonale elements)
	# cs = vector of known offspring numbers from female (sum of columns)

	# tot <- n*n 								# total number of elements in matrix

	if(length(cs) != n){ stop("CS vector should be of the same size as N")}

	mm <- matrix(data=0, ncol=n, nrow=n)						# empty matrix
	offsp <- sum(cs)											# number of offsprings
	self.count <- round(offsp * s)								# number of offsprings expected to be a result of selfing
	mats <- which(cs != 0)										# which mothers are known
	self.diag <- fill.vector(length(mats), self.count)			# selfing counts for known mothers
	diag(mm) <- c(self.diag, rep(0, length(which(cs == 0)))) 	# mating matrix diagonal
	
	for(i in 1:length(mats)){
		mm[,i] <- vect.add(mm[,i], cs[i])
	}
	
	return(mm)
}
## Example 1
# ( zz <- mating.matrix(n = 15, s = 0.3, cs = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0)) )
# sum(diag(zz)) / sum(zz)		# observed selfing rate
# colSums(zz)
# rowSums(zz)

# ( zz <- mating.matrix(n = 15, s = 0.2, cs = c(15, 11, 12, 14, 6, 11, 8, 10, 12, 13, 9, 0, 0, 0, 0)) )		# Example 2
# sum(diag(zz)) / sum(zz)		# observed selfing rate
