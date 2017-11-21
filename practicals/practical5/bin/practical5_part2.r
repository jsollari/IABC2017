#!/usr/bin/env Rscript
#autor:      Joao Sollari Lopes
#local:      University of Reading, Reading, UK
#Rversion:   3.2.3
#criado:     01.03.2010
#modificado: 20.11.2017

# @arg rej_file  - file with rejection step results (.dat)
get_cp <- function(data_file){

	#import leaps library
	library(leaps)

    #info on summstats
	lsstats <- c("pi1","S1","k_S1","sH_S1","avMFS1","sdMFS1")
	nsstats <- length(lsstats)
    
	#import the .dat file
	abc.data <- data.matrix(read.table(data_file))

    #normalize summstats
	sstats <- abc.data[,7:12]
	sstats.s <- sstats
	for(i in 1:nsstats){
		sstats.s[,i] <- (sstats[,i] - mean(sstats[,i]))/sd(sstats[,i])
	}

	#get Cp values
	x.ne <- regsubsets(x=sstats.s,y=log(abc.data[,6]),nbest=1,nvmax=6,method="exhaustive",really.big=TRUE)
	res <- summary(x.ne)
    mat <- cbind(res$rsq,res$cp,res$which[,-1])
    colnames(mat) <- c("r-sq","cp",lsstats)
    print(mat)
}

args = commandArgs(trailingOnly=TRUE)
get_cp(args[1])
