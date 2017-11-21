#!/usr/bin/env Rscript
#autor:      Joao Sollari Lopes
#local:      University of Reading, Reading, UK
#Rversion:   3.2.3
#criado:     01.03.2010
#modificado: 20.11.2017

# @arg rej_file  - file with rejection step results (.dat)
plot_sstats <- function(data_file){

    #info on summstats
	lsstats <- c("pi1","S1","k_S1","sH_S1","avMFS1","sdMFS1")
	nsstats <- length(lsstats)

	#import the .dat file
    npoints <- 1000
	abc.data <- data.matrix(read.table(data_file))[1:npoints,]

    #normalize summstats
	sstats <- abc.data[,7:12]
	sstats.s <- sstats
	for(i in 1:nsstats){
		sstats.s[,i] <- (sstats[,i] - mean(sstats[,i]))/sd(sstats[,i])
	}

	#plotting summstats
    fname <- "../results/plot_sstats.eps"
    postscript(file=fname,width=7,height=7,colormodel="rgb",horizontal=FALSE,onefile=FALSE,paper="special")
	oldpar <- par(mfrow=c(2,3),xaxt="n",yaxt="n",mar=c(2,2,2,2))
	for(i in 1:nsstats){
		plot(sstats[,i],log(abc.data[,6]),pch='.',main=lsstats[i])
		abline(lm(log(abc.data[,6])~sstats[,i]),lty=2,col="red")
	}
	par(oldpar)
	dev.off()
	print("done plot sstats")
    
}

args = commandArgs(trailingOnly=TRUE)
plot_sstats(args[1])
