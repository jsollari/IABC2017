#!/usr/bin/env Rscript
#autor:      Joao Sollari Lopes
#local:      University of Reading, Reading, UK
#Rversion:   3.2.3
#criado:     01.03.2010
#modificado: 20.11.2017

# @arg data_file - file with summary of 'real' data (.trg)
# @arg rej_file  - file with rejection step results (.rej)
check_fit <- function(data_file , rej_file){

	#import the .rej file
	abc.rej <- data.matrix(read.table(rej_file))

	#import the .trg files
	target <- data.matrix(read.table(data_file))

	#check target on summstats
    fname <- "../results/check_fit.eps"
    postscript(file=fname,width=7,height=7,colormodel="rgb",horizontal=FALSE,onefile=FALSE,paper="special")
	oldpar <- par(mfrow=c(1,3),xaxt="n",yaxt="n",mar=c(2,2,2,2))
	nparams <- 6
	isstats <- 1
	fsstats <- 3
	for(i in isstats:fsstats){
		hist(abc.rej[,nparams+i],main=i)
		abline(v=target[i],lwd=2,lty=1,col="blue")
	}
	par(oldpar)
	dev.off()
	print("done target check")

}

args = commandArgs(trailingOnly=TRUE)
check_fit(args[1],args[2])
