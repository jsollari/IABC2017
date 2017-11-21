#!/usr/bin/env Rscript
#autor:      Joao Sollari Lopes
#local:      University of Reading, Reading, UK
#Rversion:   3.2.3
#criado:     01.03.2010
#modificado: 20.11.2017

# @arg data_file - file with summary of 'real' data (.trg)
# @arg rej_file  - file with rejection step results (.rej)
# @arg pri_file  - file with sample of priors (.pri)
reg_step <- function(data_file , rej_file , pri_file){

	#import locfit library
	library(locfit)

	#import Mark Beaumont's scripts
	source("make_pd2.r")
	source("loc2plot_d.r")

	#demographic parameters' priors (minimum and maximum values)
	minNe <- 0
	maxNe <- 50000
    trueNe <- 30000
    
	#import the .rej file
	abc.rej <- data.matrix(read.table(rej_file))

	#import the .pri files
	priors <- data.matrix(read.table(pri_file))

	#import the .trg files
	target <- data.matrix(read.table(data_file))

	# Plot line for effective size of population 1 and save it in a .eps file
	abc.reg.ne <- makepd4(target,abc.rej[,6],abc.rej[,7:9],tol=1,rej=F,transf="none")
    fname <- "../results/Ne_reg.eps"
    postscript(file=fname,width=7,height=7,colormodel="rgb",horizontal=FALSE,onefile=FALSE,paper="special")
	plot(locfit(~abc.reg.ne$x,weight=abc.reg.ne$wt,xlim=c(minNe,maxNe)),main="prior (black) and posterior (blue) distributions",xlab="Ne",col="blue")
	lines(locfit(~priors[,6],xlim=c(minNe,maxNe)),col="black")
    abline(v=trueNe,lty=2,col="black")
	dev.off()

	# Calculate mode and credible intervals for effective size of population 1
	print(loc1statsx(x=abc.reg.ne$x,wt=abc.reg.ne$wt, prob=0.10,alpha=1.0,xlim=c(minNe,maxNe)))
	print("Ne done.")

}

args = commandArgs(trailingOnly=TRUE)
reg_step(args[1],args[2],args[3])
