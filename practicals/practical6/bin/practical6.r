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

	#import libraries
	library(locfit)
	library(nnet)
    library(abc)

	#import Mark Beaumont's scripts
	source("make_pd2.r")

	#demographic parameters' priors (minimum and maximum values)
	mintev <- 0
	maxtev <- 10000
	minNe1 <- 0
	maxNe1 <- 12500
	minNe2 <- 0
	maxNe2 <- 40000
	minNeA <- 0
	maxNeA <- 10000
	minmig1 <- 0
	maxmig1 <- 0.0005
	minmig2 <- 0
	maxmig2 <- 0.0005

    truetev <- 5000
    trueNe1 <- 10000
    trueNe2 <- 20000
    trueNeA <- 5000
    truemig1 <- 0.0001
    truemig2 <- 0.0001

	#import the .rej file
	abc.rej <- data.matrix(read.table(rej_file))

	#import the .pri files
	priors <- data.matrix(read.table(pri_file))

	#import the .trg files
	target <- data.matrix(read.table(data_file))

	# Plot line for splitting time and save it in a .eps file
	abc.reg.tev <- makepd4(target,abc.rej[,7],abc.rej[,13:21],tol=1,rej=F,transf="log")
	abc.nn.tev <- abc(target,abc.rej[,7],abc.rej[,13:21],tol=1,method="neuralnet",transf="log")
    fname <- "../results/tev_reg.eps"
    postscript(file=fname,width=7,height=7,colormodel="rgb",horizontal=FALSE,onefile=FALSE,paper="special")
	plot(locfit(~abc.reg.tev$x,weight=abc.reg.tev$wt,xlim=c(mintev,maxtev)),main="prior (black) and posterior (blue) distributions",xlab="tev",col="blue")
	lines(locfit(~abc.nn.tev$adj.values,weight=abc.nn.tev$weights,xlim=c(mintev,maxtev)),col="purple")
	lines(locfit(~priors[,7],xlim=c(mintev,maxtev)),col="black")
    abline(v=truetev,lty=2,col="black")
	dev.off()
	print("tev done.")

	# Plot line for effective size of population 1 and save it in a .eps file
	abc.reg.ne1 <- makepd4(target,abc.rej[,8],abc.rej[,13:21],tol=1,rej=F,transf="log")
	abc.nn.ne1 <- abc(target,abc.rej[,8],abc.rej[,13:21],tol=1,method="neuralnet",transf="log")
    fname <- "../results/Ne1_reg.eps"
    postscript(file=fname,width=7,height=7,colormodel="rgb",horizontal=FALSE,onefile=FALSE,paper="special")
	plot(locfit(~abc.reg.ne1$x,weight=abc.reg.ne1$wt,xlim=c(minNe1,maxNe1)),main="prior (black) and posterior (blue) distributions",xlab="Ne1",col="blue")
	lines(locfit(~abc.nn.ne1$adj.values,weight=abc.nn.ne1$weights,xlim=c(minNe1,maxNe1)),col="purple")
	lines(locfit(~priors[,8],xlim=c(minNe1,maxNe1)),col="black")
    abline(v=trueNe1,lty=2,col="black")
	dev.off()
	print("Ne1 done.")

	# Plot line for effective size of population 2 and save it in a .eps file
	abc.reg.ne2 <- makepd4(target,abc.rej[,9],abc.rej[,13:21],tol=1,rej=F,transf="log")
	abc.nn.ne2 <- abc(target,abc.rej[,9],abc.rej[,13:21],tol=1,method="neuralnet",transf="log")
    fname <- "../results/Ne2_reg.eps"
    postscript(file=fname,width=7,height=7,colormodel="rgb",horizontal=FALSE,onefile=FALSE,paper="special")
	plot(locfit(~abc.reg.ne2$x,weight=abc.reg.ne2$wt,xlim=c(minNe2,maxNe2)),main="prior (black) and posterior (blue) distributions",xlab="Ne2",col="blue")
	lines(locfit(~abc.nn.ne2$adj.values,weight=abc.nn.ne2$weights,xlim=c(minNe2,maxNe2)),col="purple")
	lines(locfit(~priors[,9],xlim=c(minNe2,maxNe2)),col="black")
    abline(v=trueNe2,lty=2,col="black")
	dev.off()
	print("Ne2 done.")

	# Plot line for effective size of ancestor population and save it in a .eps file
	abc.reg.nea <- makepd4(target,abc.rej[,10],abc.rej[,13:21],tol=1,rej=F,transf="log")
	abc.nn.nea <- abc(target,abc.rej[,10],abc.rej[,13:21],tol=1,method="neuralnet",transf="log")
    fname <- "../results/NeA_reg.eps"
    postscript(file=fname,width=7,height=7,colormodel="rgb",horizontal=FALSE,onefile=FALSE,paper="special")
	plot(locfit(~abc.reg.nea$x,weight=abc.reg.nea$wt,xlim=c(minNeA,maxNeA)),main="prior (black) and posterior (blue) distributions",xlab="NeA",col="blue")
	lines(locfit(~abc.nn.nea$adj.values,weight=abc.nn.nea$weights,xlim=c(minNeA,maxNeA)),col="purple")
	lines(locfit(~priors[,10],xlim=c(minNeA,maxNeA)),col="black")
    abline(v=trueNeA,lty=2,col="black")
	dev.off()
	print("NeA done.")

	# Plot line for migration rate of population 1 and save it in a .eps file
	abc.reg.mig1 <- makepd4(target,abc.rej[,11],abc.rej[,13:21],tol=1,rej=F,transf="log")
	abc.nn.mig1 <- abc(target,abc.rej[,11],abc.rej[,13:21],tol=1,method="neuralnet",transf="log")
    fname <- "../results/mig1_reg.eps"
    postscript(file=fname,width=7,height=7,colormodel="rgb",horizontal=FALSE,onefile=FALSE,paper="special")
	plot(locfit(~abc.reg.mig1$x,weight=abc.reg.mig1$wt,xlim=c(minmig1,maxmig1)),main="prior (black) and posterior (blue) distributions",xlab="mig1",col="blue")
	lines(locfit(~abc.nn.mig1$adj.values,weight=abc.nn.mig1$weights,xlim=c(minmig1,maxmig1)),col="purple")
	lines(locfit(~priors[,11],xlim=c(minmig1,maxmig1)),col="black")
    abline(v=truemig1,lty=2,col="black")
	dev.off()
	print("mig1 done.")

	# Plot line for migration rate of population 2 and save it in a .eps file
	abc.reg.mig2 <- makepd4(target,abc.rej[,12],abc.rej[,13:21],tol=1,rej=F,transf="log")
	abc.nn.mig2 <- abc(target,abc.rej[,12],abc.rej[,13:21],tol=1,method="neuralnet",transf="log")
    fname <- "../results/mig2_reg.eps"
    postscript(file=fname,width=7,height=7,colormodel="rgb",horizontal=FALSE,onefile=FALSE,paper="special")
	plot(locfit(~abc.reg.mig2$x,weight=abc.reg.mig2$wt,xlim=c(minmig2,maxmig2)),main="prior (black) and posterior (blue) distributions",xlab="mig2",col="blue")
	lines(locfit(~abc.nn.mig2$adj.values,weight=abc.nn.mig2$weights,xlim=c(minmig2,maxmig2)),col="purple")
	lines(locfit(~priors[,12],xlim=c(minmig2,maxmig2)),col="black")
    abline(v=truemig2,lty=2,col="black")
	dev.off()
	print("mig2 done.")

}

args = commandArgs(trailingOnly=TRUE)
reg_step(args[1],args[2],args[3])
