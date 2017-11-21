#!/usr/bin/env Rscript
#autor:      Joao Sollari Lopes
#local:      University of Reading, Reading, UK
#Rversion:   3.2.3
#criado:     01.03.2010
#modificado: 20.11.2017

# @arg data_file - file with summary of 'real' data (.trg)
# @arg rej_file  - file with rejection step results (.rej)
# @arg pri_file  - file with sample of priors (.pri)
model_choice <- function(data_file , rej_file , pri_file){

	#import VGAM library
	library(VGAM)

	#import Mark Beaumont's scripts
	source("calmod.r")

	#import the .rej file
	abc.rej <- data.matrix(read.table(rej_file))

	#import the .pri files
	priors <- data.matrix(read.table(pri_file))

	#import the .trg files
	target <- data.matrix(read.table(data_file))

	# Perform model choice with the rejection-step
	top1 <- length(which(abc.rej[,2]==1))/2000
	top2 <- length(which(abc.rej[,2]==2))/2000
	print(c(top1,top2))
    fname <- "../results/model_rej.eps"
    postscript(file=fname,width=7,height=7,colormodel="rgb",horizontal=FALSE,onefile=FALSE,paper="special")
	barplot(c(top1,top2),names=c("top1","top2"))
	abline(h=1.0/2,col="red")
	dev.off()
	print("model choice - rejection done.")

	# Perform model choice with the regression-step
	abc.model <- calmod(target,abc.rej[,2],abc.rej[,13:21],1,rej=F)
	print(abc.model$x2)
    fname <- "../results/model_reg.eps"
    postscript(file=fname,width=7,height=7,colormodel="rgb",horizontal=FALSE,onefile=FALSE,paper="special")
	barplot(abc.model$x2,names=c("top1","top2"))
	abline(h=1.0/2,col="red")
	dev.off()
	print("model choice - regression done.")

}

args = commandArgs(trailingOnly=TRUE)
model_choice(args[1],args[2],args[3])