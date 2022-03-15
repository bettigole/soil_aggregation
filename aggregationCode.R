library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))
source('code/projectSetup.R')
source('code/aggregateFun.R')
source('code/anovaFun.R')
source('code/plotBarFun.R')

set.seet<-42

data<-sb # choose l7, sb, or caney
nGroups<-20 # max number of aggregates per mgmt unit
nIter<-10 # number of iterations for models
sig<-1.645 #1.96 for 95% significance, 1.645 for 90%

############### AGGREGATION SECTION !!! ####################

#run the looping function
stBig<-loopFun()

#plot results
ribbonFun()

############### ANOVA SECTION !!! ####################

#run the ANOVA looping function
anovaOut<-anovaLoop()

#plot ANOVA results
anovaPlot(anovaOut)

############### One off Plotting SECTION !!! ####################

plotFun(2)

#### Need a new section that Models Will's method. Clumped into 2-3 groups. 
### Replicates taken of each 2-3x. This will inflate sig of lower groups by a LOT

