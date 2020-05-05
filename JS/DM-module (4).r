## ---------------------------------------------------------------------------
##
## The only thing necessary for the code to run is to set the working directory to the folder that also has the "dataJS.csv" file
## This is only used to get the correct ts values for the model and to compare the model to the experimental data
## After this is all set the code should run if everything is selected and ran
## In the end two graphs will be shown, the model will be the graph of my model and the J&S All will be the experimental model
## This should provide an easy comparison of the model and the experimental data
##
## ---------------------------------------------------------------------------
setwd("C:/Users/Peter/Documents/groningen/Master/Block 2/cognitive modelling/Assignment 4")
## List with parameter values:

params <- list()

params$d <- .5

## ---------------------------------------------------------------------------
## actr functions and convertion from pulses to time and reverse

a <- 1.1
b <- 0.015
t0 <- 0.011
retrievalThreshold = 12

actr.noise <- function(s,n=1) {
  rand <- runif(n,min=0.0001,max=0.9999)
  s * log((1 - rand ) / rand)
}

PulsesToTime <- function(pulses){
  n <- 0
  time <- 0
  while(n < pulses){
    SD <- a * b * t0
    t0 <- a * t0 + actr.noise(SD, 1)
    time <- time + t0 
    n <- n + 1
  }
  time <- time*1000
  return(time)
}

TimeToPulses <- function(time){
  time <- time/1000
  cTime <- 0
  pulses <- 0
  while (cTime < time){
    SD <- a*b* t0
    t0 = a * t0 + actr.noise(SD, 1)
    cTime = cTime + t0
    pulses <- pulses + 1
  }
  return(pulses)
}


## ---------------------------------------------------------------------------



## DM functions

create.dm <- function(chunks,encounters) {
  if (chunks > 52) {
    stop("Only up to 52 chunks allowed.")
  }
  DM <- array(NA,c(chunks,encounters))
  row.names(DM) <- c(letters,LETTERS)[1:chunks]
  DM
}

add.encounter <- function(DM,chunk,time) {
  tmp <- DM[chunk,]
  DM[chunk,sum(!is.na(tmp))+1] <- time
  DM
}

get.encounters <- function(DM,chunk) {
  tmp <- DM[chunk,]
  tmp[!is.na(tmp)]
}

## ---------------------------------------------------------------------------

## Baselevel activation functions:

## Simple version (only takes a single curtime value)
actr.B <- function(encounters,curtime) {
	if (curtime < min(encounters)) {
		return(NA)
	} else {
		log(sum((curtime - encounters[encounters<curtime])^-params$d))	
	}
}

## More advanced version, also takes multiple curtime values
actr.B <- function(encounters,curtime) {
	if (length(curtime)>1) {
		sapply(curtime,function(X) { actr.B(encounters,X)})
	} else {
		if (curtime < min(encounters)) {
			return(NA)
		} else {
			log(sum((curtime - encounters[encounters<curtime])^-params$d))	
		}
	}
}

actr.B.optimized <- function(n,Time,curtime=NULL) {
	if (is.null(curtime)) {
		log(n/(1-params$d)) - params$d * log(Time)
	} else {
		n <- curtime/Time * n
		log(n/(1-params$d)) - params$d * log(curtime)
	}
}

probabilityRecall <- function(chunk, participantInfo){
  if (length(get.encounters(participantInfo$DM, chunk)) == 0){
    Ai = retrievalThreshold
  }
  else{
    Ai = actr.B(get.encounters(participantInfo$DM, chunk),participantInfo$Time);
  }
  top = exp(Ai/participantInfo$Time)
  j <- 1;
  bottom <-0;
  while (j < 31){
    if (length(get.encounters(participantInfo$DM, j)) == 0){
      activation = retrievalThreshold
    }
    else {
      activation = actr.B(get.encounters(participantInfo$DM, j),participantInfo$Time);
    }
    bottom = bottom + exp(activation/participantInfo$Time);
    j = j+1;
  }
  probability = top/bottom;
  return(probability)  
}

blendedValue <- function(participantInfo){
  i <- 1;
  blendedResult <- 0
  while (i < 31){
    blendedResult = blendedResult + probabilityRecall(i, participantInfo)*i
    i=i+1
  }
  return(blendedResult)
}

## ---------------------------------------------------------------------------

## Some example code to get you started:

##params$num.chunks <- 4
##params$max.num.encounters <- 100
##params$duration <- 1000

## Create a DM with room for num.chunks chunks with up to num.encounters encounters per chunk.
##DM <- create.dm(params$num.chunks,params$max.num.encounters)

## Add 100 encounters to DM
##for (i in 1:250) {
##  DM <- add.encounter(DM,letters[trunc(runif(1,1,params$num.chunks+1))],runif(1,0,params$duration))
##}

## Calculate the activation per chunk: 

##for (i in letters[1:params$num.chunks]) {
##  cat("Activation of chunk ",i," is ");
##  cat(actr.B(get.encounters(DM,i),params$duration),"\n");
##}

## Plot activation per chunk:

##par(mfrow=c(2,2))
##for (i in letters[1:params$num.chunks]) {
##  plot(1,1,type="n",xlim=c(1,params$duration),ylim=c(-4,4),xlab="Time",ylab="Activation",main=paste("chunk",i))
  
##  curencounters <- get.encounters(DM,i)
##  if (length(curencounters)>0) {
##    lines(actr.B(curencounters,1:params$duration))
##  }
##}


## ---------------------------------------------------------------------------

##----------------------------------------------------------------------------
## global parameters model

nParticipants <- 6;
nTrialsExp <- 500;
nTrialsTrn <- 500;
dataLength <- 1;
modelData <- data.frame(matrix(ncol = 8, nrow= 0))
colnames(modelData) <- c("Subj", "Cond", "line", "Trial", "Ts", "Tp", "MaxTrial", "Main")

##----------------------------------------------------------------------------
## Model functions

## get Ts from data
experimentData <-read.csv(file ="dataJS.csv", header = TRUE);
for (i in seq(1, 6)){
  shortIntervals <- unique(experimentData[which(experimentData$Cond==1 & experimentData$Subj==i), c('Ts')])
  mediumIntervals <- unique(experimentData[which(experimentData$Cond==2 & experimentData$Subj==i), c('Ts')])
  longIntervals <- unique(experimentData[which(experimentData$Cond==3 & experimentData$Subj==i), c('Ts')])
}

trainingPhase <- function(participantInfo){
  if (participantInfo$condition == 1){
    intervals <- shortIntervals;
  }
  if (participantInfo$condition == 2){
    intervals <- mediumIntervals;
  }
  if (participantInfo$condition == 3){
    intervals <- longIntervals;
  }
  k <- 1;
  tTimes <- sample(intervals, nTrialsTrn, replace=T); 
  while (k < nTrialsTrn+1){
    ts <- as.numeric(tTimes[k]);
    
    participantInfo$Time <- participantInfo$Time + 1 + runif(1, 0.25, 0.85) + 0.1 + ts + 0.1 
    
    cPulses <- as.numeric(TimeToPulses(ts));
    
    ## check DM
    
    ##cPulses <- blendedChunks(participantInfo)
    tp <- as.numeric(PulsesToTime(cPulses));
    
    ## add to DM if within correct margin
    if(tp < 1.05*ts && tp > 0.95*ts){
      participantInfo$DM <- add.encounter(participantInfo$DM, cPulses, participantInfo$Time);
    }
    
    participantInfo$Time <- participantInfo$Time + 0.05 + tp
    
    participantInfo$data[k, ] <- c(participantInfo$subjectN, participantInfo$condition, length(modelData[,1])+k, k, ts, tp, nTrialsExp+nTrialsTrn, "FALSE");
    dataLength = dataLength+1;
    k = k + 1;
  }
  return(participantInfo)
}

experimentalPhase <- function(participantInfo){
  if (participantInfo$condition == 1){
    intervals <- shortIntervals;
  }
  if (participantInfo$condition == 2){
    intervals <- mediumIntervals;
  }
  if (participantInfo$condition == 3){
    intervals <- longIntervals;
  }
  i <- 1;
  tTimes <- sample(intervals, nTrialsExp, replace=T); 
  while (i < nTrialsExp+1){
    ts <- as.numeric(tTimes[i]);
    
    participantInfo$Time <- participantInfo$Time + 1 + runif(1, 0.25, 0.85) + 0.1 + ts + 0.1
    
    cPulses <- as.numeric(TimeToPulses(ts));
    
    ## check DM
    cPulses <- blendedValue(participantInfo)
    
    
    tp <- as.numeric(PulsesToTime(cPulses));
    
    ## add to DM if within correct margin
    if(tp < 1.05*ts && tp > 0.95*ts){
      participantInfo$DM <- add.encounter(participantInfo$DM, cPulses, participantInfo$Time);
    }
    
    participantInfo$Time <- participantInfo$Time + 0.05 + tp
    
    ## add to final data
    participantInfo$data[i,] <- c(participantInfo$subjectN, participantInfo$condition, length(modelData[,1])+i, nTrialsTrn+i, ts, tp, nTrialsExp+nTrialsTrn, "TRUE");
    dataLength = dataLength+1;
    i = i + 1;
  }
  return(participantInfo)
}

infoParticipant <- function(number, condition){
  list(DM <- create.dm(30,nTrialsExp+nTrialsTrn), condition=condition, subjectN = number, Time=0, data=data.frame(matrix(ncol = 8, nrow = nTrialsExp)));
}
##----------------------------------------------------------------------------
## Model
participantsC <- sample(c(1,2,3), nParticipants, replace=T)
j <- 1
while (j < nParticipants+1) {
  participantInfo <- infoParticipant(j, participantsC[j]);
  participantInfo$DM <- create.dm(30, nTrialsExp+nTrialsTrn);
  l <- 0
  colnames(participantInfo$data) <- c("Subj", "Cond", "line", "Trial", "Ts", "Tp", "MaxTrial", "Main")
  while (l < 3){
    participantInfo <- trainingPhase(participantInfo);
    modelData <- rbind(modelData, participantInfo$data);
    
    participantInfo <- experimentalPhase(participantInfo);
    modelData <- rbind(modelData, participantInfo$data);
    
    if (participantInfo$condition == 3){
      participantInfo$condition <- 1
    }
    else{
      participantInfo$condition = participantInfo$condition + 1;
    }
    l = l + 1;
  }
  j = j + 1
}
modelData
##----------------------------------------------------------------------------
## plot model data

brown <- "#8b4513";
red <- "#ff1100";
black <- "#000000";
brownT <- "#8b451322";
redT <- "#ff110022";
blackT <- "#00000022";

par(mfrow=c(2,1))

plotMDat <- with(modelData, aggregate(list(Tp=as.numeric(Tp)),list(Ts=Ts,Cond=Cond), mean))
yrange <- range(as.numeric(plotMDat$Ts))*c(.95,1.05)

with(plotMDat[plotMDat$Cond==3,],plot(Ts,Tp,type="b",col=red,lwd=2,ylim=yrange,xlim=yrange,main="Model"))
with(plotMDat[plotMDat$Cond==2,],lines(Ts,Tp,type="b",col=brown,lwd=2,ylim=yrange,xlim=yrange))
with(plotMDat[plotMDat$Cond==1,],lines(Ts,Tp,type="b",col=black,lwd=2,ylim=yrange,xlim=yrange))

lines(c(yrange[1],yrange[2]),c(yrange[1],yrange[2]),col="darkgrey",lty=2)

with(modelData[modelData$Cond==3,],points(jitter(as.numeric(Ts)),Tp,col=redT,pch=".",cex=3))
with(modelData[modelData$Cond==2,],points(jitter(as.numeric(Ts)),Tp,col=brownT,pch=".",cex=3))
with(modelData[modelData$Cond==1,],points(jitter(as.numeric(Ts)),Tp,col=blackT,pch=".",cex=3))


datJS <- experimentData

plotDatJS <- with(datJS,aggregate(list(Tp=Tp),list(Ts=Ts,Cond=Cond),mean))

yrange <- range(plotDatJS$Ts)*c(.95,1.05)

with(plotDatJS[plotDatJS$Cond==3,],plot(Ts,Tp,type="b",col=red,lwd=2,ylim=yrange,xlim=yrange,main="J&S All"))
with(plotDatJS[plotDatJS$Cond==2,],lines(Ts,Tp,type="b",col=brown,lwd=2,ylim=yrange,xlim=yrange))
with(plotDatJS[plotDatJS$Cond==1,],lines(Ts,Tp,type="b",col=black,lwd=2,ylim=yrange,xlim=yrange))

lines(c(yrange[1],yrange[2]),c(yrange[1],yrange[2]),col="darkgrey",lty=2)

with(datJS[datJS$Cond==3,],points(jitter(Ts),Tp,col=redT,pch=".",cex=3))
with(datJS[datJS$Cond==2,],points(jitter(Ts),Tp,col=brownT,pch=".",cex=3))
with(datJS[datJS$Cond==1,],points(jitter(Ts),Tp,col=blackT,pch=".",cex=3))


## ---------------------------------------------------------------------------
##
## What you have to do:
##
## Write the code that simulates the experiment, i.e., every so many
## miliseconds a new item is presented, which has to be stored in DM.
##
## Write the code that performs the rehearsals, and interweave that
## with the stimulus presentation.
##
## Write the code that does the final recall. Keep in mind that that
## phase also takes time...
##
## Put everything in a nice plot.
##
## Hand in the R code + a PDF version of your plot. Make sure that the
## first couple of lines of your R code contain exact instructions on
## how to run your R code to reprodruce the plot. If your R code does
## not include those instructions, we will assume that the code
## doesn't work.
##
## ---------------------------------------------------------------------------
