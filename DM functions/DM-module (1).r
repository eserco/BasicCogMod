## ---------------------------------------------------------------------------
##
## Template for DM assignment Cognitive Modeling Course
##
## Hedderik van Rijn, 091119
##
## ---------------------------------------------------------------------------

## List with parameter values:

params <- list()

params$d <- .5

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

## ---------------------------------------------------------------------------

## Some example code to get you started:

params$num.chunks <- 4
params$max.num.encounters <- 100
params$duration <- 1000

## Create a DM with room for num.chunks chunks with up to num.encounters encounters per chunk.
DM <- create.dm(params$num.chunks,params$max.num.encounters)

## Add 100 encounters to DM
for (i in 1:250) {
  DM <- add.encounter(DM,letters[trunc(runif(1,1,params$num.chunks+1))],runif(1,0,params$duration))
}

## Calculate the activation per chunk: 

for (i in letters[1:params$num.chunks]) {
  cat("Activation of chunk ",i," is ");
  cat(actr.B(get.encounters(DM,i),params$duration),"\n");
}

## Plot activation per chunk:

par(mfrow=c(2,2))
for (i in letters[1:params$num.chunks]) {
  plot(1,1,type="n",xlim=c(1,params$duration),ylim=c(-4,4),xlab="Time",ylab="Activation",main=paste("chunk",i))
  
  curencounters <- get.encounters(DM,i)
  if (length(curencounters)>0) {
    lines(actr.B(curencounters,1:params$duration))
  }
}

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
