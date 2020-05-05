setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Parameters and variables created for the experiment
sampleSize = 3
t = c(11, 100)
a = c(1.1, 1.02)
b = 0.015
s = c(b*a[1], b*a[2])
e = 2.71828
noise = 1.2 
decay = -0.5
participantDat = data.frame(matrix(ncol = 4, nrow = 0))

#load up csv file
experimentDat <-read.csv(file ="dataJS.csv", header = TRUE)

getDat <- function(subjectNumber){
  shortIntervals <- experimentDat[which(experimentDat$Cond==1 & experimentDat$Subj==subjectNumber), c('Ts')]
  mediumIntervals <- experimentDat[which(experimentDat$Cond==2 & experimentDat$Subj==subjectNumber), c('Ts')]
  longIntervals <- experimentDat[which(experimentDat$Cond==3 & experimentDat$Subj==subjectNumber), c('Ts')]
  datList <<- list(shortIntervals = shortIntervals, mediumIntervals = mediumIntervals, longIntervals = longIntervals, subjectNumber = subjectNumber)
}

#experimental design
expDesign <- function(subjectNumber){
  DM <- create.dm(50, 1000)
  Time <- 0
  short <- sample(datList$shortIntervals, 500)
  medium <- sample(datList$mediumIntervals, 500)
  long <- sample(datList$longIntervals, 500)
  randomizedCond <- sample(c(1, 2, 3))
  list(DM = DM, short = short, medium = medium, long = long, randomizedCond = randomizedCond, subjectNumber = subjectNumber, Time = Time)
}
# Time, Declerative Memory, and Blending Functions(PR inside)  ---------------------------------

#actr.noise function given by instructors
actr.noise <- function(s, n=1) {
  rand <- runif(n,min=0.0001,max=0.9999)
  s * log((1 - rand ) / rand)
}

#converts time(in miliseconds) to pulses
timeToPulses <- function(i, x){
  totalTime <- 0
  pulseCount <- 0
  while(totalTime < x){
    t[i] = a[i]*t[i]+actr.noise(s[i]*t[i])
    totalTime = totalTime + t[i]
    pulseCount = pulseCount + 1
  }
  pulseCount
  
}

#converts pulses to time(in seconds)
pulsesToTime <- function(i, x){
  totalTime <- 0
  pulseCount <- 0
  while (pulseCount < x){
    t[i] = a[i]*t[i]+actr.noise(s[i]*t[i])
    totalTime = totalTime + t[i]
    pulseCount = pulseCount + 1
  }
  totalTime
}

create.dm <- function(chunks,encounters) {
  if (chunks > 52) {
    stop("Only up to 52 chunks allowed.")
  }
  DM <- array(NA,c(chunks,encounters))
  row.names(DM) <- c(letters,LETTERS)[1:chunks]
  DM
}


#encounter add
add.encounter <- function(subjectInfo,pulse) {
  tmp <- subjectInfo$DM[pulse, ]
  subjectInfo$DM[pulse,sum(!is.na(tmp))+1] <- subjectInfo$Time
  subjectInfo
}


#encounter get
get.encounters <- function(subjectInfo,i) {
  tmp <- subjectInfo$DM[i,]  
  tmp[!is.na(tmp)]         #In the ith row, this function checks each data slot for TRUE vs FALSE values and returns only the value of data slots which turn out to be TRUE. It removes the columns where the dataslot for that row is NA, thus FALSE. 
}


#calculate activation
actr.B <- function(encounters, current_time) {
  if ( current_time < min(encounters)) {
    return(NA)
  } else {
    log(sum(( current_time - (encounters[encounters< current_time]))^-decay))
  }
}

#blending function
blendedChunks <- function(subjectInfo)
{
  
  
  blendedPulseValue <- sum(curActList[,2] * RP)
  
}


# Training and Test Trials  ---------------------------------
trainSubjects <- function(conditionNumber, subjectInfo)
{
  if(conditionNumber == 1){
    conditionSample <- subjectInfo$short
  } 
  else if(conditionNumber == 2){
    conditionSample <- subjectInfo$medium
  } 
  else if(conditionNumber == 3){
    conditionSample <- subjectInfo$long
  }
  
  for (i in seq(1, 500)) 
  {
    ts <- conditionSample[i] 
    subjectInfo$Time <- subjectInfo$Time + 1000 + runif(1, 250, 850) + 100 + ts + 50
    pulse <- timeToPulses(1,ts) 
    subjectInfo <- add.encounter(subjectInfo, pulse)
  }
  subjectInfo
}


testSubjects <- function(conditionNumber, subjectInfo){
  
  if(conditionNumber == 1){
    conditionSample <- subjectInfo$short
  } 
  else if(conditionNumber == 2){
    conditionSample <- subjectInfo$medium
  } 
  else if(conditionNumber == 3){
    conditionSample <- subjectInfo$long
  }
  
  for (i in seq(1, length(conditionSample))) 
  {
    ts <- conditionSample[i]
    pulse <- timeToPulses(1, ts)
    subjectInfo$Time <- subjectInfo$Time + 1000 + runif(1, 250, 850) + 100 + ts + 50
    subjectInfo <- add.encounter(subjectInfo, pulse)
  }  
  
  curActList <- NULL 
  curAct <- NULL
  
  for (i in seq(1, nrow(subjectInfo$DM))) 
  {
    encountersOfSameChunk <- sum(!is.na(subjectInfo$DM[i, ]))
    if (encountersOfSameChunk > 0)
    {
      encounters <- get.encounters(subjectInfo, i)  #collects creation time of chunks in the ith row in subjectInfo$DM into encounters vector
      
      curAct <- actr.B(encounters, subjectInfo$Time) #gives the activation value of an ith row in DM
      curActList <- rbind(curActList, c(activation = curAct, relevantpulse = i))
      
    }
    curActList
  }
  blendedPulseValue <- NULL
  
  totalAct <- 0
  denominator <- 0
  RP <- 0

  for(i in curActList[,2])
  {
    if(i > mean(curActList[,1]))
    {
      for (j in curActList[,1])
      {
        curActList = cbind(curActList, penalty = 5 )
        denominator = denominator + (e^((j-2)/ noise))
      }
    }
    else
    { 
      for (j in curActList[,1])
      {
        denominator = denominator + (e^((j)/ noise))
      }
    }
    RP <- (e^(curActList[,1] / noise))/denominator
  }
  
  
  blendedPulseValue <- blendedChunks(subjectInfo)
  tp <- pulsesToTime(1, blendedPulseValue)
  
  subjectInfo$participantDat <- rbind(subjectInfo$participantDat, c(subjectInfo$subjectNumber, conditionNumber, ts, tp))
}

subjectInfo
}


main <- function(){
  
  simulationDat <- data.frame(matrix(ncol = 4, nrow = 0))
  
  
  for (i in seq(sampleSize)) 
  {
    subjectNumber <- i
    getDat(subjectNumber)
    subjectInfo <- expDesign(subjectNumber)
    
    pulse = 0
    
    for (j in subjectInfo$randomizedCond) 
    {
      conditionNumber <- j
      subjectInfo$DM <- create.dm(50, 1000)
      subjectInfo <- trainSubjects(conditionNumber, subjectInfo)
      subjectInfo <- testSubjects(conditionNumber, subjectInfo) 
      
    }
    
    simulationDat <- rbind(simulationDat, subjectInfo$participantDat)
  }
  
  colnames(subjectInfo$participantDat) <- c("Subject_Number ", "Condition", "Ts", "Tp")
  participantDat <- data.frame(subjectInfo$participantDat)
  #participantDat$Response[is.nan(participantDat$Response)] <- participantDat$Prior[is.nan(participantDat$Response)]
  participantDat
}

plotting <- function(dataframe)
{
  
  brown <- "#8b4513";
  red <- "#ff1100";
  black <- "#000000";
  brownT <- "#8b451322";
  redT <- "#ff110022";
  blackT <- "#00000022";
  
  ## ---
  datJS <- dataframe
  par(mfrow=c(1,1))
  
  plotDatJS <- with(datJS,aggregate(list(Tp=Tp),list(Ts=Ts,Condition=Condition),mean))
  yrange <- range(plotDatJS$Ts)*c(.95,1.05)
  
  with(plotDatJS[plotDatJS$Cond==3,],plot(Ts,Tp,type="b",col=red,lwd=2,ylim=yrange,xlim=yrange,main="J&S All"))
  with(plotDatJS[plotDatJS$Cond==2,],lines(Ts,Tp,type="b",col=brown,lwd=2,ylim=yrange,xlim=yrange))
  with(plotDatJS[plotDatJS$Cond==1,],lines(Ts,Tp,type="b",col=black,lwd=2,ylim=yrange,xlim=yrange))
  
  lines(c(yrange[1],yrange[2]),c(yrange[1],yrange[2]),col="darkgrey",lty=2)
  
  with(datJS[datJS$Cond==3,],points(jitter(Ts),Tp,col=redT,pch=".",cex=3))
  with(datJS[datJS$Cond==2,],points(jitter(Ts),Tp,col=brownT,pch=".",cex=3))
  with(datJS[datJS$Cond==1,],points(jitter(Ts),Tp,col=blackT,pch=".",cex=3))    
}

plotting(main())