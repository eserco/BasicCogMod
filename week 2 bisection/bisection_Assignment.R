  #Load exp data into variables
  experimentalData <-list(
  expdata28 = c(0.02, 0, 0.12, 0.5, 0.84, 0.91, 1),
  expdata36 = c(0.08, 0.1, 0.2, 0.45, 0.74, 0.86, 0.95),
  expdata412 = c(0, 0.07, 0.22, 0.46, 0.69, 0.86, 0.92))
  intervals <- list(
  interval28 = c(2, 2.52, 3.18, 4, 5.04, 6.35, 8),
  interval36 = c(3, 3.37, 3.78, 4.24, 4.76, 5.34, 6),
  interval412 = c(4, 4.8, 5.77, 6.93, 8.32, 9.99, 12))
  
  #Variables created for the experiment
  longInterval28 <-c()
  longInterval36 <-c()
  longInterval412 <-c()
  tempTable28 <-c()
  tempTable36 <-c()
  tempTable412 <-c()
  meanLong28 = 0
  meanLong36 = 0
  meanLong412 = 0
  longInterval28 <-c()
  longInterval36 <-c()
  longInterval412 <-c()
  meanLong = 0
  lowerlist <- c()
  upperlist <- c()
  guesses <- c()
  meanLower = 0
  meanUpper = 0
  stimulus <- c()
  proportion <-c()
  stimuli = 0
  pulseConverted = 0
  threshold = 0
  totalTime = 0
  pulseCount = 0
  
  #Parameters for time estimation functions
  t = c(11, 100)
  a = c(1.1, 1.02)
  b = 0.015
  s = c(b*a[1], b*a[2])
  anchorProb = c(0.3, 0.7, 0.7, 0.7, 0.7, 0.7, 0.3)
  
  #actr.noise function given by instructors
  actr.noise <- function(s,n=1) {
    rand <- runif(n,min=0.0001,max=0.9999)
    s * log((1 - rand ) / rand)
  }
  
  #converts time to pulses
  timeToPulses <- function(i, x){
    while(totalTime < x*1000){
      t[i] = a[i]*t[i]+actr.noise(s[i]*t[i])
      totalTime = totalTime + t[i]
      pulseCount = pulseCount + 1
    }
    pulseCount
  }
  
  #converts pulses to time
  pulsesToTime <- function(i, x){
    for(j in 1:x){
      t[i] = a[i]*t[i]+actr.noise(s[i]*t[i])
      totalTime = totalTime + t[i]
      pulseCount = pulseCount + 1
    }
    print(pulseCount)
    print(totalTime/1000)
  }
  
  #subject learn upper and lower anchor points for given interval for 10 times
  learningPhase <- function(lower, upper){
        for (i in 1:200){  #each subject is trained for 10 times and I set subject number to 20 so in total we have 10*20 = 200 learning trials
          lowerlist <<- append(lowerlist, timeToPulses(1, lower))
          upperlist <<- append(upperlist, timeToPulses(1, upper))
        }
        meanLower <<- mean(lowerlist)
        meanUpper <<- mean(upperlist)
        lowerlist <<- c()
        upperlist <<- c()
  }
  
  #subject are randomly asked to recognize each interval being either long or not long
  expTrials <- function(intervalLevel){
    for (j in 1:100){
      stimuli <<- sample(unlist(intervals[intervalLevel]), 1, prob = anchorProb)
      pulseConverted <<- timeToPulses(1, stimuli)
      if (abs(pulseConverted - meanLower) > abs(pulseConverted - meanUpper)){
        long <<- 1
      }    
      else{
        long <<- 0
      }
      #data fame is created with each interval having corresponding long proportion value which is either 1 or 0
      stimulus <<- append(stimulus, c(stimuli))
      proportion <<- append(proportion, c(long))
      tableDat <<- data.frame(stimulus, proportion)

    }
  }  
  
  learningPhase(2,8)
  expTrials(1)  
  for (i in intervals$interval28){
    tempTable28 <<- subset(tableDat, stimulus == i, select = c("stimulus","proportion"))
    meanLong28 <<- mean(tempTable28[,"proportion"])
    longInterval28 <<- data.frame(rbind(longInterval28, c(i, meanLong28)))
  }
  tableDat <- NULL
  stimulus <- NULL
  proportion <- NULL
  learningPhase(3,6)
  expTrials(2)  
  for (i in intervals$interval36){
    tempTable36 <<- subset(tableDat, stimulus == i, select = c("stimulus","proportion"))
    meanLong36 <<- mean(tempTable36[,"proportion"])
    longInterval36 <<- data.frame(rbind(longInterval36, c(i, meanLong36)))
  }
  tableDat <- NULL
  stimulus <- NULL
  proportion <- NULL
  learningPhase(4,12)
  expTrials(3)
  for (i in intervals$interval412){
    tempTable412 <<- subset(tableDat, stimulus == i, select = c("stimulus","proportion"))
    meanLong412 <<- mean(tempTable412[,"proportion"])
    longInterval412 <<- data.frame(rbind(longInterval412, c(i, meanLong412)))
  }
  
  plot(longInterval28$X1, longInterval28$X2, type = "o", col ="blue", axes = FALSE, ann=FALSE)
  title(main ="Bisection 2 to 8")
  axis(2, at=seq(0,1, by = 0.25))
  axis(1, at=seq(2,8, by = 2))
  lines(intervals$interval28, experimentalData$expdata28,col="red")
  plot(longInterval36$X1, longInterval36$X2, type = "o", col ="blue", axes = FALSE, ann=FALSE)
  title(main ="Bisection 3 to 6")
  axis(2, at=seq(0,1, by = 0.25))
  axis(1, at=seq(3,6, by = 1))
  lines(intervals$interval36, experimentalData$expdata36,col="red")
  plot(longInterval412$X1, longInterval412$X2, type = "o", col ="blue", axes = FALSE, ann=FALSE)
  title(main ="Bisection 4 to 12")
  axis(2, at=seq(0,1, by = 0.25))
  axis(1, at=seq(4,12, by = 2))
  lines(intervals$interval412, experimentalData$expdata412,col="red")