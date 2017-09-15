remove(list = ls())
library(magrittr)
library(dplyr)
library(scatterplot3d)
library(lubridate)
library(forecast)

###---ALL FUNCTIONS---###

#Function for data cleaning
DataClean <- function(file) {
  
  setwd(dir="C:\\Users\\JAIVIGNESH\\Desktop\\Data Science - Aegis\\Capstone project\\Code\\Mine\\")
    
  playerData <- read.csv(file,stringsAsFactor=FALSE,na.strings=c(NA,"-"))
  playerData <- playerData[,-c(1,11)]
  df <- playerData
  
  # Remove rows where the batsman 'did not bat' - DNB
  a <- df$Runs != "DNB"
  batsman <- df[a,]
  
  # Remove rows with 'TDNB'
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]
  
  # Remove rows with absent
  d <- batsman$Runs != "absent"
  batsman <- batsman[d,]
  
  # Remove the "* indicating not out
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs))
  
  #c <- complete.cases(batsman)
  
  #batsman$Mins[batsman$Mins==NA] <- ""
  
  #Regression for predicting missing Mins
  predicted_mins <- lm(Mins ~ Runs+BF,data = batsman[!is.na(batsman$Mins),])
  batsman$Mins[is.na(batsman$Mins)] <- round(predict(predicted_mins, batsman[is.na(batsman$Mins),]),0)

  
  #Computing missing SRs
  NewSR=round((batsman$Runs/batsman$BF)*100,2)
  NewSR <- replace(NewSR, is.na(NewSR), 0)
  batsman$SR=NewSR 
  
  batsmanComplete <- batsman
  list(val=dim(batsmanComplete),names = names(batsmanComplete),h=head(batsmanComplete))
  
  #Return the data frame 
  batsmanComplete
  #write.csv(batsmanComplete,"Clean.csv")
}

#Function for capturing the basic statistics
Stats <- function(file,name){
  
  batsman <- NULL
  batsman <- DataClean(file)
  
  noofMatches=nrow(batsman)
  #Runs
  #avgruns=round(mean(batsman$Runs),0)
  #medruns=median(batsman$Runs)
  #IQRruns=IQR(batsman$Runs)
  #minruns=min(batsman$Runs)
  #maxruns=max(batsman$Runs)
  
  avgruns=paste("Average runs for ",name," - ",round(mean(batsman$Runs),0))
  medruns=paste("Median of runs for ",name," - ",median(batsman$Runs))
  IQRruns=paste("IQR of runs for ",name," - ",IQR(batsman$Runs))
  minruns=paste("Lowest runs for ",name," - ",min(batsman$Runs))
  maxruns=paste("Highest runs for ",name," - ",max(batsman$Runs))
  
  print(avgruns)
  print(medruns)
  print(IQRruns)
  print(minruns)
  print(maxruns)
}

#Function to remove outliers
RemoveOutlier <- function(file) {
  batsman <- NULL
  batsman <- DataClean(file)

  var <- batsman$Runs
  var_name <- eval(substitute(var),eval(batsman))
  outlier <- boxplot.stats(var_name)$out
  
  var_name <- !var_name %in% outlier
  batsman <-  batsman[var_name,]
  batsman[complete.cases(batsman),]
  
}

#Function to plot the batting performance against grounds
batsmanAvgRunsGround <- function(file, name){
  
  batsman <-Ground <- Runs <- NULL
  batsman <- DataClean(file)
  
  # use dplyr's summarise function to group by Ground and calculate mean & count
  meanRuns <- batsman %>% group_by(Ground) %>% summarise(m= mean(Runs))
  countInnings <- batsman %>% group_by(Ground) %>% summarise(len=length(Runs))
  
  # Set the margins
  par(mar=c(9,4,3,2))
  ground <- as.vector(meanRuns$Ground)
  values <- paste(ground,"-",countInnings$len)
  atitle <- paste(name,"'s Average Runs at Ground")
  barplot(meanRuns$m,names=values,las=2,ylab="Average Runs", col=rainbow(length(meanRuns$m)),main=atitle,cex.names=0.8)
  abline(h=50,lty=3,lwd=2)
  abline(h=100,lty=3,lwd=2,col="blue")
  
  mtext("Ground - No of innings", side=1, line=7.5, adj=1.0, cex=1.0, col="black")
  mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=0, adj=1.0, cex=0.8, col="blue")

}

#Function to plot the batting performance against playing position
batsmanAvgRunsPosition <- function(file, name){
  
  batsman <-Pos <- Runs <- NULL
  batsman <- DataClean(file)
  
  # use dplyr's summarise function to group by Position and calculate mean & count
  meanRuns <- batsman %>% group_by(Pos) %>% summarise(m= mean(Runs))
  countInnings <- batsman %>% group_by(Pos) %>% summarise(len=length(Runs))
  
  # Set the margins
  par(mar=c(9,4,3,2))
  posM <- as.vector(meanRuns$Pos)
  values <- paste(posM,"-",countInnings$len)
  atitle <- paste(name,"'s Average Runs per position")
  barplot(meanRuns$m,names=values,las=2,ylab="Average Runs", col=rainbow(length(meanRuns$m)),main=atitle,cex.names=0.8)
  abline(h=50,lty=3,lwd=2)
  abline(h=100,lty=3,lwd=2,col="blue")
  
  
  mtext("Position - No of innings", side=1, line=7.5, adj=1.0, cex=1.0, col="black")
  mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=0, adj=1.0, cex=0.8, col="blue")
}

#Function to plot the batting performance against oppositions
batsmanAvgRunsOpposition <- function(file, name){
  
  Opposition <-Runs <- NULL
  batsman <- DataClean(file)
  
  # Use dplyr's summarise to group by Opposition and compute mean runs and count
  meanRuns <- batsman %>% group_by(Opposition) %>% summarise(m= mean(Runs))
  countInnings <- batsman %>% group_by(Opposition) %>% summarise(len=length(Runs))
  
  # Set margins
  par(mar=c(9,4,3,2))
  opposition <- as.vector(meanRuns$Opposition)
  values <- paste(opposition,"-",countInnings$len)
  atitle <- paste(name,"'s Average Runs versus Opposition")
  barplot(meanRuns$m,names=values,las=2,ylab="Average Runs", col=rainbow(length(meanRuns$m)),main=atitle)
  abline(h=50,lty=2,lwd=2)
  
  
  mtext("Opposition - No of innings", side=1, line=7.5, adj=1.0, cex=1.0, col="black")
  mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=0, adj=1.0, cex=0.8, col="blue")
}

#Function to plot the batting performance as a combined box plot and histogram
batsmanPerfBoxHist <- function(file, name="A Hitter") {
  
  df <- DataClean(file)
  atitle <- paste(name,"'s", " - Runs Frequency vs Runs")
  
  # Set the layout and the margins. 
  nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  heights = c(1,3))
  par(mar=c(2,2,1,1))
  
  # Draw the boxplot
  boxplot(df$Runs, horizontal=TRUE,  outline=TRUE,ylim=c(0,max(df$Runs)), frame=F, col = "green1")
  
  # Draw lines showing the mean and meadian
  abline(v=median(df$Runs),col="blue",lwd=3.0)
  abline(v=mean(df$Runs),col="red",lwd=3.0)
  mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=4, adj=1.0, cex=0.8, col="blue")
  
  # Create a vector from 0 with intervals of 10 for the intervals
  maxi <- (max(df$Runs/10) + 1) *10
  v <- seq(0,maxi,by=10)
  
  
  # Draw a histogram
  hist(df$Runs,breaks=v,xlab="Runs",ylab="Runs frequency", main = atitle,labels=TRUE,col="grey")
  
  # Draw the median, mean, 1st and 3rd quantiles
  abline(v=median(df$Runs),col="blue",lwd=3.0)
  abline(v=mean(df$Runs),col="red",lwd=3.0)
  abline(v=quantile(df$Runs,.25),col="black",lwd=3.0,lty=2)
  abline(v=quantile(df$Runs,.75),col="black",lwd=3.0,lty=2)
  
  # Draw a rug below the histogram
  rug(df$Runs,col="blue",lwd=2)
  
  mn <- paste("Mean runs over career:",round(mean(df$Runs),2))
  md <- paste("Median runs over career:", round(median(df$Runs),2))
  
  # Get the value of count to determine the height of graph
  a <- hist(df$Runs, breaks=v,plot=FALSE)
  ht <- max(a$counts)
  
  text(200,ht-15,mn,col="brown")
  text(200,ht-20,md,col="brown")
  
  
  mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=4, adj=1.0, cex=0.8, col="blue")
  # reset the layout
  par(mfrow=c(1,1))
  
}

#Function to plot the scoring range
batsmanRunsRanges <- function(file, name) {
  # Clean file
  df <- DataClean(file)
  
  # Divide the runs into 20 run ranges from 0 to 400
  f <- cut(df$Runs, breaks=seq(from=0,to=400,by=20))
  
  # Create a table
  g <- table(f)
  
  # Create a vector to store the runs frequency
  v <- as.vector(g)
  
  # Compute percentage of runs in the overall run total
  percentRuns <- (g/sum(g))*100
  runfreq <- c(name, round(percentRuns,1), "\n")
  
  # Add a title
  atitle <- paste(name,"Runs %  vs Run ranges")
  
  # Plot the batting perormance 
  barplot(percentRuns, main = atitle ,xlab="Runs scored", ylab="% times runs scored in range (%)",ylim=c(0,100),col="blue")
  axis(side=2, at=seq(0, 100, by=5))
  
  mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=2, adj=1.0, cex=0.8, col="blue")  
  
}

#Function to plot the run frequency
batsmanRunsFreqPerf <- function(file, name) {
  
  df <- DataClean(file)
  
  # Create breaks in intervals of 10
  maxi <- (max(df$Runs/10) + 1) *10
  v <- seq(0,maxi,by=10)
  a <- hist(df$Runs,breaks=v,plot=FALSE)
  
  # Create mid points
  Runs <- a$mids
  RunFrequency <- a$counts
  df1 <- data.frame(Runs,RunFrequency)
  
  
  # Create a plot
  atitle <- paste(name,"'s", " Runs frequency vs Runs")
  plot(df1$Runs,df1$RunFrequency,pch=16,xlab="Runs",ylab="Runs Frequency", main=atitle)
  lines(df1$Runs,predict(loess(df1$RunFrequency~df1$Runs)),col="blue",lwd=3)
  mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=2, adj=1.0, cex=0.8, col="blue")
  
  
}

#Function to plot the scoring rate
batsmanScoringRateODTT <- function(file, name) {
  
  # Clean the batsman file and create a complete data frame
  batsman <- DataClean(file)
  
  atitle <- paste(name, "- Runs vs Balls Faced")
  
  # Make a scatter plot of balls faced and runs
  with(data=batsman,plot(BF,Runs,main=atitle))
  
  # Fit a second order polynomial used
  fit2 <- with(data=batsman,lm(Runs~poly(BF,2,raw=TRUE)))
  
  # Create seq from 0 to max batsman balls faced
  xx <- seq(from=0,to = max(batsman$BF),by=5)
  
  # Compute the predicted runs
  yy <- NULL
  for (i in seq_along(xx)) {
    yy[i] <- fit2$coefficients[3] * xx[i]^2 + fit2$coefficients[2] * xx[i] + fit2$coefficients[1] 
    
  }
  # Draw the predicted runs
  lines(xx,yy,col="blue",lwd=2.0)
  bf=50
  runs = fit2$coefficients[3] * bf^2 + fit2$coefficients[2] * bf + fit2$coefficients[1] 
  abline(v=50,lty=2,lwd=2,col="blue")
  abline(h=runs,lty=2,lwd=2,col="blue")
  
  bf=100
  runs = fit2$coefficients[3] * bf^2 + fit2$coefficients[2] * bf + fit2$coefficients[1] 
  abline(v=100,lty=3,lwd=2,col="red")
  abline(h=runs,lty=3,lwd=2,col="red")
  mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
  
  
}

#Function to plot the 3D plot for Runs  vs BF & Mins 
battingPerf3d <- function(file, name) {
  # Clean the batsman file and create a complete data frame
  batsman <- DataClean(file)
  # Make a 3 D plot and fit a regression plane
  atitle <- paste(name, "- Runs  vs BF & Mins")
  s <-with(data=batsman,scatterplot3d(BF,Mins,Runs, color=rgb(0,0,255,50,maxColorValue=255),
                                      xlab="Balls Faced",ylab="Minutes in crease",
                                      zlab="Runs scored", main=atitle,pch=16))
  
  # Fit a regression plabe
  fit <- with(data=batsman,lm(Runs ~ BF+Mins))
  
  # Draw the plane
  s$plane3d(fit)
  
  mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
  
  
}

#Function to plot the scoring likelihood
batsmanRunsLikelihood <- function(file, name) {
  
  Runs <- BF <-Mins <- Ground <-Wkts <- NULL
  batsman <- DataClean(file)
  data <- select(batsman,Runs,BF,Mins)
  
  # Use K Means with 3 clusters
  fit <- kmeans(data, 3)
  str <- paste(name,"'s Runs likelihood vs BF, Mins")
  # Create a 3D scatterplot
  s <-scatterplot3d(data$BF,data$Mins,data$Runs,color="lightblue",main=str,pch=20, xlab="Balls Faced",ylab="Minutes at crease",zlab="Runs scored")
  
  # Plot the centroids
  s$points3d(fit$centers[1,2],fit$centers[1,3],fit$centers[1,1],col="blue",type="h", pch=15,lwd=4)
  s$points3d(fit$centers[2,2],fit$centers[2,3],fit$centers[2,1],col="red",type="h", pch=15,lwd=4)
  s$points3d(fit$centers[3,2],fit$centers[3,3],fit$centers[3,1],col="black",type="h", pch=15,lwd=4)
  mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
  
  p1 <- (sum(fit$cluster==1)/length(fit$cluster)) * 100
  p2 <- (sum(fit$cluster==2)/length(fit$cluster)) * 100
  p3 <- (sum(fit$cluster==3)/length(fit$cluster)) * 100
  
  # Print the summary of the centroids
  
  cat("Summary of ",name,"'s runs scoring likelihood\n")
  cat("**************************************************\n\n")
  cat("There is a",round(p1,2), "% likelihood that",name," will make ",round(fit$centers[1,1]),
      "Runs in ",round(fit$centers[1,2]),"balls over",round(fit$centers[1,3])," Minutes \n")
  
  cat("There is a",round(p2,2), "% likelihood that",name," will make ",round(fit$centers[2,1]),
      "Runs in ",round(fit$centers[2,2]),"balls over ",round(fit$centers[2,3])," Minutes \n")
  
  cat("There is a",round(p3,2), "% likelihood that",name," will make ",round(fit$centers[3,1]),
      "Runs in ",round(fit$centers[3,2]),"balls over",round(fit$centers[3,3])," Minutes \n")
  
}

#Function to predict the runs
batsmanRunsPredict <- function(file, name, newdataframe) {
  batsman <- RemoveOutlier(file)
  
  # Fit a linear regression line between Runs and BF & Mins
  fit <- lm(as.numeric(Runs) ~ BF+Mins,data=batsman)
  
  # Predict based on the fitted model
  Runs <- predict(fit,newdata=newdataframe)
  
  newdataframe$Runs <- round(Runs,0)
  names(newdataframe) <- c("Balls Faced","Minutes",'Runs')
  newdataframe
}

#Function to forecast the runs
batsmanPerfForecast <- function(file, name) {
  b <- RemoveOutlier(file)
  
  # Read day, month and year
  date <- dmy(b$Start.Date)
  runs <- b$Runs
  
  # Create a training and a test set
  # Subset 90 percent of the rows of the time series
  rows <- length(runs)
  i <- floor(0.9 * rows)
  
  # Note the start/end month and year
  startMonth = month(date[1])
  startYear = year(date[1])
  endMonth = month(date[i])
  endYear = year(date[i])
  
  # Create training set with the 90 percent career 
  ts.train <- ts(runs, start = c(startYear,startMonth), end = c(endYear,endMonth),frequency=12)
  
  
  # Make a test set with the remaining 10 percent
  startMonth1 <- month(date[i+1])
  startYear1 = year(date[i+1])
  endMonth1 = month(date[rows])
  endYear1 = year(date[rows])
  
  ts.test <- ts(runs, start = c(startYear1,startMonth1), end = c(endYear1,endMonth1),frequency=12)
  
  # Fit a Holt Winters Model with the training set
  fit <-HoltWinters(ts.train)
  
  # Forecast based on the model
  fcast <- forecast(fit)
  atitle = paste(name,"-","Runs forecast" )
  plot(fcast,main=atitle,col="blue",lwd=1.5,xlab="Year",ylab="Runs scored")
  lines(ts.train,col="magenta")
  
  # Draw the test set
  lines(ts.test,col="red",lwd=1.5)
  
  vals <- c("forecasted runs","actual runs scored")
  col1 <- c("blue","red")
  legend(x="topleft",vals, lty=c(1,1),lwd=c(1.5,1.5),col=col1,bty="n",cex=0.8)
  acc=accuracy(fcast,ts.test)
  
  
  mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
  print(ts.train)
  print(ts.test)      
  print(fcast)
  print(acc)
}

#To plot dismissals in percentage
batsmanDismissals <- function(file, name="A Squarecut") {
  
  batsman <- DataClean(file)
  
  lbls <- NULL
  
  d <- batsman$Dismissal
  
  # Convert to data frame
  dismissal <- data.frame(table(d))
  par(mar=c(0,0,2,2))
  # Create a 3D pie chart
  lbls <- dismissal$d
  slices <- dismissal$Freq
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  atitle <- paste(name, "-Pie chart of dismissals")
  
  # Important note: Ensure the number of labels & slices match
  plotrix::pie3D(slices, labels=lbls,explode=0.1, main= atitle,pty="s",labelcex=0.8)
  
  mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue") 
  
}

#To Plot Win/loss contribution
batsmanContributionWonLost <- function(file,name="A Hitter") {
  
  result <- NULL
  playersp <- RemoveOutlier(file)
  
  won <- filter(playersp,result==1)
  lost <- filter(playersp,result==2 | result == 4 )
  won$status="won"
  lost$status="lost"
  wonLost <- rbind(won,lost)
  atitle <- paste(name,"- Runs in games won/lost-drawn")
  
  # Create boxplots
  boxplot(Runs~status,data=wonLost,col=c("red","green"),xlab="Status of game",
          ylab="Runs scored", main=atitle)
  
  
  a <- dim(won)
  b <- dim(lost)
  
  
  val1 <- paste(b[1], "games lost/drawn")
  val2 <- paste(a[1],"games won")
  vals <- list(val1,val2)
  legend(x="top", legend=vals, lty=c(1,1),   
         lwd=c(7,7),col=c("red","green"),bty="n")
  
  mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=1, col="blue")  
  
}

#Function to get most played position
getmode <- function(file) {
  
  batsman <- NULL
  batsman <- RemoveOutlier(file)
  
  var <- batsman$Pos
  
  uniqv <- unique(var)
  
  uniqv[which.max(tabulate(match(var, uniqv)))]
}


Batting_ScoreCard <- function(file,name){
  
  batsman <- RemoveOutlier(file)
  
  fours <- batsman %>% summarise(f= sum(X4s))
  sixes <- batsman %>% summarise(s= sum(X6s))
  BF <- batsman %>% summarise(s= sum(BF))
  
  #HardHitterScore
  Boundries <- 4*fours+6*sixes
  HardHitterScore <- round(Boundries/BF,2)
  
  #Finisher
  countNotOut<-batsman$Dismissal[batsman$Dismissal=="not out"]
  countInnings <- batsman %>% summarise(len=length(Runs))
  Finisher <- round(length(countNotOut)/countInnings,2)
  
  #FastScorer
  TotalRuns <- batsman %>% summarise(runs=sum(Runs))
  TotalBalls <- batsman %>% summarise(balls=sum(BF))
  FastScorer <- round(TotalRuns/TotalBalls,2)
  
  #Consistent
  countOut<- batsman$Dismissal[batsman$Dismissal!="not out"]
  Consistent <- round(TotalRuns/length(countOut),2)
  
  #RunningBetweenWickets
  CountNoFours <- batsman$X4s[batsman$X4s=="0"]
  CountNoSixes <- batsman$X6s[batsman$X6s=="0"]
  CountBoundries <- fours+sixes
  CountNoBoundries<-TotalBalls-CountBoundries
  RunningBetweenWickets <- round((TotalRuns-Boundries)/CountNoBoundries,2)
  
  
  newDF <- data.frame(name,HardHitterScore,Finisher,FastScorer,Consistent,RunningBetweenWickets)
  names(newDF) <- c("Player","HardHitter Score","Finisher Score","FastScorer","Consistent","Running Between Wickets")
  newDF
}


Batting_Metrics <- function(file,name,team,role){
  
  batsman <- RemoveOutlier(file)
  
  #Games Played
  countInnings <- batsman %>% summarise(len=length(Runs))
  
  #Total Runs
  TotalRuns <- batsman %>% summarise(runs=sum(Runs))
  
  #Total Balls Faced
  TotalBalls <- batsman %>% summarise(balls=sum(BF))
  
  #Total Dismissals
  countOut<- batsman$Dismissal[batsman$Dismissal!="not out"]
  TotalDismissal <- length(countOut)

  #Total Boundries
  fours <- batsman %>% summarise(f= sum(X4s))
  sixes <- batsman %>% summarise(s= sum(X6s))
  TotalBoundries <- fours+sixes
  
  #Batting Average
  BatAvg <- round(TotalRuns/TotalDismissal,2)
  
  #Batting Strike Rate
  BatSR <- round(TotalRuns/TotalBalls,2)
 
  #Average Contribution
  

  #Percentage Boundaries Hit
  PerBoundariesHit <- round(TotalBoundries/TotalBalls,2)
  
  #Position
  Position <- getmode(file)
  
  
  #Number of Wins
  Wins <- batsman$result[batsman$result=="1"]
  TotalWins <- length(Wins)
  
  #Percentage Wins (Y) 
  PerWin <- round(TotalWins/countInnings,2)
  
  Team <- team
  Role <- role
  Name <- name
  
  
  newDF <- data.frame(Name,Team,Role,Position,countInnings,TotalRuns,TotalBalls,BatAvg,BatSR,fours,sixes,TotalBoundries,PerBoundariesHit,TotalDismissal,TotalWins,PerWin)
  names(newDF) <- c("Player","Team","Role","Position","Game Played","Total Runs Scored","Total Balls Faced","Batting Average","Batting Strike Rate","Fours","Sixes","Total Boundries","Percentage Boundaries Hit","Total Dismissal","Total Wins","Percentage Wins")
  newDF
  
  
}




####------CALLING THE FUNCTIONS####


Stats("virat_new_bat.csv",name="Virat Kohli")
batsmanAvgRunsGround("virat_new_bat.csv",name="Virat Kohli")
batsmanAvgRunsPosition("virat_new_bat.csv",name="Virat Kohli")
batsmanAvgRunsOpposition("virat_new_bat.csv",name="Virat Kohli")
batsmanPerfBoxHist("virat_new_bat.csv",name="Virat Kohli")
batsmanRunsRanges("virat_new_bat.csv",name="Virat Kohli")
batsmanRunsFreqPerf("virat_new_bat.csv",name="Virat Kohli")
batsmanScoringRateODTT("virat_new_bat.csv",name="Virat Kohli")
battingPerf3d("virat_new_bat.csv",name="Virat Kohli")
batsmanRunsLikelihood("virat_new_bat.csv",name="Virat Kohli")

BF <- round(seq( 10, 400,length=15),0)
Mins <- round(seq(30,600,length=15),0)
newDF <- data.frame(BF,Mins)
batsmanRunsPredict("virat_new_bat.csv","Virat kohli",newdataframe=newDF)

batsmanPerfForecast("virat_new_bat.csv",name="Virat Kohli")
batsmanDismissals("virat_new_bat.csv",name="Virat Kohli")
batsmanContributionWonLost("virat_new_bat.csv",name="Virat Kohli")



getmode("virat_new_bat.csv")
getmode("ajinkya_rahane_bat.csv")
getmode("amit_mishra_bat.csv")
getmode("b_kumar_bat.csv")
getmode("hardik_pandya_bat.csv")
getmode("jasprit_bumrah_bat.csv")
getmode("kedar_jadhav_bat.csv")
getmode("kl_rahul_bat.csv")
getmode("manish_pandey_bat.csv")
getmode("ms_dhoni_bat.csv")
getmode("r_ashwin_bat.csv")
getmode("r_jadeja_bat.csv")
getmode("shikhar_dhavan_bat.csv")
getmode("umesh_yadav_bat.csv")
getmode("yuvraj_singh_bat.csv")




VK=Batting_ScoreCard("virat_new_bat.csv",name="Virat Kohli")
AR=Batting_ScoreCard("ajinkya_rahane_bat.csv",name="Ajinkya Rahane")
AM=Batting_ScoreCard("amit_mishra_bat.csv",name="Amit Mishra")
BK=Batting_ScoreCard("b_kumar_bat.csv",name="Bhuvi Kumar")
HP=Batting_ScoreCard("hardik_pandya_bat.csv",name="Hardik Pandya")
JB=Batting_ScoreCard("jasprit_bumrah_bat.csv",name="Jasprit Bumrah")
KJ=Batting_ScoreCard("kedar_jadhav_bat.csv",name="Kedar Jadhav")
KR=Batting_ScoreCard("kl_rahul_bat.csv",name="KL Rahul")
MP=Batting_ScoreCard("manish_pandey_bat.csv",name="Manish Pandey")
MSD=Batting_ScoreCard("ms_dhoni_bat.csv",name="MS Dhoni")
RA=Batting_ScoreCard("r_ashwin_bat.csv",name="R Ashwin")
RJ=Batting_ScoreCard("r_jadeja_bat.csv",name="R Jadeja")
SD=Batting_ScoreCard("shikhar_dhavan_bat.csv",name="Shikhar Dhavan")
UY=Batting_ScoreCard("umesh_yadav_bat.csv",name="Umesh Yadav")
YS=Batting_ScoreCard("yuvraj_singh_bat.csv",name="Yuvraj Singh")
finaldf<-data.frame()
finaldf<- rbind(VK,AR,AM,BK,HP,JB,KJ,KR,MP,MSD,RA,RJ,SD,UY,YS)
write.csv(finaldf,file="BattingIndex.csv", row.names=FALSE)




VK1=Batting_Metrics("virat_new_bat.csv",name="Virat Kohli","India","Batsman")
AR1=Batting_Metrics("ajinkya_rahane_bat.csv",name="Ajinkya Rahane","India","Batsman")
AM1=Batting_Metrics("amit_mishra_bat.csv",name="Amit Mishra","India","Bowler")
BK1=Batting_Metrics("b_kumar_bat.csv",name="Bhuvi Kumar","India","Bowler")
HP1=Batting_Metrics("hardik_pandya_bat.csv",name="Hardik Pandya","India","All-rounder")
JB1=Batting_Metrics("jasprit_bumrah_bat.csv",name="Jasprit Bumrah","India","Bowler")
KJ1=Batting_Metrics("kedar_jadhav_bat.csv",name="Kedar Jadhav","India","Batsman")
KR1=Batting_Metrics("kl_rahul_bat.csv",name="KL Rahul","India","Batsman")
MP1=Batting_Metrics("manish_pandey_bat.csv",name="Manish Pandey","India","Batsman")
MSD1=Batting_Metrics("ms_dhoni_bat.csv",name="MS Dhoni","India","Wicket-keeper")
RA1=Batting_Metrics("r_ashwin_bat.csv",name="R Ashwin","India","Bowler")
RJ1=Batting_Metrics("r_jadeja_bat.csv",name="R Jadeja","India","All-rounder")
SD1=Batting_Metrics("shikhar_dhavan_bat.csv",name="Shikhar Dhavan","India","Batsman")
UY1=Batting_Metrics("umesh_yadav_bat.csv",name="Umesh Yadav","India","Bowler")
YS1=Batting_Metrics("yuvraj_singh_bat.csv",name="Yuvraj Singh","India","All-rounder")
finaldfNew<-data.frame()
finaldfNew<- rbind(VK1,AR1,AM1,BK1,HP1,JB1,KJ1,KR1,MP1,MSD1,RA1,RJ1,SD1,UY1,YS1)
write.csv(finaldfNew,file="BattingMetrics.csv", row.names=FALSE)



#Random Forest For importance
BattingData <- read.csv(file="BattingMetrics.csv",stringsAsFactor=FALSE)
head(BattingData)
str(BattingData)
library(randomForest)
BattingData.rf <- randomForest(Percentage.Wins ~ ., data=BattingData, ntree=1000, keep.forest=FALSE, importance=TRUE)
importance(BattingData.rf)
importance(BattingData.rf, type=1)





