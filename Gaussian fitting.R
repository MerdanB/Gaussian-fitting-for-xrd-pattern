#Gaussian fitting
library(tidyverse)

getwd()
setwd("../Downloads/")

read_delim("Guess2Fi.txt", delim = " ", col_names = F) %>% 
  mutate_all(as.double)->dt

y <- dt$X2

myPeaksIndex <- 0
myPeaksIndexCounter=1
peakNotTaken=TRUE

for (i in seq(1,length(y))) {
  if (y[i]>800 && i+20<=length(y)) {#800 is the threshold value
    if (y[i]>y[i+10] && y[i]>y[i+20] && peakNotTaken) {
      myPeaksIndex[myPeaksIndexCounter]=i+5
      myPeaksIndexCounter=myPeaksIndexCounter+1
      peakNotTaken=FALSE
    }
    if (!peakNotTaken && y[i]<y[i+10]&& y[i]<y[i+20]) {
      peakNotTaken=TRUE
    }
  }
}

#manually remove unwanted peaks
myPeaksIndex <- myPeaksIndex[-c(1,3:6,8,11,12,21)]

dt %>% 
  # filter(X1>48.5 & X1<49.3) %>% 
  ggplot(aes(X1,X2))+
  geom_point(size=0.001)+
  geom_vline(xintercept = dt$X1[myPeaksIndex])

MyVars <- ""

for (i in seq(1,length(myPeaksIndex))) {
  paste("C",i,"*","exp(-(X1-mean",i,")**2/(2*sigma",i,"**2))",sep = "")->MyVars[i]
}

MyList <- ""

for (i in seq(1,length(myPeaksIndex))) {
  paste("C",i,"=",dt$X2[myPeaksIndex[i]],",mean",i,"=",dt$X1[myPeaksIndex[i]],
        ",sigma",i,"=0.2",sep = "")->MyList[i]
}

unlist(strsplit(unlist(strsplit(MyList, split = ",")), split = "="))->clean_MyList
paramsMyList <- clean_MyList[seq(2,132,2)]
names(paramsMyList) <- clean_MyList[seq(1,132,2)]

fit <- nls(as.formula( paste("X2 ~ ", paste(MyVars, collapse = "+") ) ), data=dt,
           start=as.list(paramsMyList), algorithm="port")

summary(fit)

# Predict the fitted model to a denser grid of x values
dffit <- data.frame(X1=seq(0, 150, 0.01))
dffit$X2 <- predict(fit, newdata=dffit)

# Plot the data with the model superimposed
print(ggplot(dt, aes(x=X1, y=X2)) + geom_point() +
        geom_smooth(data=dffit, stat="identity", color="red", size=0.5)) 





