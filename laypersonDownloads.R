NASEM_cleaned <- read.csv(file.choose(), header=TRUE) #read in download_data.csv file 1.6 GB 
library(dplyr)
install.packages("scales")
install.packages("stargazer")
install.packages("pander")
devtools::install_github( repo="OuhscBbmc/Wats" )
library(scales)
library(stargazer)
library(pander)
library(Wats)
library(lubridate)

### !!! CODE: 
### https://ds4ps.org/pe4ps-textbook/docs/p-020-time-series.html


## ***Also look at other sectors***
## Look at PeepID before and after cutoff
## predicting demand for reports with user mix
## elasticity of demand for different user groups -- 
## McCrary test > negative vs positive test
## Natural Policy Experiments: OA Floodgates
# high-resolution data pre and post 
## run without NGSS / Nursing reports -- more conservative estimate of OA effect


# USdownloads <- filter(NASEM, country == "US")
# gmail <- filter(USdownloads, email == "@gmail.com")
# yahoo <- filter(USdownloads, email == "@yahoo.com")
# hotmail <- filter(USdownloads, email == "@hotmail.com")

# gmail_yahoo <- rbind(gmail, yahoo)
# personalDownloads <- rbind(gmail_yahoo, hotmail)  ## used before

personalDownloads <- NASEM_cleaned %>% filter(is.na(sector)) ### USED ON CORRECT DATA on MARCH 9

2292188/6648781
## 34.5% are personal email downloads

rm(gmail)
rm(yahoo)
rm(hotmail)
rm(gmail_yahoo)

class(personalDownloads$date2)
personalDownloads$date <- as.Date(personalDownloads$date2, format = "%m/%d/%Y")


# preOA <- personalDownloads[personalDownloads$date >= "2011-01-01" & personalDownloads$date <= "2011-06-01",]
# postOA <- personalDownloads[personalDownloads$date >= "2011-06-03" & personalDownloads$date <= "2011-12-31",]

fullSlice <- personalDownloads[personalDownloads$date >= "2003-01-01" & personalDownloads$date <= "2020-01-01",]
itsPrep <- table(fullSlice$date, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="itsTest4.csv")

itsTest <- read.csv(file.choose(), header=TRUE)

plot( itsTest$T, itsTest$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 2000), xlim=c(0,5107),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Report Daily Downloads" )

# Line marking the interruption
abline( v=2079, col="firebrick", lty=2 )
text( 1079, 900, "Start of Open Access Policy", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( Y ~ T + D + P, data=itsTest )
lines( itsTest$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm ( Y ~ T + D + P, data=itsTest)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Open Access Effect"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "OA Policy", 
                                "Time Since OA Policy In Effect"),
           #omit.stat = "all", 
           digits = 2,
           out = "laypersonITSmodel.html")

# We create a small dataset with the new values
data1 <- as.data.frame( cbind( T = 2079, D = 1, P = 1 )) 

# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5214), 
      ylim = c(0, 2000),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of open access effect at time = 32)
points( 2079, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 2079, y1, labels = "t = June 2, 2011", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=2079, col="red", lty=2 )

######

data2 <- as.data.frame( cbind( T = 2444, D = 1, P = 365 )) # New data

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5214), 
      ylim = c(0, 2000),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of OA effect at time of policy change
points(2079, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of OA effect at time = 1 yr later)
points(2444, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(2079, y1, labels = "t = predicted", pos = 4, cex = 1)

#Label for the counterfactual 
text(2444, y2, labels = "t = 1 year (365 days) after OA policy", pos = 4, cex = 1)

# Line marking the interruption
abline( v=2079, col="red", lty=2 )

### COUNTERFACTUAL

data3 <- as.data.frame(cbind( T= 2444, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5214), 
      ylim = c(0, 2000),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a  point showing the level of OA EFFECT at time 1 year after OA policy
points(2444, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(2444, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(2444, y2, labels = "Y at t = 1 yr later", pos = 4, cex = 1)

#Label for the counterfactual 
text(2444, y3, labels = "Counterfactual at t = 1 year later", pos = 4, cex = 1)

# Line marking the interruption
abline( v=2079, col="red", lty=2 )

## MORE COUNTERFACTUAL POINTS

data4 <- as.data.frame(cbind( T = 3904, D = 1, P = 1928)) 
data5 <- as.data.frame(cbind( T = 3904, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5214), 
      ylim = c(0, 3000),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Daily Report Downloads")

# OA downloads at time = +365 days
points(2444, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = +365 days
points(2444, y3, col = "darkorange2", pch = 19, cex = 2)

# OA downloads at time = +5 yrs
points(3904, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = +5 yrs
points(3904, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(3904, y4, labels = "Downloads at t = 5 yrs after OA", pos = 4, cex = 1)
text(3904, y5, labels = "Counterfactual at t = 5 yrs after OA", pos = 4, cex = 1)
text(2444, y2, labels = "Downloads at at = 1 yr after OA", pos = 4, cex = 1)
text(2444, y3, labels = "Counterfactual at t = 1 yr after OA", pos = 4, cex = 1)

# Line marking the interruption
abline( v=2079, col="red", lty=2 )
  
## ADD REGRESSION LINES FOR COUNTERFACTUAL
pred1 <- predict(regTS, itsTest) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 5214), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5214), 
      ylim = c(0, 1500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Daily Report Downloads")

lines( rep(1:2078), pred1[1:2078], col="dodgerblue4", lwd = 3 )
lines( rep(2079:5214), pred1[2079:5214], col="dodgerblue4", lwd = 3 )
lines( rep(2079:5214), pred2[2079:5214], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 400, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(4000, 95, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption (use zoom)
abline( v=2079, col="darkorange2", lty=2 )
text( 0, 1500, "Start of Open Access Policy (June 2, 2011)", col="darkorange2", cex=.8, pos=4 )

install.packages("lmtest")
library("lmtest")
dwtest(Y ~ T + D + P, data=itsTest)

##############################################################################
##############################################################################
## STATE GOVERNMENT
##############################################################################
##############################################################################


stateGovDownloads <- NASEM_cleaned %>% filter(sector == "State Government") ### USED ON CORRECT DATA on MARCH 9

223355/6648781
## 3.3% are state government downloads

class(stateGovDownloads$date2)
stateGovDownloads$date <- as.Date(stateGovDownloads$date2, format = "%m/%d/%Y")


# preOA <- personalDownloads[personalDownloads$date >= "2011-01-01" & personalDownloads$date <= "2011-06-01",]
# postOA <- personalDownloads[personalDownloads$date >= "2011-06-03" & personalDownloads$date <= "2011-12-31",]

fullSlice <- stateGovDownloads[stateGovDownloads$date >= "2003-01-01" & stateGovDownloads$date <= "2020-01-01",]
itsPrep <- table(fullSlice$date, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
stateGovTopReports <- table(fullSlice$record_id)

write.csv(itsPrep, file="itsTest4_stateGov.csv")

itsTest <- read.csv(file.choose(), header=TRUE)

plot( itsTest$T, itsTest$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 600), xlim=c(0,4853),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Report Daily Downloads" )

# Line marking the interruption
abline( v=1734, col="firebrick", lty=2 )
text( 1750, 500, "Start of Open Access Policy", col="firebrick", cex=1.0, pos=4 )

# Add the regression line
ts <- lm( Y ~ T + D + P, data=itsTest )
lines( itsTest$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm ( Y ~ T + D + P, data=itsTest)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Open Access Effect"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "OA Policy", 
                                "Time Since OA Policy In Effect"),
           #omit.stat = "all", 
           digits = 2,
           out = "stateGovITSmodel.html")

# We create a small dataset with the new values
data1 <- as.data.frame( cbind( T = 1734, D = 1, P = 1 )) 

# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4853), 
      ylim = c(0, 500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of open access effect at time = 32)
points( 1734, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 1734, y1, labels = "t = June 2, 2011", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=1734, col="red", lty=2 )

######

data2 <- as.data.frame( cbind( T = 2099, D = 1, P = 365 )) # New data 365 days after 1734

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4853), 
      ylim = c(0, 500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of OA effect at time of policy change
points(1734, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of OA effect at time = 1 yr later)
points(2099, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(1734, y1, labels = "t = predicted", pos = 4, cex = 1)

#Label for the counterfactual 
text(2099, y2, labels = "t = 1 year (365 days) after OA policy", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1734, col="red", lty=2 )

### COUNTERFACTUAL

data3 <- as.data.frame(cbind( T= 2099, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4813), 
      ylim = c(0, 500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a  point showing the level of OA EFFECT at time 1 year after OA policy
points(2099, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(2099, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(2099, y2, labels = "Y at t = 1 yr later", pos = 4, cex = 1)

#Label for the counterfactual 
text(2099, y3, labels = "Counterfactual at t = 1 year later", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1734, col="red", lty=2 )

## MORE COUNTERFACTUAL POINTS

data4 <- as.data.frame(cbind( T = 3559, D = 1, P = 1826)) # 5 years after treatment
data5 <- as.data.frame(cbind( T = 3559, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4853), 
      ylim = c(0, 500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Daily Report Downloads")

# OA downloads at time = +365 days
points(2099, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = +365 days
points(2099, y3, col = "darkorange2", pch = 19, cex = 2)

# OA downloads at time = +5 yrs
points(3559, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = +5 yrs
points(3559, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(3559, y4, labels = "Downloads at t = 5 yrs after OA", pos = 4, cex = 1)
text(3559, y5, labels = "Counterfactual at t = 5 yrs after OA", pos = 4, cex = 1)
text(2099, y2, labels = "Downloads at at = 1 yr after OA", pos = 4, cex = 1)
text(2099, y3, labels = "Counterfactual at t = 1 yr after OA", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1734, col="red", lty=2 )

## ADD REGRESSION LINES FOR COUNTERFACTUAL
pred1 <- predict(regTS, itsTest) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 4853), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4853), 
      ylim = c(0, 500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Daily Report Downloads")

lines( rep(1:1733), pred1[1:1733], col="dodgerblue4", lwd = 3 )
lines( rep(1734:4853), pred1[1734:4853], col="dodgerblue4", lwd = 3 )
lines( rep(1734:4853), pred2[1734:4853], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 150, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(2000, 200, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption (use zoom)
abline( v=1734, col="darkorange2", lty=2 )
text( 1750, 500, "Start of Open Access Policy (June 2, 2011)", col="darkorange2", cex=1, pos=4 )

abline(v=3194, col="darkorange2", lty=2 )
text( 2800, 400, "TSRB, 2015", col="darkorange2", cex=1, pos=4 )

install.packages("lmtest")
library("lmtest")
dwtest(Y ~ T + D + P, data=itsTest)



########################################################################
### UNIVERSITY #############################################################
########################################################################

univDownloads <- NASEM_cleaned %>% filter(sector == "University") ### USED ON CORRECT DATA on MARCH 9

1903132/6648781
## 29% are university downloads

class(univDownloads$date2)
univDownloads$date <- as.Date(univDownloads$date2, format = "%m/%d/%Y")


# preOA <- personalDownloads[personalDownloads$date >= "2011-01-01" & personalDownloads$date <= "2011-06-01",]
# postOA <- personalDownloads[personalDownloads$date >= "2011-06-03" & personalDownloads$date <= "2011-12-31",]

fullSlice <- univDownloads[univDownloads$date >= "2003-01-01" & univDownloads$date <= "2020-01-01",]
itsPrep <- table(fullSlice$date, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="itsTest4_univ.csv")

itsTest <- read.csv(file.choose(), header=TRUE)

plot( itsTest$T, itsTest$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 2500), xlim=c(0,5120),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Report Daily Downloads" )

# Line marking the interruption
abline( v=1985, col="firebrick", lty=2 )
text( 1750, 2100, "Start of Open Access Policy", col="firebrick", cex=1.0, pos=4 )

# Add the regression line
ts <- lm( Y ~ T + D + P, data=itsTest )
lines( itsTest$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm ( Y ~ T + D + P, data=itsTest)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Open Access Effect"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "OA Policy", 
                                "Time Since OA Policy In Effect"),
           #omit.stat = "all", 
           digits = 2,
           out = "univITSmodel.html")

# We create a small dataset with the new values
data1 <- as.data.frame( cbind( T = 1985, D = 1, P = 1 )) 

# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5120), 
      ylim = c(0, 2500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of open access effect at time = 32)
points( 1985, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 1985, y1, labels = "t = June 2, 2011", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=1985, col="red", lty=2 )

######

data2 <- as.data.frame( cbind( T = 2350, D = 1, P = 365 )) # New data 365 days after 1734

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5120), 
      ylim = c(0, 2500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of OA effect at time of policy change
points(1985, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of OA effect at time = 1 yr later)
points(2350, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(1985, y1, labels = "t = predicted", pos = 4, cex = 1)

#Label for the counterfactual 
text(2350, y2, labels = "t = 1 year (365 days) after OA policy", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1985, col="red", lty=2 )

### COUNTERFACTUAL

data3 <- as.data.frame(cbind( T= 2350, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5120), 
      ylim = c(0, 2500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a  point showing the level of OA EFFECT at time 1 year after OA policy
points(2350, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(2350, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(2350, y2, labels = "Y at t = 1 yr later", pos = 4, cex = 1)

#Label for the counterfactual 
text(2350, y3, labels = "Counterfactual at t = 1 year later", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1985, col="red", lty=2 )

## MORE COUNTERFACTUAL POINTS

data4 <- as.data.frame(cbind( T = 3810, D = 1, P = 1826)) # 5 years after treatment
data5 <- as.data.frame(cbind( T = 3810, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5120), 
      ylim = c(0, 2500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Daily Report Downloads")

# OA downloads at time = +365 days
points(2350, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = +365 days
points(2350, y3, col = "darkorange2", pch = 19, cex = 2)

# OA downloads at time = +5 yrs
points(3810, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = +5 yrs
points(3810, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(3810, y4, labels = "Downloads at t = 5 yrs after OA", pos = 4, cex = 1)
text(3810, y5, labels = "Counterfactual at t = 5 yrs after OA", pos = 4, cex = 1)
text(2350, y2, labels = "Downloads at at = 1 yr after OA", pos = 4, cex = 1)
text(2350, y3, labels = "Counterfactual at t = 1 yr after OA", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1985, col="red", lty=2 )

## ADD REGRESSION LINES FOR COUNTERFACTUAL
pred1 <- predict(regTS, itsTest) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 5120), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5120), 
      ylim = c(0, 2500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Daily Report Downloads")

lines( rep(1:1984), pred1[1:1984], col="dodgerblue4", lwd = 3 )
lines( rep(1985:5120), pred1[1985:5120], col="dodgerblue4", lwd = 3 )
lines( rep(1985:5120), pred2[1985:5120], col="darkorange2", lwd = 3, lty = 5 ) 

text(1000, 1000, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(3000, 2000, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption (use zoom)
abline( v=1985, col="darkorange2", lty=2 )
text( 2000, 2500, "Start of Open Access Policy (June 2, 2011)", col="darkorange2", cex=1, pos=4 )

install.packages("lmtest")
library("lmtest")
dwtest(Y ~ T + D + P, data=itsTest)


########################################################################
### HEALTH CARE  #############################################################
########################################################################

healthDownloads <- NASEM_cleaned %>% filter(sector == "Health Care") ### USED ON CORRECT DATA on MARCH 9

168342/6648781
## 3.5% are health care downloads

class(healthDownloads$date2)
healthDownloads$date <- as.Date(healthDownloads$date2, format = "%m/%d/%Y")


# preOA <- personalDownloads[personalDownloads$date >= "2011-01-01" & personalDownloads$date <= "2011-06-01",]
# postOA <- personalDownloads[personalDownloads$date >= "2011-06-03" & personalDownloads$date <= "2011-12-31",]

fullSlice <- healthDownloads[healthDownloads$date >= "2003-01-01" & healthDownloads$date <= "2020-01-01",]
itsPrep <- table(fullSlice$date, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="itsTest4_health.csv")

itsTest <- read.csv(file.choose(), header=TRUE)

plot( itsTest$T, itsTest$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 500), xlim=c(0,4832),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Report Daily Downloads" )

# Line marking the interruption
abline( v=1702, col="firebrick", lty=2 )
text( 1702, 500, "Start of Open Access Policy", col="firebrick", cex=1.0, pos=4 )

# Add the regression line
ts <- lm( Y ~ T + D + P, data=itsTest )
lines( itsTest$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm ( Y ~ T + D + P, data=itsTest)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Open Access Effect"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "OA Policy", 
                                "Time Since OA Policy In Effect"),
           #omit.stat = "all", 
           digits = 2,
           out = "healthITSmodel.html")

# We create a small dataset with the new values
data1 <- as.data.frame( cbind( T = 1702, D = 1, P = 1 )) 

# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4832), 
      ylim = c(0, 500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of open access effect at time = 32)
points( 1702, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 1702, y1, labels = "t = June 2, 2011", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=1702, col="red", lty=2 )

######

data2 <- as.data.frame( cbind( T = 2067, D = 1, P = 365 )) # New data 365 days after 1702

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4832), 
      ylim = c(0, 500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of OA effect at time of policy change
points(1702, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of OA effect at time = 1 yr later)
points(2067, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(1702, y1, labels = "t = predicted", pos = 4, cex = 1)

#Label for the counterfactual 
text(2067, y2, labels = "t = 1 year (365 days) after OA policy", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1702, col="red", lty=2 )

### COUNTERFACTUAL

data3 <- as.data.frame(cbind( T= 2067, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4832), 
      ylim = c(0, 500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a  point showing the level of OA EFFECT at time 1 year after OA policy
points(2067, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(2067, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(2067, y2, labels = "Y at t = 1 yr later", pos = 4, cex = 1)

#Label for the counterfactual 
text(2067, y3, labels = "Counterfactual at t = 1 year later", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1702, col="red", lty=2 )

## MORE COUNTERFACTUAL POINTS

data4 <- as.data.frame(cbind( T = 3527, D = 1, P = 1826)) # 5 years after treatment
data5 <- as.data.frame(cbind( T = 3527, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4832), 
      ylim = c(0, 500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Daily Report Downloads")

# OA downloads at time = +365 days
points(2067, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = +365 days
points(2067, y3, col = "darkorange2", pch = 19, cex = 2)

# OA downloads at time = +5 yrs
points(3527, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = +5 yrs
points(3527, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(3527, y4, labels = "Downloads at t = 5 yrs after OA", pos = 4, cex = 1)
text(3527, y5, labels = "Counterfactual at t = 5 yrs after OA", pos = 4, cex = 1)
text(2067, y2, labels = "Downloads at at = 1 yr after OA", pos = 4, cex = 1)
text(2067, y3, labels = "Counterfactual at t = 1 yr after OA", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1702, col="red", lty=2 )

## ADD REGRESSION LINES FOR COUNTERFACTUAL
pred1 <- predict(regTS, itsTest) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 4832), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4832), 
      ylim = c(0, 500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "NASEM Daily Report Downloads")

lines( rep(1:1701), pred1[1:1701], col="dodgerblue4", lwd = 3 )
lines( rep(1702:4832), pred1[1702:4832], col="dodgerblue4", lwd = 3 )
lines( rep(1702:4832), pred2[1702:4832], col="darkorange2", lwd = 3, lty = 5 ) 

text(1000, 100, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(3000, 300, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption (use zoom)
abline( v=1702, col="darkorange2", lty=2 )
text( 1702, 500, "Start of Open Access Policy (June 2, 2011)", col="darkorange2", cex=1, pos=4 )

install.packages("lmtest")
library("lmtest")
dwtest(Y ~ T + D + P, data=itsTest)




############################################################################
## BELOW USES NON-CLEANED DATA - DO NOT USE FOR ANALYSIS!!!
############################################################################
##############################################################################
##############################################################################
########################
###### ACADEMIC UPTAKE
########################

# Academic uptake of OA NASEM reports
academics <- filter(USdownloads, grepl('edu', email))
2208784/8304027 # 26.6% academic downloads

class(academics$date)
academics$date <- as.Date(academics$date)

preOA <- academics[academics$date >= "2011-01-01" & academics$date <= "2011-06-01",]
postOA <- academics[academics$date >= "2011-06-03" & academics$date <= "2011-12-31",]

fullSlice <- academics[academics$date >= "2006-01-01" & academics$date <= "2020-01-01",]
itsPrep <- table(fullSlice$date)
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="itsTest4acad.csv")

itsTest <- read.csv(file.choose(), header=TRUE) ## REMEMBER TO REFORMAT HEADER TO Y,T,D,P

plot( itsTest$T, itsTest$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 2000), xlim=c(0,5107),
      xlab = "Time (days)", 
      ylab = "NASEM Report Daily Downloads" )

# Line marking the interruption
abline( v=1976, col="firebrick", lty=2 )
text( 1976, 2000, "Start of Open Access Policy", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( Y ~ T + D + P, data=itsTest )
lines( itsTest$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm ( Y ~ T + D + P, data=itsTest)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Open Access Effect"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "OA Policy", 
                                "Time Since OA Policy In Effect"),
           omit.stat = "all", 
           digits = 2, 
           out = "academicITSmodel.html")

summary(regTS)

# We create a small dataset with the new values
data1 <- as.data.frame( cbind( T = 1976, D = 1, P = 1 )) 

# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "OA Effect Index")

# We add a point showing the level of open access effect at time = 32)
points( 1976, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 1976, y1, labels = "t = June 2, 2011", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=1976, col="red", lty=2 )

######

data2 <- as.data.frame( cbind( T = 2341, D = 1, P = 365 )) # New data

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "OA Effect Index")

# We add a point showing the level of OA effect at time of policy change
points(1976, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of OA effect at time = 1 yr later)
points(2341, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(1976, y1, labels = "t = predicted", pos = 4, cex = 1)

#Label for the counterfactual 
text(2341, y2, labels = "t = 1 year (365 days) after OA policy", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1976, col="red", lty=2 )

### COUNTERFACTUAL

data3 <- as.data.frame(cbind( T= 2341, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "OA Effect Index")

# We add a  point showing the level of OA EFFECT at time 1 year after OA policy
points(2341, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(2341, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(2341, y2, labels = "Y at t = 1 yr later", pos = 4, cex = 1)

#Label for the counterfactual 
text(2341, y3, labels = "Counterfactual at t = 1 year later", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1976, col="red", lty=2 )

## MORE COUNTERFACTUAL POINTS

data4 <- as.data.frame(cbind( T = 3801, D = 1, P = 1825)) 
data5 <- as.data.frame(cbind( T = 3801, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "NASEM Daily Report Downloads")

# OA downloads at time = +365 days
points(2341, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = +365 days
points(2341, y3, col = "darkorange2", pch = 19, cex = 2)

# OA downloads at time = +5 yrs
points(3801, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = +5 yrs
points(3801, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(3801, y4, labels = "Downloads at t = 5 yrs after OA", pos = 4, cex = 1)
text(3801, y5, labels = "Counterfactual at t = 5 yrs after OA", pos = 4, cex = 1)
text(2341, y2, labels = "Downloads at at = 1 yr after OA", pos = 4, cex = 1)
text(2341, y3, labels = "Counterfactual at t = 1 yr after OA", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1976, col="red", lty=2 )

## ADD REGRESSION LINES FOR COUNTERFACTUAL
pred1 <- predict(regTS, itsTest) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 5107), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 1000),
      xlab = "Time (days)", 
      ylab = "NASEM Daily Report Downloads")

lines( rep(1:1975), pred1[1:1975], col="dodgerblue4", lwd = 3 )
lines( rep(1976:5107), pred1[1976:5107], col="dodgerblue4", lwd = 3 )
lines( rep(1976:5107), pred2[1976:5107], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 400, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(3000, 35, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline( v=1976, col="darkorange2", lty=2 )

install.packages("lmtest")
library("lmtest")
dwtest(Y ~ T + D + P, data=itsTest)

#######
####### GOVT USE OF NASEM ITS
#######

# Government employee uptake of OA NASEM reports
govt <- filter(USdownloads, grepl('gov', email))
676365/8304027 # 8.1% government employee downloads

class(govt$date)
govt$date <- as.Date(govt$date)

preOA <- govt[govt$date >= "2011-01-01" & govt$date <= "2011-06-01",]
postOA <- govt[govt$date >= "2011-06-03" & govt$date <= "2011-12-31",]

fullSlice <- govt[govt$date >= "2006-01-01" & govt$date <= "2020-01-01",]
itsPrep <- table(fullSlice$date)
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="itsTest4govt.csv")

itsTest <- read.csv(file.choose(), header=TRUE) ## REMEMBER TO REFORMAT HEADER TO Y,T,D,P

plot( itsTest$T, itsTest$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 2000), xlim=c(0,5107),
      xlab = "Time (days)", 
      ylab = "NASEM Report Daily Downloads" )

# Line marking the interruption
abline( v=1942, col="firebrick", lty=2 ) # NOT 1976 due to 34 missing observations!!
text( 1942, 2000, "Start of Open Access Policy", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( Y ~ T + D + P, data=itsTest )
lines( itsTest$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm ( Y ~ T + D + P, data=itsTest)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Open Access Effect"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "OA Policy", 
                                "Time Since OA Policy In Effect"),
           omit.stat = "all", 
           digits = 2, 
           out = "govtITSmodel.html")

summary(regTS)

# We create a small dataset with the new values
data1 <- as.data.frame( cbind( T = 1942, D = 1, P = 1 )) ## NOTE T not 1976

# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "OA Effect Index")

# We add a point showing the level of open access effect at time = 32)
points( 1942, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 1942, y1, labels = "t = June 2, 2011", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=1942, col="red", lty=2 )

######

data2 <- as.data.frame( cbind( T = 2341, D = 1, P = 365 )) # New data

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "OA Effect Index")

# We add a point showing the level of OA effect at time of policy change
points(1942, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of OA effect at time = 1 yr later)
points(2341, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(1942, y1, labels = "t = predicted", pos = 4, cex = 1)

#Label for the counterfactual 
text(2341, y2, labels = "t = 1 year (365 days) after OA policy", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1942, col="red", lty=2 )

### COUNTERFACTUAL

data3 <- as.data.frame(cbind( T= 2341, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "OA Effect Index")

# We add a  point showing the level of OA EFFECT at time 1 year after OA policy
points(2341, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(2341, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(2341, y2, labels = "Y at t = 1 yr later", pos = 4, cex = 1)

#Label for the counterfactual 
text(2341, y3, labels = "Counterfactual at t = 1 year later", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1942, col="red", lty=2 )

## MORE COUNTERFACTUAL POINTS

data4 <- as.data.frame(cbind( T = 3801, D = 1, P = 1825)) 
data5 <- as.data.frame(cbind( T = 3801, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "NASEM Daily Report Downloads")

# OA downloads at time = +365 days
points(2341, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = +365 days
points(2341, y3, col = "darkorange2", pch = 19, cex = 2)

# OA downloads at time = +5 yrs
points(3801, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = +5 yrs
points(3801, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(3801, y4, labels = "Downloads at t = 5 yrs after OA", pos = 4, cex = 1)
text(3801, y5, labels = "Counterfactual at t = 5 yrs after OA", pos = 4, cex = 1)
text(2341, y2, labels = "Downloads at at = 1 yr after OA", pos = 4, cex = 1)
text(2341, y3, labels = "Counterfactual at t = 1 yr after OA", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1942, col="red", lty=2 )

## ADD REGRESSION LINES FOR COUNTERFACTUAL
pred1 <- predict(regTS, itsTest) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 5107), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 1000),
      xlab = "Time (days)", 
      ylab = "NASEM Daily Report Downloads")

lines( rep(1:1941), pred1[1:1941], col="dodgerblue4", lwd = 3 ) # NOTE 1941 instead of 1975
lines( rep(1942:5107), pred1[1942:5107], col="dodgerblue4", lwd = 3 ) # NOTE 1942 instead of 1976
lines( rep(1942:5107), pred2[1942:5107], col="darkorange2", lwd = 3, lty = 5 ) # NOTE 1942 instead of 1976

text(0, 400, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(3000, 35, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline( v=1942, col="darkorange2", lty=2 )

install.packages("lmtest")
library("lmtest")
dwtest(Y ~ T + D + P, data=itsTest)


####################
###### MILITARY UPTAKE OF NASEM REPORTS
####################

# Military uptake of OA NASEM reports
military <- filter(USdownloads, grepl('mil', email))
99873/8304027 # 1.2% military downloads

class(military$date)
military$date <- as.Date(military$date)

preOA <- military[military$date >= "2011-01-01" & military$date <= "2011-06-01",]
postOA <- military[military$date >= "2011-06-03" & military$date <= "2011-12-31",]

fullSlice <- military[military$date >= "2006-01-01" & military$date <= "2020-01-01",]
itsPrep <- table(fullSlice$date)
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="itsTest4mil.csv")

itsTest <- read.csv(file.choose(), header=TRUE) ## REMEMBER TO REFORMAT HEADER TO Y,T,D,P
#### Note many missing obs because table command ignores "0 download" dates

plot( itsTest$T, itsTest$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 2000), xlim=c(0,5107),
      xlab = "Time (days)", 
      ylab = "NASEM Report Daily Downloads" )

# Line marking the interruption
abline( v=1772, col="firebrick", lty=2 )
text( 1772, 2000, "Start of Open Access Policy", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( Y ~ T + D + P, data=itsTest )
lines( itsTest$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm ( Y ~ T + D + P, data=itsTest)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Open Access Effect"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "OA Policy", 
                                "Time Since OA Policy In Effect"),
           omit.stat = "all", 
           digits = 2, 
           out = "milITSmodel.html")

summary(regTS)

# We create a small dataset with the new values
data1 <- as.data.frame( cbind( T = 1772, D = 1, P = 1 )) ## NOTE 1772 instead of 1976!!
                        
                        
# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "OA Effect Index")

# We add a point showing the level of open access effect at time = 32)
points( 1772, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 1772, y1, labels = "t = June 2, 2011", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=1772, col="red", lty=2 )

######

data2 <- as.data.frame( cbind( T = 2341, D = 1, P = 365 )) # New data

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "OA Effect Index")

# We add a point showing the level of OA effect at time of policy change
points(1772, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of OA effect at time = 1 yr later)
points(2341, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(1772, y1, labels = "t = predicted", pos = 4, cex = 1)

#Label for the counterfactual 
text(2341, y2, labels = "t = 1 year (365 days) after OA policy", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1772, col="red", lty=2 )

### COUNTERFACTUAL

data3 <- as.data.frame(cbind( T= 2341, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "OA Effect Index")

# We add a  point showing the level of OA EFFECT at time 1 year after OA policy
points(2341, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(2341, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(2341, y2, labels = "Y at t = 1 yr later", pos = 4, cex = 1)

#Label for the counterfactual 
text(2341, y3, labels = "Counterfactual at t = 1 year later", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1772, col="red", lty=2 )

## MORE COUNTERFACTUAL POINTS

data4 <- as.data.frame(cbind( T = 3801, D = 1, P = 1825)) 
data5 <- as.data.frame(cbind( T = 3801, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5107), 
      ylim = c(0, 2000),
      xlab = "Time (days)", 
      ylab = "NASEM Daily Report Downloads")

# OA downloads at time = +365 days
points(2341, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = +365 days
points(2341, y3, col = "darkorange2", pch = 19, cex = 2)

# OA downloads at time = +5 yrs
points(3801, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = +5 yrs
points(3801, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(3801, y4, labels = "Downloads at t = 5 yrs after OA", pos = 4, cex = 1)
text(3801, y5, labels = "Counterfactual at t = 5 yrs after OA", pos = 4, cex = 1)
text(2341, y2, labels = "Downloads at at = 1 yr after OA", pos = 4, cex = 1)
text(2341, y3, labels = "Counterfactual at t = 1 yr after OA", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1772, col="red", lty=2 )

## ADD REGRESSION LINES FOR COUNTERFACTUAL
pred1 <- predict(regTS, itsTest) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 5107), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4769), 
      ylim = c(0, 400),
      xlab = "Time (days)", 
      ylab = "NASEM Daily Report Downloads")

lines( rep(1:1771), pred1[1:1771], col="dodgerblue4", lwd = 3 )
lines( rep(1772:4769), pred1[1772:4769], col="dodgerblue4", lwd = 3 )
lines( rep(1772:4769), pred2[1772:4769], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 200, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(3000, 185, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline( v=1772, col="darkorange2", lty=2 )

install.packages("lmtest")
library("lmtest")
dwtest(Y ~ T + D + P, data=itsTest)

