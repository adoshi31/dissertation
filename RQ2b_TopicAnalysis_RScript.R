library(dplyr)
matchedReports <- full_join(x = reports, y = report_names, by = "record_id")
write.csv(matchedReports, file = "matchedReports.csv")
table(matchedReports$title)
summary(matchedReports)
?sort
head(matchedReports$topic)
table(matchedReports$topic)
sort(table(matchedReports$topic))
head(matchedReports$sector)
head(matchedReports$email)
sortedTopics <- sort(table(matchedReports$topic))
head(sortedTopics)
tail(sortedTopics, 25)
names(matchedReports)
head(matchedReports$email, 100)
gmailReports <- subset(matchedReports, subset = (email == "@gmail.com"))
hotmailReports <- subset(matchedReports, subset = (email == "@hotmail.com"))
yahooReports <- subset(matchedReports, subset = (email == "@yahoo.com"))
lycosReports <- subset(matchedReports, subset = (email == "@lycos.com"))
comcastReports <- subset(matchedReports, subset = (email == "@comcast.net"))

personalReports <- rbind(gmailReports, hotmailReports, yahooReports, lycosReports, comcastReports)

sort(table(personalReports$topic))

## Could also look at specific email domains and top topic download ##

sort(table(yahooReports$topic))
sort(table(hotmailReports$topic))
head(gmailReports$date2)
head(gmailReports, 2)
class(gmailReports$date2)
library(lubridate)
library(dplyr)
library(tidyr)

head(gmailReports)
?lubridate
gmailReports$downloadDate <- mdy_hms(gmailReports$date2)
summary(gmailReports$downloadDate)

personalReports$newDate <- as.Date(personalReports$date2, format = "%m/%d/%Y")

fullSlice <- personalReports[personalReports$newDate >= "2003-01-01" & personalReports$newDate <= "2020-01-01",]
itsPrep <- table(fullSlice$newDate, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="personalReports_Oct12_2022.csv")

### Healthcare report topic - Interrupted Time Series Analysis
personalReportsHealthcare <- subset(personalReports, subset = (topic == "Healthcare and Quality"))
fullSliceHealth <- personalReportsHealthcare[personalReportsHealthcare$newDate >= "2003-01-01" & personalReportsHealthcare$newDate <= "2020-01-01",]
itsPrepHealth <- table(fullSliceHealth$newDate, useNA = "ifany")
itsPrepHealth <- as.data.frame(itsPrepHealth)
write.csv(itsPrepHealth, file="personalReportsHEALTHCARE_Oct12_2022.csv")

#fullSliceHealth <- gmailReportsHealth[gmailReportsHealth$newDate >= "2003-01-01" & gmailReportsHealth$newDate <= "2020-02-07",]
#itsPrepHealth <- table(fullSliceHealth$newDate, useNA = "ifany")
#itsPrepHealth <- as.data.frame(itsPrepHealth)
#write.csv(itsPrepHealth, file="itsHealth_gmailReports.csv")

itsTest <- read.csv(file.choose(), header=TRUE)

plot( itsTest$T, itsTest$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 400), xlim=c(0,4200),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "Health Topic Daily Downloads" )

# Line marking the interruption
abline( v=1068, col="firebrick", lty=2 )
text( 1100, 350, "Start of Open Access Policy", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( Y ~ T + D + P, data=itsTest )
lines( itsTest$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm ( Y ~ T + D + P, data=itsTest)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Open Access Effect for Healthcare and Quality Topic"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "OA Policy", 
                                "Time Since OA Policy In Effect"),
           #omit.stat = "all", 
           digits = 2,
           out = "healthcareITSmodel.html")

# We create a small dataset with the new values NOTE: 1068 is treatment date
data1 <- as.data.frame( cbind( T = 1068, D = 1, P = 1 )) 

# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4200), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of open access effect at time = 32)
points( 1068, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 1068, y1, labels = "t = June 2, 2011", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=1068, col="red", lty=2 )

######

data2 <- as.data.frame( cbind( T = 1433, D = 1, P = 365 )) # New data (1 year later)

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4200), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of OA effect at time of policy change
points(1068, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of OA effect at time = 1 yr later)
points(1433, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(1068, y1, labels = "t = predicted", pos = 4, cex = 1)

#Label for the counterfactual 
text(1433, y2, labels = "t = 1 year (365 days) after OA policy", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1068, col="red", lty=2 )

### COUNTERFACTUAL

data3 <- as.data.frame(cbind( T= 1433, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4200), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a  point showing the level of OA EFFECT at time 1 year after OA policy
points(1433, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(1433, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(1433, y2, labels = "Y at t = 1 yr later", pos = 4, cex = 1)

#Label for the counterfactual 
text(1433, y3, labels = "Counterfactual at t = 1 year later", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1068, col="red", lty=2 )

## MORE COUNTERFACTUAL POINTS
## 5 years after OA policy = 2893 (1068 + 5 years or 1825 days)
data4 <- as.data.frame(cbind( T = 2893, D = 1, P = 1825)) 
data5 <- as.data.frame(cbind( T = 2893, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4200), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "Health Topic Downloads")

# OA downloads at time = +365 days
points(1433, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = +365 days
points(1433, y3, col = "darkorange2", pch = 19, cex = 2)

# OA downloads at time = +5 yrs
points(2893, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = +5 yrs
points(2893, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(2893, y4, labels = "Downloads at t = 5 yrs after OA", pos = 4, cex = 1)
text(2893, y5, labels = "Counterfactual at t = 5 yrs after OA", pos = 4, cex = 1)
text(1433, y2, labels = "Downloads at at = 1 yr after OA", pos = 4, cex = 1)
text(1433, y3, labels = "Counterfactual at t = 1 yr after OA", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1068, col="red", lty=2 )

## ADD REGRESSION LINES FOR COUNTERFACTUAL
pred1 <- predict(regTS, itsTest) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 4201), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4200), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "Health Topic Personal Downloads")

lines( rep(1:1067), pred1[1:1067], col="dodgerblue4", lwd = 3 )
lines( rep(1068:4201), pred1[1068:4201], col="dodgerblue4", lwd = 3 )
lines( rep(1068:4201), pred2[1068:4201], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 60, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(3000, 110, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption (use zoom)
abline( v=1068, col="darkorange2", lty=2 )
text( 1100, 350, "Start of Open Access Policy (June 2, 2011)", col="darkorange2", cex=.8, pos=4 )

###

install.packages("lmtest")
library("lmtest")
dwtest(Y ~ T + D + P, data=itsTest)


###################################################################################################################
###################################################################################################################
### Policy, Reviews, Evaluations report topic - Interrupted Time Series Analysis

personalReportsPolicy <- subset(personalReports, subset = (topic == "Policy, Reviews and Evaluations"))
fullSlicePolicy <- personalReportsPolicy[personalReportsPolicy$newDate >= "2003-01-01" & personalReportsPolicy$newDate <= "2020-01-01",]
itsPrepPolicy <- table(fullSlicePolicy$newDate, useNA = "ifany")
itsPrepPolicy <- as.data.frame(itsPrepPolicy)
write.csv(itsPrepPolicy, file="personalReportsPOLICY_Oct13_2022.csv")

itsTest <- read.csv(file.choose(), header=TRUE)

plot( itsTest$T, itsTest$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 400), xlim=c(0,5100),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "Policy Topic Daily Personal Downloads" )

# Line marking the interruption
abline( v=1919, col="firebrick", lty=2 )
text( 2000, 350, "Start of Open Access Policy", col="firebrick", cex=1.3, pos=4 )

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
           out = "policyevalITSmodel.html")

# We create a small dataset with the new values NOTE: 1920 is treatment date
data1 <- as.data.frame( cbind( T = 1919, D = 1, P = 1 )) 

# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5100), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of open access effect at time = 32)
points( 1919, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 1919, y1, labels = "t = June 2, 2011", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=1919, col="red", lty=2 )

######

data2 <- as.data.frame( cbind( T = 2284, D = 1, P = 365 )) # New data (1 year later)

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5100), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of OA effect at time of policy change
points(1919, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of OA effect at time = 1 yr later)
points(2284, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(1919, y1, labels = "t = predicted", pos = 4, cex = 1)

#Label for the counterfactual 
text(2284, y2, labels = "t = 1 year (365 days) after OA policy", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1919, col="red", lty=2 )

### COUNTERFACTUAL

data3 <- as.data.frame(cbind( T= 2284, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5100), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "Policy Topic Daily Personal Downloads")

# We add a  point showing the level of OA EFFECT at time 1 year after OA policy
points(2284, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(2284, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(2284, y2, labels = "Y at t = 1 yr later", pos = 4, cex = 1)

#Label for the counterfactual 
text(2284, y3, labels = "Counterfactual at t = 1 year later", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1919, col="red", lty=2 )

## MORE COUNTERFACTUAL POINTS
## 5 years after OA policy = 3744 (1919 + 5 years or 1825 days)
data4 <- as.data.frame(cbind( T = 3744, D = 1, P = 1825)) 
data5 <- as.data.frame(cbind( T = 3744, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5100), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "Policy Topic Personal Daily Downloads")

# OA downloads at time = +365 days
points(2284, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = +365 days
points(2284, y3, col = "darkorange2", pch = 19, cex = 2)

# OA downloads at time = +5 yrs
points(3744, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = +5 yrs
points(3744, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(3744, y4, labels = "Downloads at t = 5 yrs after OA", pos = 4, cex = 1)
text(3744, y5, labels = "Counterfactual at t = 5 yrs after OA", pos = 4, cex = 1)
text(2284, y2, labels = "Downloads at at = 1 yr after OA", pos = 4, cex = 1)
text(2284, y3, labels = "Counterfactual at t = 1 yr after OA", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1919, col="red", lty=2 )

## ADD REGRESSION LINES FOR COUNTERFACTUAL
pred1 <- predict(regTS, itsTest) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 5050), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5100), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "Policy Topic Personal Daily Downloads")

lines( rep(1:1919), pred1[1:1919], col="dodgerblue4", lwd = 3 )
lines( rep(1919:5050), pred1[1919:5050], col="dodgerblue4", lwd = 3 )
lines( rep(1919:5050), pred2[1919:5050], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 60, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(3000, 110, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption (use zoom)
abline( v=1919, col="darkorange2", lty=2 )
text( 2000, 350, "Start of Open Access Policy (June 2, 2011)", col="darkorange2", cex=.8, pos=4 )

###################################################
###################################################
## Math and Science Education Topic Reports #######
###################################################
###################################################

personalReports <- read.csv(file.choose(), header=TRUE)
personalReportsMath <- subset(personalReports, subset = (topic == "Math and Science Education"))
personalReportsMath$newDate <- as.Date(personalReportsMath$date2, format = "%m/%d/%Y")

fullSliceMath <- personalReportsMath[personalReportsMath$newDate >= "2003-01-01" & personalReportsMath$newDate <= "2020-01-01",]

itsPrepMath <- table(fullSliceMath$newDate, useNA = "ifany")
itsPrepMath <- as.data.frame(itsPrepMath)
write.csv(itsPrepMath, file="personalReportsMATH_Oct18_2022.csv")

itsTest <- read.csv(file.choose(), header=TRUE)

plot( itsTest$T, itsTest$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 1100), xlim=c(0,5100),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "Math and Science Education Topic Daily Personal Downloads" )

# Line marking the interruption
abline( v=1959, col="firebrick", lty=2 )
text( 2000, 350, "Start of Open Access Policy", col="firebrick", cex=1.3, pos=4 )

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
           out = "mathITSmodel.html")

# We create a small dataset with the new values NOTE: 1959 is treatment date
data1 <- as.data.frame( cbind( T = 1959, D = 1, P = 1 )) 

# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5100), 
      ylim = c(0, 1100),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of open access effect at time = 32)
points( 1959, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 1959, y1, labels = "t = June 2, 2011", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=1959, col="red", lty=2 )

######

data2 <- as.data.frame( cbind( T = 2324, D = 1, P = 365 )) # New data (1 year later)

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5100), 
      ylim = c(0, 1100),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of OA effect at time of policy change
points(1959, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of OA effect at time = 1 yr later)
points(2324, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(1959, y1, labels = "t = predicted", pos = 4, cex = 1)

#Label for the counterfactual 
text(2324, y2, labels = "t = 1 year (365 days) after OA policy", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1959, col="red", lty=2 )

### COUNTERFACTUAL

data3 <- as.data.frame(cbind( T= 2324, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5100), 
      ylim = c(0, 1100),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "Math and Science Education Topic Daily Personal Downloads")

# We add a  point showing the level of OA EFFECT at time 1 year after OA policy
points(2324, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(2324, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(2324, y2, labels = "Y at t = 1 yr later", pos = 4, cex = 1)

#Label for the counterfactual 
text(2324, y3, labels = "Counterfactual at t = 1 year later", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1959, col="red", lty=2 )

## MORE COUNTERFACTUAL POINTS
## 5 years after OA policy = 3744 (1919 + 5 years or 1825 days)
data4 <- as.data.frame(cbind( T = 3784, D = 1, P = 1825)) 
data5 <- as.data.frame(cbind( T = 3784, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5100), 
      ylim = c(0, 1100),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "Math and Science Education Topic Personal Daily Downloads")

# OA downloads at time = +365 days
points(2324, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = +365 days
points(2324, y3, col = "darkorange2", pch = 19, cex = 2)

# OA downloads at time = +5 yrs
points(3784, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = +5 yrs
points(3784, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(3784, y4, labels = "Downloads at t = 5 yrs after OA", pos = 4, cex = 1)
text(3784, y5, labels = "Counterfactual at t = 5 yrs after OA", pos = 4, cex = 1)
text(2324, y2, labels = "Downloads at at = 1 yr after OA", pos = 4, cex = 1)
text(2324, y3, labels = "Counterfactual at t = 1 yr after OA", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1959, col="red", lty=2 )

## ADD REGRESSION LINES FOR COUNTERFACTUAL
pred1 <- predict(regTS, itsTest) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 5100), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 5100), 
      ylim = c(0, 1100),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "Math and Science Education Topic Personal Daily Downloads")

lines( rep(1:1959), pred1[1:1959], col="dodgerblue4", lwd = 3 )
lines( rep(1959:5049), pred1[1959:5049], col="dodgerblue4", lwd = 3 )
lines( rep(1959:5049), pred2[1959:5049], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 60, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(3000, 110, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption (use zoom)
abline( v=1959, col="darkorange2", lty=2 )
text( 2000, 350, "Start of Open Access Policy (June 2, 2011)", col="darkorange2", cex=.8, pos=4 )

################################################################
################################################################
########## K-12 Education Topic Reports ########################
################################################################
################################################################

personalReports <- read.csv(file.choose(), header=TRUE)
personalReportsK12 <- subset(personalReports, subset = (topic == "K-12 Education"))
personalReportsK12$newDate <- as.Date(personalReportsK12$date2, format = "%m/%d/%Y")

fullSliceK12 <- personalReportsK12[personalReportsK12$newDate >= "2003-01-01" & personalReportsK12$newDate <= "2020-01-01",]

itsPrepK12 <- table(fullSliceK12$newDate, useNA = "ifany")
itsPrepK12 <- as.data.frame(itsPrepK12)
write.csv(itsPrepK12, file="personalReportsK12_Oct19_2022.csv")

itsTest <- read.csv(file.choose(), header=TRUE)

plot( itsTest$T, itsTest$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 400), xlim=c(0,4500),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "K-12 Education Topic Daily Personal Downloads" )

# Line marking the interruption
abline( v=1200, col="firebrick", lty=2 )
text( 1300, 350, "Start of Open Access Policy", col="firebrick", cex=1.3, pos=4 )

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
           out = "k12ITSmodel.html")

# We create a small dataset with the new values NOTE: 1200 is treatment date
data1 <- as.data.frame( cbind( T = 1200, D = 1, P = 1 )) 

# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4300), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of open access effect at time = 32)
points( 1200, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 1200, y1, labels = "t = June 2, 2011", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=1200, col="red", lty=2 )

######

data2 <- as.data.frame( cbind( T = 1565, D = 1, P = 365 )) # New data (1 year later)

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4300), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "OA Effect Index")

# We add a point showing the level of OA effect at time of policy change
points(1200, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of OA effect at time = 1 yr later)
points(1565, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(1200, y1, labels = "t = predicted", pos = 4, cex = 1)

#Label for the counterfactual 
text(1565, y2, labels = "t = 1 year (365 days) after OA policy", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1200, col="red", lty=2 )

### COUNTERFACTUAL

data3 <- as.data.frame(cbind( T= 1565, D = 0, P = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4300), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "K-12 Education Topic Daily Personal Downloads")

# We add a  point showing the level of OA EFFECT at time 1 year after OA policy
points(1565, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(1565, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(1565, y2, labels = "Y at t = 1 yr later", pos = 4, cex = 1)

#Label for the counterfactual 
text(1565, y3, labels = "Counterfactual at t = 1 year later", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1200, col="red", lty=2 )

## MORE COUNTERFACTUAL POINTS
## 5 years after OA policy = 3744 (1200 + 5 years or 1825 days)
data4 <- as.data.frame(cbind( T = 3025, D = 1, P = 1825)) 
data5 <- as.data.frame(cbind( T = 3025, D = 0, P = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4300), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "K-12 Education Topic Personal Daily Downloads")

# OA downloads at time = +365 days
points(1565, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = +365 days
points(1565, y3, col = "darkorange2", pch = 19, cex = 2)

# OA downloads at time = +5 yrs
points(3025, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time = +5 yrs
points(3025, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(3025, y4, labels = "Downloads at t = 5 yrs after OA", pos = 4, cex = 1)
text(3025, y5, labels = "Counterfactual at t = 5 yrs after OA", pos = 4, cex = 1)
text(1565, y2, labels = "Downloads at at = 1 yr after OA", pos = 4, cex = 1)
text(1565, y3, labels = "Counterfactual at t = 1 yr after OA", pos = 4, cex = 1)

# Line marking the interruption
abline( v=1200, col="red", lty=2 )

## ADD REGRESSION LINES FOR COUNTERFACTUAL
pred1 <- predict(regTS, itsTest) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 4300), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( itsTest$Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 4300), 
      ylim = c(0, 400),
      xlab = "Time (days), January 2003 - January 2020", 
      ylab = "K-12 Education Topic Personal Daily Downloads")

lines( rep(1:1200), pred1[1:1200], col="dodgerblue4", lwd = 3 )
lines( rep(1200:4335), pred1[1200:4335], col="dodgerblue4", lwd = 3 )
lines( rep(1200:4335), pred2[1200:4335], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 60, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(3000, 110, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption (use zoom)
abline( v=1200, col="darkorange2", lty=2 )
text( 1300, 350, "Start of Open Access Policy (June 2, 2011)", col="darkorange2", cex=.8, pos=4 )



