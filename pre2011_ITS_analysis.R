library(lubridate)
library(dplyr)
library(tidyr)
library(stargazer)

fullDownloads <- read.csv(file.choose(), header=TRUE)
bookMetadata <- read.csv(file.choose(), header=TRUE)
pre2011copyright <- left_join(fullDownloads, bookMetadata, by = "record_id")

names(bookMetadata)
names(fullDownloads)

names(pre2011copyright)
class(pre2011copyright$copyright)
summary(pre2011copyright$copyright)
table(pre2011copyright$copyright)

pre2011downloads <- subset(pre2011copyright, subset = (copyright < 2011))
table(pre2011downloads$copyright)

gmailReports <- subset(pre2011downloads, subset = (email == "@gmail.com"))
hotmailReports <- subset(pre2011downloads, subset = (email == "@hotmail.com"))
yahooReports <- subset(pre2011downloads, subset = (email == "@yahoo.com"))
lycosReports <- subset(pre2011downloads, subset = (email == "@lycos.com"))
comcastReports <- subset(pre2011downloads, subset = (email == "@comcast.net"))
aolReports <- subset(pre2011downloads, subset = (email == "@aol.com"))
verizonReports <- subset(pre2011downloads, subset = (email == "@verizon.net"))
msnReports <- subset(pre2011downloads, subset = (email == "@msn.com"))
sbcglobalReports <- subset(pre2011downloads, subset = (email == "@sbcglobal.net"))
macReports <- subset(pre2011downloads, subset = (email == "@mac.com"))
attReports <- subset(pre2011downloads, subset = (email == "@att.net"))
earthlinkReports <- subset(pre2011downloads, subset = (email == "@earthlink.net"))
coxReports <- subset(pre2011downloads, subset = (email == "@cox.net"))
meReports <- subset(pre2011downloads, subset = (email == "@me.com"))
liveReports <- subset(pre2011downloads, subset = (email == "@live.com"))
bellsouthReports <- subset(pre2011downloads, subset = (email == "@bellsouth.net"))
icloudReports <- subset(pre2011downloads, subset = (email == "@icloud.com"))
ymailReports <- subset(pre2011downloads, subset = (email == "@ymail.com"))
charterReports <- subset(pre2011downloads, subset = (email == "@charter.net"))
gmailCapReports <- subset(pre2011downloads, subset = (email == "@GMAIL.COM"))
mindspringReports <- subset(pre2011downloads, subset = (email == "@mindspring.com"))
junoReports <- subset(pre2011downloads, subset = (email == "@juno.com"))
# roadrunnerRRReports <- subset(pre2011downloads, subset = (email == ".rr.com")) ## all road runner RR emails
roadrunnerReports <- subset(pre2011downloads, subset = (email == "@roadrunner.com"))
rocketmailReports <- subset(pre2011downloads, subset = (email == "@rocketmail.com"))
aimReports <- subset(pre2011downloads, subset = (email == "@aim.com"))
mailReports <- subset(pre2011downloads, subset = (email == "@mail.com"))
yahooCapitalReports <- subset(pre2011downloads, subset = (email == "@YAHOO.COM"))
## allOtherYahooReports 
frontierReports <- subset(pre2011downloads, subset = (email == "@frontier.com"))
aolCapitalReports <- subset(pre2011downloads, subset = (email == "@AOL.COM"))
csReports <- subset(pre2011downloads, subset = (email == "@cs.com"))
hotmailCapitalReports <- subset(pre2011downloads, subset = (email == "@HOTMAIL.COM"))
protonmailReports <- subset(pre2011downloads, subset = (email == "@protonmail.com"))
exciteReports <- subset(pre2011downloads, subset = (email == "@excite.com"))
compuserveReports <- subset(pre2011downloads, subset = (email == "@compuserve.com"))
twcReports <- subset(pre2011downloads, subset = (email == "@twc.com"))
mailinatorReports <- subset(pre2011downloads, subset = (email == "@mailinator.com"))
pacbellReports <- subset(pre2011downloads, subset = (email == "@pacbell.net"))


pre2011PersonalReports <- rbind(gmailReports, hotmailReports, yahooReports, lycosReports, comcastReports,
                                aolReports, verizonReports, msnReports, sbcglobalReports, macReports,
                                attReports, earthlinkReports, coxReports, meReports, liveReports, bellsouthReports,
                                icloudReports, ymailReports, charterReports, gmailCapReports, mindspringReports,
                                junoReports, roadrunnerReports, rocketmailReports, aimReports, mailReports, 
                                yahooCapitalReports, frontierReports, aolCapitalReports, csReports, hotmailCapitalReports,
                                protonmailReports, exciteReports, compuserveReports, twcReports, mailinatorReports,
                                pacbellReports)

 ## Total of 1076133 observations of personal email addresses

pre2011PersonalReports$newDate <- as.Date(pre2011PersonalReports$date2, format = "%m/%d/%Y")

fullSlice <- pre2011PersonalReports[pre2011PersonalReports$newDate >= "2003-01-01" & pre2011PersonalReports$newDate <= "2020-01-01",]
itsPrep <- table(fullSlice$newDate, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="pre2011PersonalReports_Feb15_2023.csv")

### file written on 2/13/23. Still need to update CSV per instructions (add 1's and 0's etc)

dataTS <- read.csv(file.choose(), header=TRUE)
names(dataTS)
summary(dataTS)

plot( dataTS$T, dataTS$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 5850), xlim=c(0,5150),  # note: height is 5040, 5833 top download day, and length is 5150, 5146 days
      xlab = "Time (days)", 
      ylab = "NASEM Report Downloads" )

# Line marking the interruption
abline( v=2014, col="firebrick", lty=2 )
text( 2015, 3000, "Start of Open Access Policy on June 1, 2011", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( dataTS$Y ~ dataTS$T + dataTS$D + dataTS$P)
lines( dataTS$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm(Y ~ T + D + P, data=dataTS)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Downloads"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "Treatment", 
                                "Time Since Treatment"),
           omit.stat = "all", 
           digits = 2,
           out = "pre2011copyright_regression_personal_v3.htm")


#### Agent Orange Report #####
### Only looking at ostensibly personal email domains ###
agentOrangeReport <- subset(pre2011PersonalReports, subset = (title == "Veterans and Agent Orange Update 2008"))
agentOrangeReport$newDate <- as.Date(agentOrangeReport$date2, format = "%m/%d/%Y")

fullSlice <- agentOrangeReport[agentOrangeReport$newDate >= "2003-01-01" & agentOrangeReport$newDate <= "2020-01-01",]
fullSlice$newDate2 <- as.factor(fullSlice$newDate)
itsPrep <- table(fullSlice$newDate, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="agentOrangeReports_May10_2023.csv")

### NOTE: Still need to update CSV per instructions (add 1's and 0's etc)

dataTS <- read.csv(file.choose(), header=TRUE)
names(dataTS)
summary(dataTS)

plot( dataTS$T, dataTS$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 100), xlim=c(0,1250),  # note: height is 100, 88 top download day, and length is 1250, 1213 days
      xlab = "Time (days)", 
      ylab = "NASEM Report Downloads by Non-Researchers: Veterans and Agent Orange Update 2008" )

# Line marking the interruption
abline( v=451, col="firebrick", lty=2 )
text( 500, 100, "Start of Open Access Policy on June 1, 2011", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( dataTS$Y ~ dataTS$T + dataTS$D + dataTS$P)
lines( dataTS$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm(Y ~ T + D + P, data=dataTS)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Downloads"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "Treatment", 
                                "Time Since Treatment"),
           omit.stat = "all", 
           digits = 2,
           out = "agentOrangeReport_personalDownloads.htm")

## Marijuana report ###
### Only looking at ostensibly personal email domains ###
marijuanaReport <- subset(pre2011PersonalReports, subset = (title == "Marijuana As Medicine? The Science Beyond the Controversy"))
marijuanaReport$newDate <- as.Date(marijuanaReport$date2, format = "%m/%d/%Y")

fullSlice <- marijuanaReport[marijuanaReport$newDate >= "2003-01-01" & marijuanaReport$newDate <= "2020-01-01",]
itsPrep <- table(fullSlice$newDate, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="marijuanaReports_May10_2023.csv")

### NOTE: Still need to update CSV per instructions (add 1's and 0's etc)

dataTS <- read.csv(file.choose(), header=TRUE)
names(dataTS)
summary(dataTS)

plot( dataTS$T, dataTS$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 20), xlim=c(0,1645),  # note: height is 10, 8 top download day, and length is 1645 days
      xlab = "Time (days)", 
      ylab = "NASEM Report Downloads by Non-Researchers: Marijuana As Medicine? The Science Beyond the Controversy
" )

# Line marking the interruption
abline( v=15, col="firebrick", lty=2 )
text( 15, 10, "Start of Open Access Policy on June 1, 2011", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( dataTS$Y ~ dataTS$T + dataTS$D + dataTS$P)
lines( dataTS$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm(Y ~ T + D + P, data=dataTS)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Downloads"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "Treatment", 
                                "Time Since Treatment"),
           omit.stat = "all", 
           digits = 2,
           out = "marijuanaReport_personalDownloads.htm")

### GMOs reports ###

### Only looking at ostensibly personal email domains ###
gmoReport <- subset(pre2011PersonalReports, subset = (title == "The Impact of Genetically Engineered Crops on Farm Sustainability in the United States"))
gmoReport$newDate <- as.Date(gmoReport$date2, format = "%m/%d/%Y")

fullSlice <- gmoReport[gmoReport$newDate >= "2003-01-01" & gmoReport$newDate <= "2020-01-01",]
itsPrep <- table(fullSlice$newDate, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="gmoReports_May10_2023.csv")

### NOTE: Still need to update CSV per instructions (add 1's and 0's etc)

dataTS <- read.csv(file.choose(), header=TRUE)
names(dataTS)
summary(dataTS)

plot( dataTS$T, dataTS$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 20), xlim=c(0,1645),  # note: height is 10, 8 top download day, and length is 1645 days
      xlab = "Time (days)", 
      ylab = "NASEM Report Downloads by Non-Researchers: The Impact of Genetically Engineered Crops on Farm Sustainability in the United States" )

# Line marking the interruption
abline( v=15, col="firebrick", lty=2 )
text( 15, 10, "Start of Open Access Policy on June 1, 2011", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( dataTS$Y ~ dataTS$T + dataTS$D + dataTS$P)
lines( dataTS$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm(Y ~ T + D + P, data=dataTS)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Downloads"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "Treatment", 
                                "Time Since Treatment"),
           omit.stat = "all", 
           digits = 2,
           out = "gmoReport_personalDownloads.htm")



### Einstein report: Einstein's Unfinished Symphony Listening to the Sounds of Space-Time
 ###
### Only looking at ostensibly personal email domains ###
einReport <- subset(pre2011PersonalReports, subset = (title == "Einstein's Unfinished Symphony Listening to the Sounds of Space-Time"))
einReport$newDate <- as.Date(einReport$date2, format = "%m/%d/%Y")

fullSlice <- einReport[einReport$newDate >= "2003-01-01" & einReport$newDate <= "2020-01-01",]
itsPrep <- table(fullSlice$newDate, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="einsteinReports_May10_2023.csv")

### NOTE: Still need to update CSV per instructions (add 1's and 0's etc)

dataTS <- read.csv(file.choose(), header=TRUE)
names(dataTS)
summary(dataTS)

plot( dataTS$T, dataTS$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 950), xlim=c(0,20),  # note: height is 10, 8 top download day, and length is 1645 days
      xlab = "Time (days)", 
      ylab = "NASEM Report Downloads by Non-Researchers: Einstein's Unfinished Symphony Listening to the Sounds of Space-Time" )

# Line marking the interruption
abline( v=3, col="firebrick", lty=2 )
text( 4, 600, "Start of Open Access Policy on June 1, 2011", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( dataTS$Y ~ dataTS$T + dataTS$D + dataTS$P)
lines( dataTS$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm(Y ~ T + D + P, data=dataTS)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Downloads"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "Treatment", 
                                "Time Since Treatment"),
           omit.stat = "all", 
           digits = 2,
           out = "einsteinReport_personalDownloads.htm")



###########################################
### Science, Evolution, and Creationism ###
###########################################

### Only looking at ostensibly personal email domains ###
createReport <- subset(pre2011PersonalReports, subset = (title == "Science, Evolution, and Creationism"))
createReport$newDate <- as.Date(createReport$date2, format = "%m/%d/%Y")

fullSlice <- createReport[createReport$newDate >= "2003-01-01" & createReport$newDate <= "2020-01-01",]
itsPrep <- table(fullSlice$newDate, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="createReports_May17_2023.csv")

### NOTE: Still need to update CSV per instructions (add 1's and 0's etc)

dataTS <- read.csv(file.choose(), header=TRUE)
names(dataTS)
summary(dataTS)

plot( dataTS$T, dataTS$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 600), xlim=c(0,3500),  # note: height is 1050, 8 top download day, and length is 3500 days
      xlab = "Time (days)", 
      ylab = "NASEM Report Downloads by Non-Researchers: Science, Evolution, and Creationism" )

# Line marking the interruption
abline( v=1087, col="firebrick", lty=2 )
text( 1300, 500, "Start of Open Access Policy on June 1, 2011", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( dataTS$Y ~ dataTS$T + dataTS$D + dataTS$P)
lines( dataTS$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm(Y ~ T + D + P, data=dataTS)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Downloads"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "Treatment", 
                                "Time Since Treatment"),
           omit.stat = "all", 
           digits = 2,
           out = "createReport_personalDownloads.htm")

######################################################################################
### Principles and Practices for a Federal Statistical Agency Fourth Edition ###
######################################################################################

### Only looking at ostensibly personal email domains ###
statsReport <- subset(pre2011PersonalReports, subset = (title == "Principles and Practices for a Federal Statistical Agency Third Edition"))
statsReport$newDate <- as.Date(statsReport$date2, format = "%m/%d/%Y")

fullSlice <- statsReport[statsReport$newDate >= "2003-01-01" & statsReport$newDate <= "2020-01-01",]
itsPrep <- table(fullSlice$newDate, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="fedstatsReport_May31_2023.csv")

### NOTE: Still need to update CSV per instructions (add 1's and 0's etc)

dataTS <- read.csv(file.choose(), header=TRUE)
names(dataTS)
summary(dataTS)

plot( dataTS$T, dataTS$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 600), xlim=c(0,3500),  # note: height is 1050, 8 top download day, and length is 3500 days
      xlab = "Time (days)", 
      ylab = "NASEM Report Downloads by Non-Researchers: Federal Statistics" )

# Line marking the interruption
abline( v=1087, col="firebrick", lty=2 )
text( 1300, 500, "Start of Open Access Policy on June 1, 2011", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( dataTS$Y ~ dataTS$T + dataTS$D + dataTS$P)
lines( dataTS$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm(Y ~ T + D + P, data=dataTS)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Downloads"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "Treatment", 
                                "Time Since Treatment"),
           omit.stat = "all", 
           digits = 2,
           out = "fedstatsReport_personalDownloads.htm")




######################################################################################
# Using the American Community Survey for the National Science Foundation's Science and Engineering Workforce Statistics Programs
######################################################################################

### Only looking at ostensibly personal email domains ###
acsReport <- subset(pre2011PersonalReports, subset = (title == "Using the American Community Survey for the National Science Foundation's Science and Engineering Workforce Statistics Programs"))
acsReport$newDate <- as.Date(acsReport$date2, format = "%m/%d/%Y")

fullSlice <- acsReport[acsReport$newDate >= "2003-01-01" & acsReport$newDate <= "2020-01-01",]
itsPrep <- table(fullSlice$newDate, useNA = "ifany")
itsPrep <- as.data.frame(itsPrep)
write.csv(itsPrep, file="acsReport_May31_2023.csv")

### NOTE: Still need to update CSV per instructions (add 1's and 0's etc)

dataTS <- read.csv(file.choose(), header=TRUE)
names(dataTS)
summary(dataTS)

plot( dataTS$T, dataTS$Y,
      bty="n", pch=19, col="gray",
      ylim = c(0, 8), xlim=c(0,100),  # note: height is 1050, 8 top download day, and length is 3500 days
      xlab = "Time (days)", 
      ylab = "NASEM Report Downloads by Non-Researchers: ACS Report" )

# Line marking the interruption
abline( v=24, col="firebrick", lty=2 )
text( 25, 7, "Start of Open Access Policy on June 1, 2011", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( dataTS$Y ~ dataTS$T + dataTS$D + dataTS$P)
lines( dataTS$T, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm(Y ~ T + D + P, data=dataTS)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("Downloads"),
           column.labels = ("Model results"),
           covariate.labels = c("Time", "Treatment", 
                                "Time Since Treatment"),
           omit.stat = "all", 
           digits = 2,
           out = "acsReport_personalDownloads.htm")
