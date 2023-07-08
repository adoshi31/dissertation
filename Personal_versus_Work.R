


commentData <- read.csv(file.choose(), header=TRUE)
downloadData <- read.csv(file.choose(), header=TRUE)
bookData <- read.csv(file.choose(), header=TRUE)

matchedComments <- read.csv(file.choose(), header=TRUE) ## Matched Comments CSV in dissertation folder
                
library(dplyr)
# matchedComments <- left_join(commentData, bookData, by="record_id")
names(matchedComments)

updatedMatchedComments <- left_join(commenterUsers.df, matchedComments, by="user.number")

## Still need to change data to factors...

matchedComments$user.number <- as.factor(matchedComments$user.number)
matchedComments$category <- as.factor(matchedComments$category)                                         
matchedComments$title <- as.factor(matchedComments$title)
matchedComments$subtitle <- as.factor(matchedComments$subtitle)
matchedComments$topic <- as.factor(matchedComments$topic)                            
matchedComments$topic.simplification <- as.factor(matchedComments$topic.simplification)
matchedComments$field <- as.factor(matchedComments$field)                                                  
matchedComments$email <- as.factor(matchedComments$email)
matchedComments$copyright <- as.factor(matchedComments$copyright)

aviator <- subset(matchedComments, subset = (user.number == "378700")) # turns out that 378700 is a professor (not a student)
healthcare_downloader <- subset(matchedComments, subset = (user.number == "653531")) # designs apps but also uses NASEM reports for family
aviator2 <- subset(matchedComments, subset = (user.number == "g1734700")) # definitely in aviation
mathsci <- subset(matchedComments, subset = (user.number == "606307")) ## teacher
children <- subset(matchedComments, subset = (user.number == "838081")) ## seems to be a student who has summers "off" and a great affinity for NASEM sources (grad?)
policy <- subset(matchedComments, subset = (user.number == "859643")) ## difficult to say -- maybe a self-motivated learner
mathsci2 <- subset(matchedComments, subset = (user.number == "224165")) ## a dentist mainly reading NASEM reports for "personal edification"
user_653531 <- subset(matchedComments, subset = (user.number == "653531"))
user_257 <- subset(matchedComments, subset = (user.number == "257"))
gpc <- subset(matchedComments, subset = (email == "@gpc.edu"))

matchedPersonal <- subset(matchedComments, subset = (category == "personal"))
matchedLearnAbout <- subset(matchedComments, subset = (category == "learn about"))
matchedRead <- subset(matchedComments, subset = (category == "read"))
matchedSharing <- subset(matchedComments, subset = (category == "sharing"))
matchedFamily <- subset(matchedComments, subset = (category == "family"))
matchedGratitude <- subset(matchedComments, subset = (category == "gratitude"))
matchedSerious <- subset(matchedComments, subset = (category == "SL"))
matchedRant <- subset(matchedComments, subset = (category == "rant"))
matchedUncertain <- subset(matchedComments, subset = (category == "uncertain"))
matchedDelight <- subset(matchedComments, subset = (category == "delight"))
matchedHomeSchool <- subset(matchedComments, subset = (category == "homeschool"))
matchedjobTransition <- subset(matchedComments, subset = (category == "job transition"))
matchedSkills <- subset(matchedComments, subset = (category == "skills"))
matchedRetired <- subset(matchedComments, subset = (category == "retired"))
matchedArgue <- subset(matchedComments, subset = (category == "argue"))
matchedIllness <- subset(matchedComments, subset = (category == "illness"))
matchedVeteran <- subset(matchedComments, subset = (category == "veteran"))


newMatched <- bind_rows(matchedFamily, matchedHomeSchool, 
                        matchedPersonal, matchedSerious, 
                        matchedVeteran, matchedIllness, 
                        matchedArgue, matchedRetired, 
                        matchedSkills, matchedjobTransition)

# for comparing Personal vs Work domains:

gmailOverall <- subset(matchedComments, subset = (email == "@gmail.com"))
yahooOverall <- subset(matchedComments, subset = (email == "@yahoo.com"))
hotmailOverall <- subset(matchedComments, subset = (email == "@hotmail.com"))
table(gmailOverall$category)
table(yahooOverall$category)
table(hotmailOverall$category) 

addmargins(table(gmailOverall$category))
addmargins(table(yahooOverall$category))
addmargins(table(hotmailOverall$category))

commenterUsers.df <- as.data.frame(table(newMatched$user.number))
commenterUsers.df$logic <- (commenterUsers.df$Freq >= 2)
commenterUsers.df <- as.data.frame(commenterUsers.df)

commenterUsers.df <- subset(commenterUsers.df, subset = (logic == "TRUE"))


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

matchedFrustration <- 
write.csv(matchedFrustration, file="matchedFrustratation.csv")
text <- read.csv(file.choose(), encoding = "iso-8859-1")
Encoding(text$LongComments) <- "latin1"
iconv(text$LongComments, "latin1", "ASCII", sub="")

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(text$LongComments)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)


## adding column for logistic regression: personal vs work

matchedComments$personalUsage <- ifelse(matchedComments$category == "personal", 1, 0)

model2 <- lm(pers_vs_work ~ topic + downloads + 
                            altmetric.score + comments, 
                            data=matchedComments)

model3 <- lm(pers_vs_work ~ downloads + 
               altmetric.score, 
             data=matchedComments)

## regression results
stargazer(model2, report = ('vct*'), type="html", out="NASEM_personal_regression.html", 
          title="Table 2: Covariates associated with classification of NASEM report use as personal")

stargazer(model3, report = ('vct*'), type="html", out="Altmetric_NASEM_personal_regression.html", 
          title="Table 3: Effect of Altmetric Score on classification of NASEM report use as personal")

model1 <- glm(pers_vs_work ~ downloads + altmetric.score + comments + current.dwnlds,
          family=binomial(link=logit),
          data=matchedComments)


summary(model1)


matchedComments$workUsage <- ifelse(matchedComments$category != "personal", 1, 0)

model5 <- glm(workUsage ~ downloads + altmetric.score + comments + current.dwnlds,
              family=binomial(link=logit),
              data=matchedComments)

summary(model5)

model6 <- lm(workUsage ~ downloads + altmetric.score + comments + current.dwnlds, 
             data=matchedComments)

summary(model6)

model7 <- lm(workUsage ~ downloads + altmetric.score + comments + current.dwnlds
             + topic, data=matchedComments)

summary(model7)
stargazer(model7, report = ('vct*'), type="html", out="Regression_workUsage_dep_var.html")





install.packages("randomForest")
library(randomForest)
library(ggplot2)

set.seed(4545)
rf.fit3 <- randomForest(workUsage ~ topic, 
                       data=matchedComments, ntree=10,
                       keep.forest=FALSE, importance=TRUE, na.action = na.omit)

rf.fit

### Visualize variable importance ----------------------------------------------

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

matchedCommentsClean <- matchedComments[matchedComments$topic != " | ", ]

##################################################3
# these two functions produce the BIC and
# the log likelihood for the model named L1

BIC(model1)
logLik(model1)

# This code uses the exponetial function to 
# compute factor changes in the odds
# for every independent variable in the model

round(exp(coef(model1)),digits=2)


matchedPersonal$copyright <- as.factor(matchedPersonal$copyright)
matchedPersonal$email <- as.factor(matchedPersonal$email)

vaGov <- subset(matchedPersonal, subset = (email == "@va.gov"))
epaGov <- subset(matchedPersonal, subset = (email == "@epa.gov"))

topPersonal_170 <- subset(matchedPersonal, subset = (user.number == "77222"))

twoWay <- read.csv(file.choose(), header=TRUE)
cor(twoWay$personal, twoWay$work)
plot(twoWay$personal, twoWay$work, main="NASEM Report Comments: Personal vs Work Use", 
     sub="Classified by BERT NLP algorithm, 2022", 
     xlab="Unique users with comments indicating personal use", 
     ylab="Unique users with comments indicating work use")
# legend(400, 200, legend=c("Personal use", "Work use"),
# col=c("red", "black"), fill=c("red","black"), cex=0.8)

############################################3
#############################################

install.packages("FactoMineR")
install.packages("vcd")
install.packages("factoextra")
install.packages("missMDA")

library(FactoMineR)
library(vcd)
library(factoextra)
library(missMDA)

matchedPersonal_famd <- FAMD(matchedPersonal, graph=FALSE)
vaGov_famd <- FAMD(vaGov, graph=TRUE)


vaGov_famd1 <- FAMD(vaGov_famd, graph=TRUE)

fviz_famd_ind(vaGov_famd,col.ind = "cos2",
              gradient.cols = c("blue", "orange", "red"),
              repel = TRUE)

vaGov_mca <- MCA(vaGov_famd,
                     ncp = 3,
                     graph = FALSE)

 
fviz_mca_biplot(vaGov_famd,
                repel = TRUE,
                ggtheme = theme_minimal())

