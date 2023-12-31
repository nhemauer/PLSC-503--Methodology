
library(haven)
library(car)
library(tidyverse)
library(kableExtra)
library(dplyr)
library(stargazer)
library(MASS)
library(VGAM)
library(xtable)
library(sjmisc)
library(jtools)
library(margins)

#FIRST USE ANES VARIABLES. GSS VARIABLES ARE WEIRD/DON'T EXIST FOR ALL RESPONDENTS.
#ONLY HAVE ACCESS TO POST VARIABLES IN ANES *******

anes <- read.csv("C:/Users/ndhem/Downloads/anes.csv")

gss <- read_dta("C:/Users/ndhem/Downloads/gss1.dta")



names(gss) <- toupper(names(gss))

finalData <- merge(x = anes, y = gss, by = "YEARID") #Merge data sets by ID

finalData$emoProbs <- finalData$V202629 #Create emotional problems column
finalData$emoProbs <- abs(finalData$emoProbs) #Absolute value

finalData$vote20 <- abs(finalData$V202109x) #Absolute value
finalData <- subset(finalData, emoProbs != 9) #Remove "refuse to answer"

finalData <- subset(finalData, vote20 != 2) #Remove all "dont know" answers
finalData$emoProbs <- as.factor(finalData$emoProbs) #emoProbs factored to Ordinal data

finalData$disPol <- abs(finalData$V202023) #Discussed politics in last 7 days; using as proxy for political interest

finalData$partyAffil <- abs(finalData$V202064)
finalData <- subset(finalData, partyAffil < 5) #Remove those who answered party affiliation other than dem/rep/indp

finalData$InstitutionConfidence <- abs(finalData$V202634)
finalData <- subset(finalData, InstitutionConfidence < 5)

finalData$PoliceFeeling <- abs(finalData$V202171)

#VOTE LOGIT MODEL

finalData <- finalData %>% rename("Age" = "AGE_2", "Education" = "EDUC_2", "DiscPol" = "disPol", "PartyAffiliation" = "partyAffil", "Emotion" = "emoProbs")
#Rename Variables for Output

finalData <- finalData %>% rename("Vote2020" = "vote20")

modelVote <- glm(data = finalData, Vote2020 ~ Emotion + Age + Education + DiscPol + PartyAffiliation + InstitutionConfidence + PoliceFeeling, family = "binomial")
summary(modelVote) #Don't have access to income variable unfortunately

margins <- margins(modelVote, variables = c("Age", "Education", "DiscPol", "Emotion"))
plot(margins)

#DEPRESSION ORDINAL MODEL

modelDepression <- polr(data = finalData, factor(Emotion) ~ Age + Education + DiscPol + PartyAffiliation + InstitutionConfidence + PoliceFeeling)

modelPolice <- lm(data = finalData, PoliceFeeling ~ Age + Education + DiscPol + PartyAffiliation + InstitutionConfidence + Emotion)
  
models <- list(modelVote, modelDepression)


stargazer(models, type = "html", title = "Behavior Models", out = "table1.html")
stargazer(modelPolice, type = "html", title = "Police Feeling 0-100 (Negative to Positive)", out = "police.html")

vifDepression <- data.frame(vif(modelDepression))
vifVote <- data.frame(vif(modelVote))
vifPolice <- data.frame(vif(modelPolice))

resid <- resid(modelPolice)

par(mfrow = c(2, 2))

plot(finalData$Age[1:1084], resid,
     ylab = "Residuals", xlab = "Age",
     main = "Police Feeling")
abline(0, 0) #The Horizon

plot(finalData$Education[1:1084], resid,
     ylab = "Residuals", xlab = "Education",
     main = "Police Feeling")
abline(0, 0) #The Horizon

plot(finalData$DiscussPolitics[1:1084], resid,
     ylab = "Residuals", xlab = "DiscussPolitics",
     main = "Police Feeling")
abline(0, 0) #The Horizon

plot(finalData$EmotionProblems[1:1084], resid,
     ylab = "Residuals", xlab = "EmotionProblems",
     main = "Police Feeling")
abline(0, 0) #The Horizon


#V202634 confidence in people running institutions 1 good, 2 meh, 3 bad

#predict depression? using ordinal model