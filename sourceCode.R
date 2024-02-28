library(ggplot2)
library(tidyverse)
library(dplyr)
library(Lahman)
revisedTeams <- Teams %>%
filter(yearID >= 2005)
head(revisedTeams)
g <- ggplot(data = revisedTeams, aes(x = yearID, y = HRA, color = franchID)) +
geom_point()
gg <- ggplot(data = revisedTeams, aes(x = HRA)) +
geom_histogram(binwidth = 20, color = "deeppink", fill = "darkblue")
g
gg
RevisedTeams2 <- filter(revisedTeams, yearID != 2020)
head(RevisedTeams2)
g <- ggplot(data = RevisedTeams2, aes(x = yearID, y = HRA, color = franchID)) +
geom_point()
gg <- ggplot(data = RevisedTeams2, aes(x = HRA)) +
geom_histogram(binwidth = 20, color = "deeppink", fill = "darkblue")
g
gg
set.seed(1234)
# training set w/ 80% of total data
train <- RevisedTeams2 %>%
dplyr::sample_frac(.8)
# test set with remaining 20% of the data
test <- dplyr::anti_join(RevisedTeams2, train)
# checking to make sure everything is good
nrow(RevisedTeams2)
nrow(train)
nrow(test)
# the number of rows of test and train add up to the number of rows in
# RevisedTeams2 so we are good.
fit <- lm(HRA ~ W + L + RA + ER + ERA + CG + SHO + SV + IPouts + HA + BBA + SOA,
data = train)
summary(fit)
fit1 <- lm(HRA ~ W + L + RA + ER + CG + SHO + SV + IPouts + HA + BBA + SOA,
data = train)
summary(fit1)
fit2 <- lm(HRA ~ W + L + RA + ER + CG + SHO + SV + HA + BBA + SOA,
data = train)
summary(fit2)
fit3 <- lm(HRA ~ W + RA + ER + CG + SHO + SV + HA + BBA + SOA,
data = train)
summary(fit3)
fit4 <- lm(HRA ~ W + RA + ER + SHO + SV + HA + BBA + SOA,
data = train)
summary(fit4)
fit4 <- lm(HRA ~ W + ER + SHO + SV + HA + BBA + SOA,
data = train)
summary(fit4)
fit5 <- lm(HRA ~ W + ER + SHO + HA + BBA + SOA,
data = train)
summary(fit5)
fit6 <- lm(HRA ~ ER + SHO + HA + BBA + SOA,
data = train)
summary(fit6)
res <- resid(fit6)
boxplot(res, col = "lightblue", ylab = "residual values")
plot(fitted(fit6), res, col = "deeppink", xlab = "Home Runs Agianst (HRA)", ylab = "residuals")
abline(0,0)
hist(res, col = "green", xlab = "Residuals", main = "Histogram of Residuals")
predictions <- predict(fit6, test)
head(predictions)
predictDF <- data.frame(test, predictions)
RevisedTeams2DF <- select(predictDF, "yearID", "franchID", "HRA", "predictions")
RevisedTeams2DF$roundedPredictions <- round(RevisedTeams2DF$predictions, 0)
RevisedTeams2DF$TF <- RevisedTeams2DF$HRA == RevisedTeams2DF$roundedPredictions
head(RevisedTeams2DF)
g <- ggplot(data = RevisedTeams2DF, aes(x = roundedPredictions, y = HRA, col = franchID)) +
geom_point() + geom_abline(intercept = 0, slope = 1, color = "red",alpha = 0.5, linewidth = 2)
g
results <- RevisedTeams2DF %>%
group_by(TF) %>%
summarize(count = n())
results
# making 2022 dataset manually from Baseball Reference website
# importing csv file
Teams2022 <- read.csv("2022 Team Pitching stats MLB.csv", header = T)
# cleaning the data set
Teams_2022 <- Teams2022[-(31:32),]
franchID <- c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KCR","ANA", "LAD", "FLA", "MIL", "MIN", "NYM", "NYY", "OAK",   "PHI","PIT","SDP","SEA","SFG","STL","TBD","TEX","TOR","WSN")
Teams__2022 <- data.frame(franchID, Teams_2022)
Teams___2022 <- select(Teams__2022, franchID,HR, ER, tSho, H, BB, SO)
Teams2022Rename <- Teams___2022 %>%
rename(HRA = HR, ER = ER, SHO = tSho, HA = H, BBA = BB, SOA = SO)
head(Teams2022Rename)
predictions2 <- predict(fit6, Teams2022Rename)
head(predictions2)
predictDF2022 <- data.frame(Teams2022Rename, predictions2)
RevisedTeams2DF2022 <- select(predictDF2022, "franchID", "HRA", "predictions2")
RevisedTeams2DF2022$roundedPredictions2 <- round(RevisedTeams2DF2022$predictions2, 0)
RevisedTeams2DF2022$TF <- RevisedTeams2DF2022$HRA == RevisedTeams2DF2022$roundedPredictions2
print(RevisedTeams2DF2022)
results2022 <- RevisedTeams2DF2022 %>%
group_by(TF) %>%
summarize(count = n())
results2022
g <- ggplot(data = RevisedTeams2DF2022, aes(x = roundedPredictions2, y = HRA, col = franchID)) +
geom_point() + geom_abline(intercept = 0, slope = 1, color = "red",alpha = 0.5, linewidth = 2)
g
df <- RevisedTeams2DF2022[c(1,3,6,7,8,10,11,12,13,14,15,16,17,18,19,20,23,24,26,27,28,29),]
df
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Lahman)
revisedTeams <- Teams %>%
filter(yearID >= 2005)
head(revisedTeams)
g <- ggplot(data = revisedTeams, aes(x = yearID, y = HRA, color = franchID)) +
geom_point()
gg <- ggplot(data = revisedTeams, aes(x = HRA)) +
geom_histogram(binwidth = 20, color = "deeppink", fill = "darkblue")
g
gg
RevisedTeams2 <- filter(revisedTeams, yearID != 2020)
head(RevisedTeams2)
g <- ggplot(data = RevisedTeams2, aes(x = yearID, y = HRA, color = franchID)) +
geom_point()
gg <- ggplot(data = RevisedTeams2, aes(x = HRA)) +
geom_histogram(binwidth = 20, color = "deeppink", fill = "darkblue")
g
gg
set.seed(1234)
# training set w/ 80% of total data
train <- RevisedTeams2 %>%
dplyr::sample_frac(.8)
# test set with remaining 20% of the data
test <- dplyr::anti_join(RevisedTeams2, train)
# checking to make sure everything is good
nrow(RevisedTeams2)
nrow(train)
nrow(test)
# the number of rows of test and train add up to the number of rows in
# RevisedTeams2 so we are good.
fit <- lm(HRA ~ W + L + RA + ER + ERA + CG + SHO + SV + IPouts + HA + BBA + SOA,
data = train)
summary(fit)
fit1 <- lm(HRA ~ W + L + RA + ER + CG + SHO + SV + IPouts + HA + BBA + SOA,
data = train)
summary(fit1)
fit2 <- lm(HRA ~ W + L + RA + ER + CG + SHO + SV + HA + BBA + SOA,
data = train)
summary(fit2)
fit3 <- lm(HRA ~ W + RA + ER + CG + SHO + SV + HA + BBA + SOA,
data = train)
summary(fit3)
fit4 <- lm(HRA ~ W + RA + ER + SHO + SV + HA + BBA + SOA,
data = train)
summary(fit4)
fit4 <- lm(HRA ~ W + ER + SHO + SV + HA + BBA + SOA,
data = train)
summary(fit4)
fit5 <- lm(HRA ~ W + ER + SHO + HA + BBA + SOA,
data = train)
summary(fit5)
fit6 <- lm(HRA ~ ER + SHO + HA + BBA + SOA,
data = train)
summary(fit6)
res <- resid(fit6)
boxplot(res, col = "lightblue", ylab = "residual values")
plot(fitted(fit6), res, col = "deeppink", xlab = "Home Runs Agianst (HRA)", ylab = "residuals")
abline(0,0)
hist(res, col = "green", xlab = "Residuals", main = "Histogram of Residuals")
predictions <- predict(fit6, test)
head(predictions)
predictDF <- data.frame(test, predictions)
RevisedTeams2DF <- select(predictDF, "yearID", "franchID", "HRA", "predictions")
RevisedTeams2DF$roundedPredictions <- round(RevisedTeams2DF$predictions, 0)
RevisedTeams2DF$TF <- RevisedTeams2DF$HRA == RevisedTeams2DF$roundedPredictions
head(RevisedTeams2DF)
g <- ggplot(data = RevisedTeams2DF, aes(x = roundedPredictions, y = HRA, col = franchID)) +
geom_point() + geom_abline(intercept = 0, slope = 1, color = "red",alpha = 0.5, linewidth = 2)
g
results <- RevisedTeams2DF %>%
group_by(TF) %>%
summarize(count = n())
results
# making 2022 dataset manually from Baseball Reference website
# importing csv file
Teams2022 <- read.csv("2022 Team Pitching stats MLB.csv", header = T)
# cleaning the data set
Teams_2022 <- Teams2022[-(31:32),]
franchID <- c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KCR","ANA", "LAD", "FLA", "MIL", "MIN", "NYM", "NYY", "OAK",   "PHI","PIT","SDP","SEA","SFG","STL","TBD","TEX","TOR","WSN")
Teams__2022 <- data.frame(franchID, Teams_2022)
Teams___2022 <- select(Teams__2022, franchID,HR, ER, tSho, H, BB, SO)
Teams2022Rename <- Teams___2022 %>%
rename(HRA = HR, ER = ER, SHO = tSho, HA = H, BBA = BB, SOA = SO)
head(Teams2022Rename)
predictions2 <- predict(fit6, Teams2022Rename)
head(predictions2)
predictDF2022 <- data.frame(Teams2022Rename, predictions2)
RevisedTeams2DF2022 <- select(predictDF2022, "franchID", "HRA", "predictions2")
RevisedTeams2DF2022$roundedPredictions2 <- round(RevisedTeams2DF2022$predictions2, 0)
RevisedTeams2DF2022$TF <- RevisedTeams2DF2022$HRA == RevisedTeams2DF2022$roundedPredictions2
print(RevisedTeams2DF2022)
results2022 <- RevisedTeams2DF2022 %>%
group_by(TF) %>%
summarize(count = n())
results2022
g <- ggplot(data = RevisedTeams2DF2022, aes(x = roundedPredictions2, y = HRA, col = franchID)) +
geom_point() + geom_abline(intercept = 0, slope = 1, color = "red",alpha = 0.5, linewidth = 2)
g
df <- RevisedTeams2DF2022[c(1,3,6,7,8,10,11,12,13,14,15,16,17,18,19,20,23,24,26,27,28,29),]
df
