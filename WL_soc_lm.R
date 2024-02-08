# logistic regression attempt, I don't think this works but idk..
library(tidyverse)

WLdata <- read_csv("WL_soc_clean.csv")

WLdata$binary_money <- ifelse(WLdata$prop.money.to.winner != 0.5, 1, 0)

logistic_money <- glm(binary_money ~ 1, family = binomial, data = WLdata)

summary(logistic_money)

WLdata$binary_coaching <- ifelse(WLdata$prop.coach.to.winner != 0.5, 1, 0)

logistic_coaching <- glm(binary_coaching ~ 1, family = binomial, data = WLdata)

summary(logistic_coaching)

# I have no clue how to interpret this...