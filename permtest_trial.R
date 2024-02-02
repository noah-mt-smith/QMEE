# Noah Smith QMEE_HW4 

library(tidyverse)
library(EnvStats)

WLdata <- read_csv("WL_soc_clean.csv")

# run permutation test on money allocated to winner (set to one-hundred thousand, maybe overkill)

money.to.winner <- WLdata$prop.money.to.winner

perm.test.money <- oneSamplePermutationTest(money.to.winner, 
        alternative = "two.sided",
        mu = 0.5,
        exact = FALSE,
        n.permutations = 100000)

print(perm.test.money)
plot(perm.test.money)

# run permutation test on coaching hours allocated to winner (set to one-hundred thousand, maybe overkill)

coaching.to.winner <- WLdata$prop.coach.to.winner

perm.test.coaching <- oneSamplePermutationTest(coaching.to.winner, 
        alternative = "two.sided",
        mu = 0.5,
        exact = FALSE,
        n.permutations = 100000)

print(perm.test.coaching)
plot(perm.test.coaching)

# calculate effect size for money to winner


# What i need: confidence intervals of "money to winner" and "coaching to winner" on a 
# plot with x-axis of 0-1 and a line at 0.5