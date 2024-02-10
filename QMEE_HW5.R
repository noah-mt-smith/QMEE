# My current predictions are that participants will allocate more than 0.5 of their 
# funds and coaching hours to winners. I can construct two linear models to model the effect 
# of my predictors on my two response variables.

library(tidyverse)
library(performance)
WLdata <- read.csv("WL_soc_clean.csv")

# for some reason the diagnostic plots (performance::check_model()) do not work when I use all five levels of gender, so I've
# created a dataframe that only has two levels of gender. 

WLdata_cis <- (WLdata
               %>% filter(grepl("cisgender", participant.gender))
)
money_lm <- lm(prop.money.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis)
check_model(money_lm)

# the check_model() function shows that my model does not meet several important assumptions. Most importantly, it does not 
# the homogeneity of variance assumption, indicating that my response varies inconsistently depending on the levels of my predictor. 

coaching_lm <- lm(prop.coach.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis)
check_model(coaching_lm)

# the 

WLdata_cis_asin <- (WLdata_cis
    %>% mutate(prop.money.to.winner = asin(prop.money.to.winner))
    %>% mutate(prop.coach.to.winner = asin(prop.coach.to.winner))
)

coaching_lm_asin <- lm(prop.coach.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis_asin)
check_model(coaching_lm_asin)
view(WLdata_cis_asin)

WLdata_cis_sqrt <- (WLdata_cis
    %>% mutate(prop.money.to.winner = sqrt(prop.money.to.winner))
    %>% mutate(prop.coach.to.winner = sqrt(prop.coach.to.winner))
)

coaching_lm_sqrt <- lm(prop.coach.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis_sqrt)
check_model(coaching_lm_sqrt)
view(WLdata_cis_sqrt)

WLdata_cis_log <- (WLdata_cis
    %>% mutate(prop.money.to.winner = log1p(prop.money.to.winner))
    %>% mutate(prop.coach.to.winner = log1p(prop.coach.to.winner))
)

coaching_lm_log <- lm(prop.coach.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis_log)
check_model(coaching_lm_log)
view(WLdata_cis_log)

money_lm_log <- lm(prop.money.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis_log)
check_model(money_lm_log)









# Permutation test trial

library(EnvStats)

WLdata <- read_csv("WL_soc_clean.csv")
#View(WLdata)
# run permutation test on money allocated to winner (set to one-hundred thousand, maybe overkill)

money.to.winner <- WLdata$prop.money.to.winner

perm.test.money <- oneSamplePermutationTest(money.to.winner, 
      alternative = "two.sided",
      mu = 0.5,
      exact = FALSE,
      n.permutations = 100000
)

print(perm.test.money)
plot(perm.test.money)

# run permutation test on coaching hours allocated to winner (set to one-hundred thousand, maybe overkill)

coaching.to.winner <- WLdata$prop.coach.to.winner

perm.test.coaching <- oneSamplePermutationTest(coaching.to.winner, 
     alternative = "two.sided",
     mu = 0.5,
     exact = FALSE,
     n.permutations = 100000
)

print(perm.test.coaching)
plot(perm.test.coaching)

# calculate effect size for money to winner


# What i need: confidence intervals of "money to winner" and "coaching to winner" on a 
# plot with x-axis of 0-1 and a line at 0.5