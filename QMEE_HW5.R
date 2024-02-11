# My current predictions are that participants will allocate more than 0.5 of their 
# funds and coaching hours to winners. I can construct two linear models to model the effect 
# of my predictors on my two response variables.

# update 

library(tidyverse)
library(performance)
library(EnvStats)
library(lsr)

WLdata <- read.csv("WL_soc_clean.csv")

# for some reason the diagnostic plots (performance::check_model()) do not work when I use all five levels of gender, so I've
# created a dataframe that only has two levels of gender. 

WLdata_cis <- (WLdata
               %>% filter(grepl("cisgender", participant.gender))
)

money_lm <- lm(prop.money.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis)
performance::check_model(money_lm)

# the check_model() function shows that my model 'money_lm' does not meet several assumptions. Importantly, it does not 
# the homogeneity of variance assumption, indicating that my response variable varies inconsistently depending on the levels of my predictor. 

coaching_lm <- lm(prop.coach.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis)
performance::check_model(coaching_lm)

# My coaching_lm violates even more assumptions than my money_lm, and the posterior predicive check is especially odd. However, 
# at least the observed data in the posterior predictive check somewhat resembles the predicted (blue) line, as they are 
# both normal (although the observed data is far noisier)

# In general, both of these lms violate several of the assumptions, and I think (most importantly) the homogeneity of variance
# assumption.

# Below I run through various transformations to see if I can get my linear model to meet the homogeneity of variance assumption.
# Importantly, I am not plotting any summary tables that include the significance values for any of these linear models. 
# I really just wanted to see if any of these transformations could help the weird skewness of my data, especially the coaching 
# data.

# I started with an arcsine transformation because on tuesday Dr. Bolker said this is sometimes (but not always) appropriate for proportion data.

WLdata_cis_asin <- (WLdata_cis
    %>% mutate(prop.money.to.winner = asin(prop.money.to.winner))
    %>% mutate(prop.coach.to.winner = asin(prop.coach.to.winner))
)

money_lm_asin <-lm(prop.money.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis_asin)
coaching_lm_asin <- lm(prop.coach.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis_asin)
performance::check_model(money_lm_asin)
performance::check_model(coaching_lm_asin)


# Heteroscedasticity looks a bit lower in the arcsine transformation, especially for the "money" linear model. 
# However, The obvious issue with this is that it's hard for me to interpret this transformed data with regards to my null, 
# as I now have (transformed) proportion values that are above 1. So I believe that the arcsine transformation is probably 
# not the way to go, and I'm going to try a square root transformation here instead.

WLdata_cis_sqrt <- (WLdata_cis
    %>% mutate(prop.money.to.winner = sqrt(prop.money.to.winner))
    %>% mutate(prop.coach.to.winner = sqrt(prop.coach.to.winner))
)

money_lm_sqrt <- lm(prop.money.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis_sqrt)
coaching_lm_sqrt <- lm(prop.coach.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis_sqrt)
performance::check_model(money_lm_sqrt)
performance::check_model(coaching_lm_sqrt)

# Heteroscedasticity for the square-root transformed money data looks very similar to the non-transformed data, and it also 
# looks similar for the coaching data. However, I do not think sqrt transformations are ideal for proportion data.

# Finally, I wanted to try a ln transformation on my data. Seeing as I'm using proportion data that includes 0 and 1 in some responses, 
# I've decided to to do a log(1+x) transformation, which I can do using the log1p() command in tidyverse. Again, I am not
# plotting any summary tables or anything, I just want to see if these transformations can help improve the homoscedasticity 
# of my response variables.

WLdata_cis_log <- (WLdata_cis
    %>% mutate(prop.money.to.winner = log1p(prop.money.to.winner))
    %>% mutate(prop.coach.to.winner = log1p(prop.coach.to.winner))
)

money_lm_log <- lm(prop.money.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis_log)
coaching_lm_log <- lm(prop.coach.to.winner ~ participant.gender + comp.context + char.age + winner.name, data = WLdata_cis_log)
performance::check_model(money_lm_log)
performance::check_model(coaching_lm_log)

# seeing as none of these transformations really improved the homoscedasticity of my data, I'm just going to stick with my 
# initial linear models ('money_lm' and 'coaching_lm') which contained the untransformed response variables. I will compute 
# inferential plots that have confidence intervals for these two response variables. I'm going to compare my two response
# variables to a mean of 0.5 (my nulls), but I've also added two other hashmarks at ±0.05 of the null (0.45 and 0.55) to indicate what I would
# consider a substantial effect.

library(emmeans)
library(ggplot2)

emmean_money <- emmeans(money_lm, specs = "comp.context")
plot(emmean_money) + geom_vline(xintercept = 0.5, lty = 2) + geom_vline(xintercept = c(0.45, 0.55), lty = 3) + xlim(0.3,0.75) + 
  labs(x = "Mean proportion money allocated to winner",  y = "Competitive context")

# This inferential plot shows that the confidence intervals for the two levels of my first response variable (money allocated to winner)
# do not overlap 0.5, indicating that there is significantly more allocation of money to winners in both types of competitive contexts
# (academic and athletic). Also, they do not cross the ±0.05 threshold that I indicated I'd use as a litmus test for the 
# effect size. However, to be sure, I am going to compute effect size statistics by comparing the two levels of my first response variable
# to a mean of 0.5. 

# First I'm going to create two dataframes that I can use to extract the values I want for the cohen's d calculations.

WLdata_athletic <- (WLdata
    %>% filter(grepl("Athletic", comp.context))
)
WLdata_academic <- (WLdata
                    %>% filter(grepl("Academic", comp.context))
)

lsr::cohensD(WLdata_athletic$prop.money.to.winner, mu = 0.5)

lsr::cohensD(WLdata_academic$prop.money.to.winner, mu = 0.5)

# Both of these effect sizes are fairly large and positive (around 0.8), indicating that there is a fairly strong bias to allocate 
# more money to winners in athletic and academic contexts. 

# I am also going to create the same emmeans plot and calculate the effect sizes for the amount of coaching hours allocated to the winner.

emmean_coaching <- emmeans(coaching_lm, specs = "comp.context")

plot(emmean_coaching) + geom_vline(xintercept = 0.5, lty = 2) + geom_vline(xintercept = c(0.45, 0.55), lty = 3) + xlim(0.3, 0.75) +
  labs(x = "Mean proportion of coaching hours allocated to winner", y = "Competitive context")

# These confidence intervals indicate that there is a significant but small effect for allocating more coaching hours 
# to winners in the athletic context. However, this does not cross our ±0.05 threshold (i.e., it isn't over 0.55).
# Conversely, the data suggest a clear and slightly stronger tendency to award fewer coaching hours to winners 
# in an academic context. I have computed effect sizes on these two means below.

lsr::cohensD(WLdata_athletic$prop.coach.to.winner, mu = 0.5)

# This is a very weak positive effect.

lsr::cohensD(WLdata_academic$prop.coach.to.winner, mu = 0.5)

# This is a medium to strong negative effect. The data suggest that we may be biased to giving fewer coaching hours 
# to winners in an academic context. Albeit the confidence interval does cross 0.45.

# PERMUTAITON TEST ATTEMPT (ONE-SAMPLE PERMUTATION TEST USING "ENVSTATS" PACKAGE)
# link to method: https://search.r-project.org/CRAN/refmans/EnvStats/html/oneSamplePermutationTest.html

# One other thing I wanted to try was a one-sample permutation test on both of my response variables. Seeing as 
# my response variables do not meet many of the assumptions, I thought it might be useful to use a permutation 
# test, which doesn't rely on many of the assumptions. I do not know if this method is actually correct, and next week
# or for my final project I'm going to try implementing the method that I spoke about with Dr. Bolker, which is using 
# a logistic regression. The models I constructed in this assignment are obviously not ideal because my data violate many of 
# of the important assumptions, so I think in the coming weeks I will focus on constructing a model that better fits my data.

# For the permuation test, my null hypotheses are that funds allocated to winners will equal 0.5, and coaching hours 
# allocated to winners will equal 0.5.

# One-sample permutation test on money to winner being different from 0.5.

perm.test.money.athletic <- EnvStats::oneSamplePermutationTest(WLdata_athletic$prop.money.to.winner, 
      alternative = "two.sided",
      mu = 0.5,
      exact = FALSE,
      n.permutations = 100000
)
 
print(perm.test.money.athletic)
plot(perm.test.money.athletic)

perm.test.money.academic <- EnvStats::oneSamplePermutationTest(WLdata_academic$prop.money.to.winner, 
     alternative = "two.sided",
     mu = 0.5,
     exact = FALSE,
     n.permutations = 100000
)

print(perm.test.money.academic)
plot(perm.test.money.academic)

# One-sample permutation test on coaching hours to winner being different from 0.5.

perm.test.coach.athletic <- EnvStats::oneSamplePermutationTest(WLdata_athletic$prop.coach.to.winner, 
     alternative = "two.sided",
     mu = 0.5,
     exact = FALSE,
     n.permutations = 100000
)

print(perm.test.coach.athletic)
plot(perm.test.coach.athletic)

perm.test.coach.academic <- EnvStats::oneSamplePermutationTest(WLdata_academic$prop.coach.to.winner, 
     alternative = "two.sided",
     mu = 0.5,
     exact = FALSE,
     n.permutations = 100000
)

print(perm.test.coach.academic)
plot(perm.test.coach.academic)

# The permutation tests indicate that the means of my response variables (money allocated and coaching hours allocated) are 
# clearly different from 0.5 (with the exception of proportion of coaching allocated to winners in the athletic
# context). However, the confidence intervals and effect sizes I computed above are more informative, but 
# it's important to note that the models they arose from violate several important assumptions, 
# while the permutation test (I think) avoids such assumptions.
