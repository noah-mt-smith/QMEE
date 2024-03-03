# Noah Smith QMEE HW7

library(glmmTMB)
library(dplyr)
library(DHARMa)
library(emmeans)
library(ggplot2); theme_set(theme_linedraw())

WLdata <- read.csv("WL_soc_clean.csv")

# for the sake of power, I have excluded responses from individuals who reported
# in one of the non-cisgender levels of the "gender" predictor. As "nonbinary" 
# and "transgender man" included only one response each, and prefer not to answer
# included 3.

WLdata_cis <- (WLdata
               %>% filter(grepl("cisgender", participant.gender))
)

# for some reason R has added an "X" column as my first column, even though it wasn't
# there before, so I'm just removing it.

# just changing characters to factors.

WLdata_cis <- (WLdata_cis
           %>% mutate(across(where(is.character), as.factor))
           %>% mutate(across(participant.ID, as.factor))
)

summary(WLdata_cis)

# now to create the GLMs (or try to)
# Since my response variables are proportion data, I'm going to use a beta distributed
# GLM with a log odds (logit) link function. I can do this with the glmmTMB package. I 
# am also doing this after having spoken to ben after lecture and following 
# some advice he provided as feedback on a previous assignment. I also know that 
# there is potentially a better approach to modelling my data, which I will 
# iron out in the final project. Anyways, now to start building the models for my 
# two response variables.

money.to.winner <- WLdata_cis$prop.money.to.winner

# glmmTMB(money.to.winner ~ participant.gender
#        + comp.context + char.age + winner.name, 
#        data = WLdata_cis, family = beta_family(link="logit"))

summary(WLdata_cis$prop.money.to.winner)
summary(WLdata_cis$prop.coach.to.winner)

# I believe I'm getting an error in the above glmmTMB code because one of my response 
# variables (in particular, the "proportion of money allocated to winners)
# includes 1s, and for this model, the response variable cannot include 0 or 1.
# I have a feeling this is just because of how the math of the model works. 
# Since I'm trying to take the natural logarithm of the odds of my response, 
# any response of "1" would have an odds of infinity, and the natural log of infinity
# is infinity, so the model cannot compute that (at least this is what I think is going on).
# So I've transformed the 1s in my money response variable to 0.999 to see if it
# works. I'm not sure what other solutions there are to this 

# WLdata_cis <- (WLdata_cis 
#               %>% mutate(prop.money.to.winner = ifelse(prop.money.to.winner
#               == 1, 0.999, prop.money.to.winner))
# )

# I initially had the above code, but it made my model nearly 
# uninterpretable, because there were huge outliers in my data for 
# the responses that were 0.999. To simplify my model, I thus 
# transformed all of the values that were 1 to 0.9. Since only 
# 11 out of the 164 responses were 1, the mean of my first 
# response variable ("money allocated to winner") decreased by
# only 0.004, which is quite negligible. Furthermore, this makes
# the resulting model much cleaner, while changing the underlying data negligibly.

WLdata_cis <- (WLdata_cis 
                   %>% mutate(prop.money.to.winner = ifelse(prop.money.to.winner
                   == 1, 0.9, prop.money.to.winner))
)

money.to.winner <- WLdata_cis$prop.money.to.winner
coaching.to.winner <- WLdata_cis$prop.coach.to.winner

BetaLogMoney <- glmmTMB(money.to.winner ~ participant.gender
                + comp.context + char.age + winner.name, 
                data = WLdata_cis, family = beta_family(link="logit"))

BetaLogCoaching <- glmmTMB(coaching.to.winner ~ participant.gender
                   + comp.context + char.age + winner.name, 
                   data = WLdata_cis, family = beta_family(link="logit"))

summary(BetaLogMoney)
summary(BetaLogCoaching)

# Now that I've constructed my models, I would like to check them with DHARMa.

DHARMa::testDispersion(BetaLogMoney, plot = "F")
DHARMa::testDispersion(BetaLogCoaching, plot = "F")

# The dispersion values for both of my models are around 1, which 
# looks fairly good, as in class we discussed that it should sit around 1.
# I set plot = "F" because the dispersion plots are plotted below.

DHARMa::testResiduals(BetaLogMoney, plot = T)

# Based on the QQ plot for my "money allocated to winners" data, 
# my resitudals deviate slightly from normality, but I don't think it
# deviates enough to be of too much concern (not too mention that the K.S. test is
# not significant, but again this isn't something we should care about too much)
# . I think some of the weird shape of the curve may come from the fact that my
# data exists only in tenths(i.e. 0.1, 0.2, etc.), and is not truly continuous.

# Similarly, the dispersion plot (which is a graphical depiction of 
# the testdispersion() code above), again shows that the dispersion 
# is not too bad in my model, and neither are the outliers.

DHARMa::testResiduals(BetaLogCoaching, plot = T)

# The QQ plot here looks quite good, and more importantly the dispersion and outlier plots
# also look okay as well (especially the dispersion plot). The dispersion plot is again 
# a graphical depiction of the dispersion value generated above.

# Finally, I would just like to recreate some of the inferential plots
# that I made in assignment 5, but using the new GLM that I've created here. 
# To do this, I'm going to use emmeans. 

emmean_money <- emmeans(BetaLogMoney, specs = c("participant.gender", "comp.context"))
summary(emmean_money)
plot(emmean_money) + xlim(-1,1) + geom_vline(xintercept = 0, lty = 3) +  
  labs(x = "Log odds of proportion of money allocated to winners", 
  y = "Participant gender and competitive context")

emmean_coaching <- emmeans(BetaLogCoaching, specs = c("participant.gender", "comp.context"))
summary(emmean_coaching)
plot(emmean_coaching) + xlim(-0.5,0.5) + geom_vline(xintercept = 0, lty = 3) + 
  labs(x = "Log odds of proportion of coaching hours allocated to winners", 
  y = "Participant gender and competitive context")

# As the summary tables mention, these plots include the log odds of my response variables, 
# not the response variables themselves. This means that if any of my confidence intervals
# overlap 0, it suggests that there is no bias for allocating funds
# to winners (since in my raw response variable, a probability of 0.5 (odds of 1) would indicate no bias,
# the log odds scale equivalent of that is ln(1) or 0).



