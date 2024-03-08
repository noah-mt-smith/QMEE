# Noah Smith QMEE HW7

library(glmmTMB)
library(dplyr)
library(DHARMa)
library(emmeans)
library(ggplot2); theme_set(theme_linedraw())

WLdata <- read.csv("WL_soc_clean.csv")

# for the sake of power and reliability, I have excluded responses from the 6 out of 170 individuals who reported
# in one of the non-cisgender levels of the "gender" predictor. "Nonbinary" included 2 responses,
# "transgender man" included one response, and prefer not to answer included 3.

WLdata_cis <- (WLdata
               %>% filter(grepl("cisgender", participant.gender))
)

# just changing characters to factors to avoid any issues with analysis.

WLdata_cis <- (WLdata_cis
           %>% mutate(across(where(is.character), as.factor))
           %>% mutate(across(participant.ID, as.factor))
)

summary(WLdata_cis) # checking that everything looks good with data

## JD: Better to put the above in a separate script that saves an .rda file

# now to create the GLMs

# Since my response variables are proportion data, I'm going to use a beta distributed
# GLM with a log odds (logit) link function. I can do this with the glmmTMB package. I 
# am also doing this after having spoken to Ben after lecture and following 
# some advice he provided as feedback on a previous assignment. Now to start building the models for my 
# two response variables. I would like to start with the model for my first 
# response variable, "proportion of money allocated to winner".

money.to.winner <- WLdata_cis$prop.money.to.winner

# glmmTMB(money.to.winner ~ participant.gender
#        + comp.context + char.age + winner.name, 
#        data = WLdata_cis, family = beta_family(link="logit"))

# I believe I'm getting an error in the above glmmTMB code because one of my response 
# variables (in particular, the "proportion of money allocated to winners)
# includes 1s.

summary(WLdata_cis$prop.money.to.winner) # includes 1s, but not 0s
summary(WLdata_cis$prop.coach.to.winner) # includes no 1s or 0s

# The above GLM was not working since the response variable cannot include 0 or 1.
# I believe this is just because of how the math of the model works: 
# Since I'm trying to take the natural logarithm of the odds of my response, 
# any response of "1" would have an odds of infinity, and the natural log of infinity
# is infinity, so the model cannot compute that.

# So I've transformed the 1s in my money response variable to 0.999 to see if it
# works (WARNING: I realized this is very wrong, and see below for my explanation
# of the problem with this approach).

# WLdata_cis <- (WLdata_cis 
#               %>% mutate(prop.money.to.winner = ifelse(prop.money.to.winner
#               == 1, 0.999, prop.money.to.winner))
# )

# I initially had the above code, but it made my model nearly 
# uninterpretable. Although the substitution of 1 for 0.999 made my 
# model run, since it no longer included 1, the value of 0.999 generated huge 
# outliers in my final model. This is because of how log odds works. 
# The maximum odds of my response variable before the transformation (if we exclude 1) is
# 0.9/0.1 = 9, and log odds = ln(9) ≈ 2.2. 

# However, when substituting 0.999 for 1 , I inadvertently made the 
# maximum odds of my response variable 0.999/0.001 = 999, and log odds = ln(999) ≈ 6.9. 
# There were 11 such outliers in my "money" response variable, since 11 individuals
# chose to allocate their entire proportion of funds to the winner.

# Thus instead of transforming my 1 values to 0.999, I transformed
# all of the values that were 1 to 0.9. Since only 
# 11 out of the 164 responses were 1, the mean of my first 
# response variable ("money allocated to winner") decreased by
# only 0.004, which is quite negligible. This makes
# the resulting model much cleaner (since the transformed values will not 
# generate outliers) while changing the underlying data negligibly.
# Also, only the "money" response variable included 1s, while the "coaching" 
# response variable included no 1s or 0s, so the model for my response of 
# "coaching hours to winner" (which is below) didn't need to be changed at all.

WLdata_cis <- (WLdata_cis 
                   %>% mutate(prop.money.to.winner = ifelse(prop.money.to.winner
                   == 1, 0.9, prop.money.to.winner))
)

money.to.winner <- WLdata_cis$prop.money.to.winner
coaching.to.winner <- WLdata_cis$prop.coach.to.winner

GLM_Money <- glmmTMB(money.to.winner ~ participant.gender
                + comp.context + char.age + winner.name, 
                data = WLdata_cis, family = beta_family(link="logit"))

GLM_Coaching <- glmmTMB(coaching.to.winner ~ participant.gender
                   + comp.context + char.age + winner.name, 
                   data = WLdata_cis, family = beta_family(link="logit"))

# Now that I've constructed my models, I would like to check how they fit my 
# data using DHARMa.

DHARMa::testDispersion(GLM_Money, plot = "F")
DHARMa::testDispersion(GLM_Coaching, plot = "F")

# The dispersion values for both of my models are around 1, which 
# looks fairly good, as in class we discussed that it should sit around 1.
# I set plot = "F" because the dispersion plots are plotted below.

DHARMa::testResiduals(GLM_Money, plot = T)

# The dispersion plot (which is a graphical depiction of 
# the "testDispersion()" code above), shows that the dispersion 
# is good in my model, and that there are no outliers to be too
# conerned about. So right now it seems like my model fits my data fairly well.

# Also, based on the QQ plot,my residuals deviate slightly from normality, 
# but I don't think it deviates enough to be of too much concern. I think 
# some of the weird shape of the curve may come from the fact that my data 
# exists only in tenths (i.e. 0.1, 0.2, etc.), and so is not truly continuous.

DHARMa::testResiduals(GLM_Coaching, plot = T)

# For my "coaching" response variable, the QQ plot looks quite good, and more importantly
# the dispersion and outlier plots also look okay as well (especially the dispersion plot). 
# The dispersion plot is again a graphical depiction of the dispersion value generated by the
# "testDispersion()" code above.

# Another good thing to do would be testing the predicted vs. observed
# residuals of my model at each quantile level. 

testQuantiles(GLM_Money)
testQuantiles(GLM_Coaching)

# Based on these two diagnostic plots, it seems like my models
# fit my data fairly well. For the money plot, the fit
# is extremely close to the predicted fit. For the 
# coaching plot, the fit is fairly close, except for the 
# top quantile (0.75), which looks a bit wonky. However, 
# this deviation likely isn't enough to cause any concern, 
# especially given the good dispersion numbers for both response 
# variables.

# Finally, I would like to use emmeans to recreate some of the inferential plots
# that I made in assignment 5, but using the new GLM that I've created here.

emmean_money <- emmeans(GLM_Money, specs = c("participant.gender", "comp.context"))
summary(emmean_money)
plot(emmean_money) + xlim(-1,1) + geom_vline(xintercept = 0, lty = 3) +  
  labs(x = "Log odds of proportion of money allocated to winners", 
  y = "Participant gender and competitive context")

emmean_coaching <- emmeans(GLM_Coaching, specs = c("participant.gender", "comp.context"))
summary(emmean_coaching)
plot(emmean_coaching) + xlim(-0.5,0.5) + geom_vline(xintercept = 0, lty = 3) + 
  labs(x = "Log odds of proportion of coaching hours allocated to winners", 
  y = "Participant gender and competitive context")

# As the summary tables mention, these plots include the log odds of my response variables, 
# not the response variables themselves. This means that if any of my confidence intervals
# overlap 0, it suggests that there is no bias for allocating funds
# to winners (since in my raw response variable, a probability of 0.5 (odds of 1) would indicate no bias,
# the log odds scale equivalent of that is ln(1) or 0). In both of these plots, we can 
# see that only one of the eight confidence intervals crosses the 0 line (which is equivalent to 0.5 in the
# raw response variable). So within my respondents, I detected a) a strong bias to allocate more money to winners than 
# to losers across genders and contexts, and b) that in terms of coaching hours, it depends on gender and the context of the competition. 
# In academics, it seems like the respondents tended to want to help those struggling, while in athletics, it seems the respondents 
# wanted to recruit and coach stronger players (at least for males, while for females I have not detected a clear effect). 

## This is a nice effort, and I'm frustrated that I don't have clear answers for you.
## If you are going to stick with this general approach, however, I would strongly recommend using a clear, symmetric, invertible transformation to move your observation away from the edge. The simplest one would be p-hat = f+(1-2f)*p. For your example, it seems like your scientific judgment would be to use f=0.1.

## Grade 2/3
