# bivariate model attempt

library(tidyverse)
library(ggplot2);theme_set(theme_linedraw())
library(DHARMa)
library(glmmTMB)
library(forcats)
library(emmeans)

WLdata <- readRDS("WLdata.rds")

WLdata <- (WLdata 
      %>% mutate(prop.money.to.winner = ifelse(prop.money.to.winner
          == 1, 0.9, prop.money.to.winner))
)

# I transformed all 1 values to 0.9 so that I can carry out the beta logit GLMM below. This 
# will slightly deflate my estimate of the bias for rewarding winners. Since I 
# transformed 11 out of 164 responses from 1 to 0.9, I reduced the overall 
# mean proportion of money allocated to winners by 0.004. There were no 0 values
# that required transforming. Coaching hours was not affected because no
# participants gave none or all of their resources to winners.

ggplot(WLdata, aes(x = comp.context, y = prop.money.to.winner, fill = participant.gender)) + geom_boxplot()
money.to.winner <- WLdata$prop.money.to.winner
coaching.to.winner <- WLdata$prop.coach.to.winner

# old models

GLM_Money <- glmmTMB(money.to.winner ~ participant.gender
                     + comp.context + char.age + winner.name, 
                     data = WLdata, family = beta_family(link="logit"))

GLM_Coaching <- glmmTMB(coaching.to.winner ~ participant.gender
                        + comp.context + char.age + winner.name, 
                        data = WLdata, family = beta_family(link="logit"))

# need to reshape data to put "money" and "coaching" as two levels of a new fixed factor "resource".
# "Proportion allocated" will become the new response variable. 

WLdata_long <- (WLdata
    %>% pivot_longer(cols = starts_with("prop"),
                     names_to = "resource.allocated",
                     values_to = "proportion.allocated")
)

# Data reformatted, now just going to do some cleaning and renaming for the sake of 
# simplicity.

WLdata_long <- (WLdata_long
  %>% select(-scenario.number)
)

WLdata_long <- (WLdata_long
      %>% rename(resource = resource.allocated)
      %>% rename(prop.allocated = proportion.allocated)
      %>% rename(participant.id = participant.ID)
)

WLdata_long <- (WLdata_long
    %>% mutate(resource = fct_recode(resource,
        "money" = "prop.money.to.winner",
        "coaching" = "prop.coach.to.winner"))
    %>% mutate(comp.context = fct_recode(comp.context,
        "athletic" = "Athletic",
        "academic" = "Academic"))
)

# plotting data to view any trends

ggplot(WLdata_long, aes(x = comp.context, y = prop.allocated, fill = resource)) + geom_boxplot() + labs(x = "Competitive context", y = "Proportion of resource allocated", fill = "Resource type") + ylim(0,1)

GLMM_allocation <- glmmTMB(prop.allocated ~ resource*comp.context + participant.gender
                     + char.age + winner.name + participant.age + (1|participant.id), 
                     data = WLdata_long, family = beta_family(link="logit"))

summary(GLMM_allocation)

DHARMa::testDispersion(GLMM_allocation, plot = "F")

DHARMa::testResiduals(GLMM_allocation, plot = T)

testQuantiles(GLMM_allocation)

# all diagnostic plots look pretty good. Will explain in more detail for final

emmean_allocation <- emmeans(GLMM_allocation, specs = c("participant.gender", "resource", "comp.context"))
summary(emmean_allocation)
plot(emmean_allocation) + xlim(-1,1.1) + geom_vline(xintercept = 0, lty = 3) +  
  labs(x = "Log odds of proportion of resource allocated to winners", y = "")

