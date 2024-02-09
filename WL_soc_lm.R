# I would like to create a linear model that looks at whether my response 
# variables are different from 0.5. So I guess I can just construct a linear
# model with my response variables, and then construct a plot or confidence intervales
# that compare them to 0.5.
library(tidyverse)
library(performance)
WLdata <- read.csv("WL_soc_clean.csv")
WLdata_cis <- (WLdata
               %>% filter(grepl("cisgender", participant.gender))
)
money_lm <- lm(prop.money.to.winner ~ comp.context*participant.gender*char.age*winner.name, data = WLdata_cis)
check_model(money_lm)

coaching_lm <- lm(prop.coach.to.winner ~ comp.context*participant.gender*char.age*winner.name, data = WLdata_cis)
check_model(coaching_lm)

view(WLdata)

WLdata1 <- (WLdata
    %>% select(-scenario.number)
)

view(WLdata1)

summary(WLdata1)

write.csv(WLdata1, "WL_soc_clean.csv")
