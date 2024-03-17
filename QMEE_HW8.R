# QMEE Homework 8

library(lme4)
library(brms)
options(brms.backend = "cmdstanr")
library(broom.mixed)
library(purrr)
library(dplyr)
library(tidybayes)
library(bayesplot)
library(bayestestR)
library(ggplot2); theme_set(theme_bw())
library(see)
options(ggplot2.discrete.colour= scale_color_okabeito)
library(ggrastr)
library(cowplot)

WLdata_long <- readRDS("WLdata_long.rds")

#View(WLdata_long)

form1 <- (prop.allocated ~ resource + comp.context + participant.gender + (1|participant.id))

# I am keeping the model simple so I don't need to worry too much about interaction 
# terms.

get_prior(form1, WLdata_long, family = Beta(link = "logit"))

b_prior <- c(set_prior("normal(0,0.1)", "Intercept"),
             set_prior("normal(0, 0.05)", "b"),
             set_prior("normal(0.1, 0.1", "sd")
)

test_prior <- function(p) {
  capture.output(
    b <- brm(form1, WLdata_long, prior = b_prior, family = Beta(link = "logit"),
             seed = 15,             
             sample_prior = 'only',  
             chains = 1, iter = 100,
             silent = 2, refresh = 0)
  )
  p_df <- WLdata_long |> add_predicted_draws(b)
  gg1 <- ggplot(p_df, aes(resource, (log((.prediction)/(1-.prediction))))) + geom_jitter() + ylim(-1,1)
  print(gg1)
  invisible(b)
}

test_prior(b_prior)

# I have no clue why, but the intercept and slope to "reasonable values" (what might be expected in my
# actual data) doesn't seem to be doing anything for my plot. Although my response variable in the actual
# data is constrained between 0 and 1, and I'm telling R that I want an intercept at 0.5 with an sd of 0.1. This 
# is a prior of (essentially) no bias to reward to winners over losers. I also set a prior for effect of resource of 0, 
# or that there shouldn't be any difference between money allocated to winners and coaching hours allocated to winners. 
# However, my function is giving me an output figure with predicted values that are between nearly 
# -50 and +50. I have no clue why this is happening. The intercept is changing, but my data is 
# not constrained at all to between 0 and 1, and I don't know why. 

# For the prior of intercept, I also tried setting a mandatory upper bound of 1 and a 

make_stancode(prop.allocated ~ resource + participant.gender + (1|participant.id), 
              data = WLdata_long, prior = b_prior, family = Beta(link = "logit"))

# ^ checking that the priors are in the code, which they seem to be.

test_prior <- function(p) {
  capture.output(
    b <- brm(form1, WLdata_long, prior = b_prior, family = Beta(link = "logit"),
             seed = 5318008,             
             sample_prior = 'only',  
             chains = 1, iter = 100,
             silent = 2, refresh = 0)
)
  p_df <- WLdata_long |> add_predicted_draws(b)
  gg1 <- ggplot(p_df, aes(resource, .prediction)) + geom_line(alpha = 0.1) + geom_jitter()
  print(gg1)
  invisible(b)
}

test_prior(b_prior)








# Since this figure has a predicted response that stretches too high and low on the y-axis, when it should
# be from near -1 to +1), I have converted the response variable to a non-proportion measure to see if it .
# fixes anything. The response variable are now the raw non-proportion values from the actual study ($0-$10,000). 
# I.e., 0.1 become $1000, 0.2 becomes $2000, and so on.

WLdata_longx10k <- (WLdata_long
  %>% mutate(prop.allocated = prop.allocated*10000)
)

# I am now going to try the model again.

form2 <- prop.allocated ~ 1 + resource + participant.gender + (1|participant.id)

get_prior(form2, WLdata_longx10k)

b_prior2 <- c(set_prior("normal(5000,50)", "Intercept"),
             set_prior("normal(0, 50)", "b"))

test_prior <- function(p) {
  capture.output(
    b <- brm(form2, WLdata_longx10k, prior = b_prior2,
             seed = 50,             
             sample_prior = 'only',  
             chains = 1, iter = 100,
             silent = 2, refresh = 0)
  )
  p_df <- WLdata_longx10k |> add_predicted_draws(b)
  gg1 <- ggplot(p_df, aes(resource, .prediction)) + geom_line(alpha = 0.1) + geom_jitter()
  print(gg1)
  invisible(b)
}

test_prior(b_prior2)



?add_predicted_draws
