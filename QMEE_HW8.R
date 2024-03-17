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

# ^ creating model formula, not doing any transformations just to keep things straightforward

get_prior(form1, WLdata_long)

# ^ checking which priors need to be set

b_prior <- c(set_prior("normal(0.5,0.02)", "Intercept"),
             set_prior("normal(0, 0.05)", "b"),
             set_prior("normal(0.05, 0.01", "sigma"), 
             set_prior("normal(0.05, 0.01", "sd")
)             

# ^ setting priors.

# I'm assuming that there is a prior distribution that people don't 
# have any bias in giving more or less money to winners. I'm also 
# assuming that there isn't a ton of variation in these preferences. 

make_stancode(prop.allocated ~ resource + participant.gender + (1|participant.id), 
              data = WLdata_long, prior = b_prior)

# ^ checking that the priors made it into the code (they did)

test_prior <- function(p) {
  capture.output(
    b <- brm(form1, WLdata_long, prior = b_prior,
             seed = 15,             
             sample_prior = 'only',  
             chains = 1, iter = 20, # VERY light run
             silent = 2, refresh = 0)
  )
  p_df <- WLdata_long |> add_predicted_draws(b)
  gg1 <- ggplot(p_df, aes(resource, .prediction)) + geom_jitter() + ylim(0,1)
  print(gg1)
  invisible(b)
}

test_prior(b_prior)

# Our prior plot looks pretty decent, it is a bit wider than I would have 
# anticipated, and I've 

