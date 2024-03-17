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

View(WLdata_long)

form1 <- (prop.allocated ~ resource + comp.context + participant.gender + (1|participant.id))

# ^ creating model formula, not doing any transformations just to keep things straightforward

get_prior(form1, WLdata_long)

# ^ checking which priors need to be set

b_prior <- c(set_prior("normal(0.5,0.02)", "Intercept"), # intercept at 0.5, small sd
             set_prior("normal(0, 0.05)", "b"), # no effect of resource type
             set_prior("normal(0.01, 0.005", "sigma"), # small resid st dev 
             set_prior("normal(0.01, 0.005", "sd") # small random effect sd
)             

# ^ setting priors. Note that because my response is on the scale 
# of 0 to 1, my priors are tiny. Another way to model this would be 
# to beta transform my response and use a logit link function, as I have 
# done with the GLMs on my prior assignments and which I'll be using 
# for my final project. I tried to use bayes with a beta distributed response
# and a logit link function for a while, but I ultimately couldn't figure it 
# out after several hours and many visits to the stan forums, so I 
# came back to these priors instead.

# With these priors, I'm beginning with the assumption that people don't 
# have any bias in giving more or less money to winners. I'm also 
# assuming that there isn't a ton of variation in these preferences. 

make_stancode(prop.allocated ~ resource + participant.gender + (1|participant.id), 
              data = WLdata_long, prior = b_prior)

# ^ checking that the priors made it into my stan code (they did)

test_prior <- function(p) {
  capture.output(
    b <- brm(form1, WLdata_long, prior = b_prior,
             seed = 15,             
             sample_prior = 'only',  
             chains = 1, iter = 100, # VERY light run
             silent = 2, refresh = 0)
  )
  p_df <- WLdata_long |> add_predicted_draws(b)
  gg1 <- ggplot(p_df, aes(resource, .prediction)) + geom_jitter() + ylim(0,1) + labs(x = "Resource allocated", 
  y = "Proportion of resource allocated to winner (simulated prediction)")
  print(gg1)
  invisible(b)
}

test_prior(b_prior)

# My prior plot looks pretty decent: it shows that I am beginning with the 
# assumption that there won't really be any difference in people's
# preferences to allocate more or less money or coaching hours to winners or losers
# (i.e., clustering is pretty tight around 0.5).

# Now to fit a model using my priors and actual observed data

mod1 <- lmer(form1, WLdata_long)

## beginning with default settings to see what happens

b_default <- brm(form1, WLdata_long, seed = 15)

# it seems very unhappy about the defaults, so I'm going to customize things a bit

b_custom1 <- brm(form1, WLdata_long, prior = b_prior, seed = 15, 
                 control = list(adapt_delta = 0.95)
                 )

# it still seems very unhappy, even with the updated prior, so I'm not 
# quite sure how to fix this.

print(bayestestR::diagnostic_posterior(b_custom1),
      digits = 4)

# the values here all look good to me, but I'm not sure about the tiny
# MCSE values. They do look very small, but I'm not sure if this suggests
# that something is wrong with my model, I.e., deflating these values.

mcmc_trace(b_custom1, regex_pars = "b_|sd_")

# The trace plots look good, but it also looks like my priors about there being generally
# no bias towards rewarding winners with more or less money and coaching may have been
# incorrect. 

# the intercept is slightly lower (on average) than 0.5, indicating that for 
# coaching hours people might be willing to give fewer coaching hours to winners.

# For the main effect of money, it's not 0 as I had predicted in my priors, but is instead
# around 0.17 or so, indicating that people might be biased to giving more money 
# to winners than coaching hours to winners. 

# The plots are a bit "spiky", which could be due to potential skewness
# in my response variables, but overall they don't look too bad.





# old prior code:

b_prior <- c(set_prior("normal(0.5,0.02)", "Intercept"),
             set_prior("normal(0, 0.05)", "b"),
             set_prior("normal(0.05, 0.01", "sigma"), 
             set_prior("normal(0.05, 0.01", "sd")
)             
