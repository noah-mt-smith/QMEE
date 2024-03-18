# QMEE Homework 8

library(lme4)
library(brms)
options(brms.backend = "cmdstanr")
library(broom.mixed)
library(broom)
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
library(Cairo)

WLdata_long <- readRDS("WLdata_long.rds")

# View(WLdata_long)

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

# Now using the function that Ben showed in lecture on Thursday test the prior I made.

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

## beginning with default settings to see what happens (commented out
# because just gave a bunch of divergence warnings).

# b_default <- brm(form1, WLdata_long, seed = 15)

# it seems very unhappy about the defaults, so I'm going to customize things a bit using my 
# own previously set prior.

b_custom1 <- brm(form1, WLdata_long, prior = b_prior, seed = 15, 
                 control = list(adapt_delta = 0.95)
                 )

# it still seems very unhappy, even with the updated prior, so I'm not 
# quite sure how to fix this. I think this might have to do with the small size 
# of my priors, but I'm not exactly sure.

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

# Now to look at the actual results of my model

summary(b_custom1)

# Overall, the summary table echoes what I see in my trace plots. 
# The intercept value (coaching hours) is about 0.47, so slightly 
# below my prior assumption of 0.5. For money, the estimate is about 0.17 with 
# a credible interval from 0.16 to 0.19, indicating that my participants
# were giving around 0.16 to 0.19 more money to winners than coaching hours
# to winners.

# now to plot the results from my summary table to get a better sense
# of what they look like.

brms_bayes <- (list(brms_custom1 = b_custom1))

res_bayes_mod1 <- (brms_bayes
              |> purrr::map_dfr(~ tidy(., conf.int = TRUE), .id = "model")
)

res_mod1 <- suppressMessages(mod1
                             |> tidy(conf.int = TRUE, conf.method = "profile")
                             |> mutate(model = "lmer", .before = 1)
)

res <- (bind_rows(res_bayes_mod1, res_mod1)
        |> select(-c(std.error, statistic, component, group))
        |> filter(term != "(Intercept)")
        |> mutate(facet = ifelse(grepl("^cor", term), "cor",
                                 ifelse(grepl("resource", term), "resource",
                                        "int")))
        |> mutate(across(model, ~ factor(., levels = c("lmer", names(brms_bayes)))))
)

ggplot(res, aes(estimate, term, colour = model, shape = model)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high))

# The scale of these results is a little bit confusing, but is does
# fairly closely match my results summary table that I produced with my 
# above brms code. The intercept estimate for coaching hours is missing, 
# but if we comment out the "|> filer(term != "(Intercept)")" code, then
# we can see that the intercept (coaching) estimate is about 0.48 for both models. 
# However, the way this figure is laid out, interpreting the result is a bit confusing
# but this is due to how I've constructed the model. It's important to note 
# that the "b" estimate (population level effect of money) is actually 
# being "added" to the baseline intercept value of 0.5 (so people give about 
# 0.16 to 0.19 than 0.5 money to winners (i.e. 0.66 to 0.69 ish), while the intercept 
# (coaching) estimate of 0.48 is actually showing the true value--that 
# people give about 0.48 of the coaching hours to winners).

# We can also do some posterior predictive simulations of my data

posterior_df <- WLdata_long |> add_predicted_draws(b_custom1)

gg2 <- ggplot(posterior_df, aes(resource, .prediction, group = interaction(participant.id, .draw))) +
  geom_jitter() + labs(y = "Proportion allocated to winner") + geom_point(aes(y = prop.allocated), col = "skyblue")

print(gg2)

# This shows the posterior predicted distributions, however they are slightly odd because they are so wide
# and include values above and below zero. I have a feeling they are so wide because the distribution of observed
# data (the blue dots) is quite wide, and when combined with the prior distribution generates a much wider 
# posterior, which unfortunately overlaps 1 and 0 (which is impossible). This may have been 
# mitigated if I'd used a beta distributed response, but as I mentioned earlier, I tried that approach 
# for a good amount of time and couldn't get past properly setting the priors.

gg_violin <- ggplot(WLdata_long, aes(x = resource, y = prop.allocated)) + geom_violin()
print(gg_violin)

# gg3 <- ggplot(posterior_df, aes(resource, .prediction, group = interaction(participant.id, .draw))) 
# + geom_jitter() + labs(y = "Proportion allocated to winner") 
# + geom_violin(aes(x = resource, y = prop.allocated), col = "skyblue")


# print(gg3)

# ^ Also, I attempted to overlay a violin plot of the real data instead of a point plot, as this would allow
# me to visualize the true frequency of the responses at each level of the predictor, and 
# therefore would give better information and a good idea of why the two posteriors look different.
# With geom_point, it only shows me the levels of the response where there are responses. 
# This way, the two response distributions for coaching hours and money look fairly similar, which isn't the case at 
# all (see "gg_violin"). Although I tried to overlay the violin plot, it still wouldn't work even after
# a couple hours worth of (attempted) debugging. I'm not sure if this has something to do with the stan
# package or the way that the code puts the plot together, but I would've much preferred such a plot. 
