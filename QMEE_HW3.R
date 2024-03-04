# Noah Smith QMEE HW3

library(tidyverse)
library(patchwork)
library(performance)
library(see)
library(ggplot2); theme_set(theme_linedraw())
library(qqplotr)
WLdata <- read_csv("WL_soc_clean.csv")
summary(WLdata)

# Before creating various plots,  I need to change some variables in my dataframe. 

WLdata <- (WLdata
  %>% mutate(across(where(is.character), as.factor))
  %>% mutate(across(participant.ID, as.factor))
)

summary(WLdata)

#View(WLdata)

# the first thing I'd like to do is create boxplots on how gender interacts with the proportion of 
# money and coaching hours allocated to the winner and loser. Boxplots allow the audience 
# to see roughly what the sample of each boxplot is doing without simplifying the dataset too much, so 
# I think they are an appropriate choice here.

## JD: You could also try plotting points, but see below.

money.gender.boxplot <- (ggplot(WLdata, aes(x = participant.gender, y= prop.money.to.winner)) + 
  geom_boxplot(fill="gray") + 
  labs(x="Gender", y = "Proportion of funds allocated to winner") + 
  ylim(0,1)
)

## JD: Better to not just leave genders in alphabetical order

print(money.gender.boxplot)

coaching.gender.boxplot <- (ggplot(WLdata, aes(x = participant.gender, y= prop.coach.to.winner)) + 
  geom_boxplot(fill="gray") + 
  labs(x="Gender", y = "Proportion of coaching hours to winner") + 
  ylim(0,1)
)

print(coaching.gender.boxplot)

# The lack of power for the non-cisgender individuals makes those 3 plots fairly 
# uninformative, so I'm not sure having them is necessary. So, I'm going to create 
# a dataframe without those categories and plot those instead. Ideally, I would have a
# larger sample of non-cis individuals.


WLdata_cis <- (WLdata
    %>% filter(grepl("cisgender", participant.gender))
)

# ^ thank you Ben for showing me that code. Way easier than using base R.

summary(WLdata_cis)

# now I can create the same boxplots with only the cisgender individuals.

money.cisgender.boxplot <- (ggplot(WLdata_cis, aes(x = participant.gender, y= prop.money.to.winner)) + 
  geom_boxplot(fill="gray") + 
  labs(x="Gender", y = "Proportion of funds allocated to winner") + 
  ylim(0,1)
)

print(money.cisgender.boxplot)

coaching.cisgender.boxplot <- (ggplot(WLdata_cis, aes(x = participant.gender, y= prop.coach.to.winner)) + 
  geom_boxplot(fill="gray") + 
  labs(x="Gender", y = "Proportion of coaching hours allocated to winner") +
  ylim(0,1)
)

print(coaching.cisgender.boxplot)

# Although this is a good start, it is a) very similar to what I did in "HW2b.R" last week, and b) it 
# excludes an important predictor--"competitive context"--from being visualized. 
# The competitive context refers to whether participants read a scenario about a spelling bee
# (academic context) or a soccer match (athletic context). 

context.money.boxplot <- (ggplot(WLdata_cis, aes(x = comp.context, y = prop.money.to.winner)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Competitive context", y = "Proportion of funds allocated to winner") +
  ylim(0,1)
)

print(context.money.boxplot)

context.coaching.boxplot <- (ggplot(WLdata_cis, aes(x = comp.context, y = prop.coach.to.winner)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Competitive context", y = "Proportion of coaching hours allocated to winner") +
  ylim(0,1)
)

print(context.coaching.boxplot)

# Although it's useful to have these various plots, it would be more intuitive/in line with the Cleveland hierarchy 
# if we can fit the four plots on one scale, as it would make it easier to compare how gender, competitive context, and the 
# allocation of money or coaching hours to the winner interact with one another. Increasing the number of boxplots in a single 
# figure would also allow us to go from four figures to two figures, but more importantly makes the data more understandable because
# of its proximity to one another. These are descriptive plots which can show us roughly what the sample of 
# each factor and sublevel of each factor is doing.

# I also added a line to the boxplots at the point of interest, y = 0.5, because 
# we care about comparing the distribution of funds and coaching hours to that point specifically. 
# I.e., if there's no difference between funds allocated to winners and losers, 
# then boxplots should be centered around the y = 0.5 line. Although theme(linedraw)
# already includes a line at 0.5, I just wanted to make it clearer. If this line were absent, 
# viewers might be unsure about where to look on the plot.

money.cont.cis.boxplot <- (ggplot( WLdata_cis, aes(x = comp.context, y = prop.money.to.winner, fill = participant.gender)) +
  geom_boxplot() +
  labs(x="Competitive context", y = "Proportion of funds allocated to winner", fill = "Gender") +
  scale_fill_manual(values = c("blue","red"), labels = c("cis men", "cis women")) + 
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.5) +
  theme_linedraw() + 
  ylim(0,1)
)

print(money.cont.cis.boxplot)

coaching.cont.cis.boxplot <- (ggplot( WLdata_cis, aes(x = comp.context, y = prop.coach.to.winner, fill = participant.gender)) +
  geom_boxplot() +
  labs(x="Competitive context", y = "Proportion of coaching hours allocated to winner", fill = "Gender") +
  scale_fill_manual(values = c("blue","red"), labels = c("cis men", "cis women")) + 
  theme_linedraw() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.5) +
  ylim(0,1)
)

print(coaching.cont.cis.boxplot)

# It would also be interesting to see how age interacts with the proportion of money allocated to winners and losers. Since age is numeric, 
# we can create a scatter plot to see what kind of trend there is. I also initially fit a loess to it, 
# but it was returning so many errors that I figure it is not a good method to use in the case of my data.

## JD: Not clear why you prefer points in this case and boxplots in the other case. The numeric nature of age does not seem very relevant to me.
## JD: These points are suspiciously regular, and presumably plotted on top of each other. You should scale the size of the points by their "counts" if you want to do it this way.
## It seems dangerous to me not to use scale_size_area; not sure why that's not the default

age.money.scatter <- (ggplot(WLdata_cis, aes(x = participant.age, y = prop.money.to.winner)) + 
  geom_count() + scale_size_area() +
# geom_smooth() + # I had added this and it was working, but it was giving me a ton of warnings..., 
# I just decided to comment it out.
  ylim(0,1) +
  labs(x = "Participant ages", y = "Proportion of money allocated to winner")
)
print(age.money.scatter)

# there seems to be a slight upward trend for awarding money to the winner as age increases, but there are so few observations in the 
# upper age categories that they should be taken with a grain of salt.


# we can also create a scatter plot that depicts how how age interacts with the allocation of coaching hours.

age.coaching.scatter <- (ggplot(WLdata_cis, 
  aes(x = participant.age, y = prop.coach.to.winner)) + 
  geom_point() + 
# geom_smooth() + # again, this was working but it was
# giving me several warnings so I commented it out. 
  ylim(0,1) +
  labs(x = "Participant age", y = "Proportion of coaching hours allocated to winner")
)

print(age.coaching.scatter)

# The trend here looks very flat across all levels of age.

# I can also create some histograms for the perceived variation factors. These represent how
# participants perceive variation in athleticism and intelligence within the population. The third level of the factor
# "1", represents a population with a standard deviation equal to the standard normal distribution (e.g., the IQ distribution)

# for the histograms to appear nicely, I convert the numeric variables into factors, as the bins on the histogram weren't functioning
# to my liking (i.e., there was unequal spacing between the bars). I am thus using a bar plot to depict the distribution of the responses.

WLdata <- (WLdata 
  %>% mutate(athleticism.perception = as.factor(athleticism.perception))
  %>% mutate(intelligence.perception = as.factor(intelligence.perception))
)

athleticism.hist <- (ggplot(WLdata,
  aes(athleticism.perception)) + 
  geom_bar() +
  labs(x = "Perceived athletic variation in the population", 
  y = "Frequency") + 
  ylim(0,80)
)

print(athleticism.hist)

intelligence.hist <- (ggplot(WLdata,
  aes(intelligence.perception)) + 
  geom_bar() + 
  labs(x = "Perceived intellectual variation in the population", 
  y = "Frequency") + 
  ylim(0,80)
)

print(intelligence.hist)

# Although it's nice to have these two separate
# histograms, it might also be useful for us to have them closer to one another, perhaps
# one above the other, so that we can more closely see how the distributions compare. 
# So, I'm going to use the "patchwork" package to stack the two figures (alternatively), 
# I could use ggplot's "facet" feature, but patchwork makes this a bit more straightforward.
# Proximity of comparison is maximized here, so I think it's best to have them stacked.

stacked.hist <- athleticism.hist/intelligence.hist 
print(stacked.hist)

# Back to the two response variables "prop.money.to.winner" and "prop.coach.to.winner", I would still like to see 
# whether my other fixed factors affect the distribution of funds to the participants. I can do this by constructing 
# a linear model that includes the other fixed factors I'm interested in, and then testing how well the assumptions of my 
# lm fit my data using the performance::check_model() function.

WLdata_money_lm_cis <- lm(prop.money.to.winner ~ participant.gender + 
                          winner.name + 
                          char.age + 
                          comp.context, WLdata_cis
)
summary(WLdata_money_lm_cis)
check_model(WLdata_money_lm_cis)

WLdata_coaching_lm_cis <- lm(prop.coach.to.winner ~ participant.gender + 
                             winner.name + 
                             char.age + 
                             comp.context, WLdata_cis
)
summary(WLdata_coaching_lm_cis)
check_model(WLdata_coaching_lm_cis)

# Although worse for the second model, the check_model() function seems to indicate that both datasets
# could probably be modeled more accurately. 

# Also, for some reason, my "check_model()" function only works when I use the "cis" dataframe (i.e., the dataframe
# that has only 2 levels for gender instead of the 5 that are in the full dataframe). This excludes 6 out of 170 
# datapoints. It's not ideal that I've excluded those values, but I can't find any other reason why it doesn't work. 
# Also, if I try to run check_model() on my lm for the full dataset, but exclude the "participant.gender" factor, 
# check_model() works. When I include "participant.gender" (for the full dataset) it check_model() doesn't work. 
# If the "check_model" function worked with my full dataframe, I'd be using that dataframe instead. 

# However, this can be fixed by creating our original LMs (that, include all levels of participant.gender),
# and just manually printing out the plots one by one. 

WLdata_money_lm <- lm(prop.money.to.winner ~ participant.gender + 
                  winner.name + 
                  char.age + 
                  comp.context, WLdata
)

plot(check_posterior_predictions(WLdata_money_lm))
plot(check_normality(WLdata_money_lm))
plot(check_heteroscedasticity(WLdata_money_lm)) ## this plot is way different than the homogeneity of variance check_model() 
# plot generated by performance::check_model(WLdata_money_lm_cis). I'm assuming this may be due to the addition of 
# the three other levels to gender, but I'm not sure.
plot(check_distribution(WLdata_money_lm))

WLdata_coaching_lm <- lm(prop.coach.to.winner ~ participant.gender + 
                        winner.name + 
                        char.age + 
                        comp.context, WLdata
)

plot(check_posterior_predictions(WLdata_coaching_lm))
plot(check_normality(WLdata_coaching_lm))
plot(check_heteroscedasticity(WLdata_coaching_lm))
plot(check_distribution(WLdata_coaching_lm))

## JD: Not clear on what check_distribution does, but feeling skeptical.

# In general, the diagnostic plots inform me that my data could probably be modeled more accurately, 
# as many of the observed datapoints do not fall along the expected lines.

## Nicely ambitious; 2.2/3
