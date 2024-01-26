# Noah Smith QMEE HW3

library(tidyverse)
library(patchwork)
library(performance)
#library(see)
library(ggplot2); theme_set(theme_linedraw())
WLdata <- read_csv("WL_soc_clean.csv")
summary(WLdata)

# need to change the relevant characters and numeric variables to factors.

## load performance() package and create an lm for one 
# of my predictors. 

WLdata <- (WLdata
  %>% mutate(across(where(is.character), as.factor))
  %>% mutate(across(participant.ID, as.factor))
)

summary(WLdata)

#View(WLdata)

# the first thing I'd like to do is create boxplots on how gender affects proportion of 
# money and coaching hours allocated to the winner and loser

money.gender.boxplot <- (ggplot(WLdata, aes(x = participant.gender, y= prop.money.to.winner)) + 
  geom_boxplot(fill="gray") + 
  labs(x="Gender", y = "Proportion of funds allocated to winner") + 
  ylim(0,1)
)

print(money.gender.boxplot)

coaching.gender.boxplot <- (ggplot(WLdata, aes(x = participant.gender, y= prop.coach.to.winner)) + 
  geom_boxplot(fill="gray") + 
  labs(x="Gender", y = "Proportion of coaching hours to winner") + 
  ylim(0,1)
)

print(coaching.gender.boxplot)

# The lack of power for the non-cisgender individuals makes these boxplots fairly 
# uninformative, so I'm not sure having them is necessary. So, I'm going to create 
# a dataframe without those categories and plot those instead. Ideally, I would have a
# larger sample of non-cis identity individuals.


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
# of its proximity to one another.

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

# I also added a line to the boxplots at the point of interest, y = 0.5, because 
# we care about comparing the distribution of funds and coaching hours to that point. 
# I.e., if there's no difference between funds allocated to winners and losers, 
# then boxplots should be centered around the y = 0.5 line. Although theme(linedraw)
# already includes a line at 0.5, I just wanted to make it clearer. 

# It would also be interesting to see how age influences the proportion of money allocated to winners and losers. Since age is numeric, 
# we can create a scatter plot with a loess to see what kind of trend there is. 

age.money.scatter <- (ggplot(WLdata_cis, aes(x = participant.age, y = prop.money.to.winner)) + 
  geom_point() +
# geom_smooth() + # I had added this and it was working, but it was giving me a ton of warnings..., 
# I just decided to comment it out.
  ylim(0,1) +
  labs(x = "Participant age", y = "Proportion of money allocated to winner")
)

print(age.money.scatter)

# there seems to be a slight upward trend for awarding money to the winner as age increases, but there are so few observations in the 
# upper age categories that they should be taken with a grain of salt.


# we can also do the same for coaching hours.

age.coaching.scatter <- (ggplot(WLdata_cis, 
  aes(x = participant.age, y = prop.coach.to.winner)) + 
  geom_point() + 
# geom_smooth() + # again, this was working but it was
# giving me several warnings so I commented it out. 
  ylim(0,1) +
  labs(x = "Participant age", y = "Proportion of coaching hours allocated to winner")
)

print(age.coaching.scatter)

# The trend here looks very flat across all levels of age

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

stacked.hist <- athleticism.hist/intelligence.hist 
print(stacked.hist)

# I believe my main variables of interest are best tested using wilcoxon sign-ranked tests (but I'm not sure...) 
# The wilcoxon test provides a robust non-parametric way to test whether the mean of my factors
# is equal to what they would be if there was no difference between the amount of money 
# participants allocated to winners and losers (and no difference between coaching hours
# allocated to winners and losers. This is equivalent to saying that participants allocated 0.5
# of the available funds and coaching hours to the winner.

wilcox.money <- wilcox.test(WLdata$prop.money.to.winner - 0.5)
print(wilcox.money)

wilcox.coaching <- wilcox.test(WLdata$prop.coach.to.winner - 0.5)
print(wilcox.coaching)

# however, I would still like to see whether my other fixed factors affected the distribution
# of funds to the participants. I can do this by constructing a linear model that
# includes the other fixed factors I'm interested in. 

WLdata_money_lm <- lm(prop.money.to.winner  ~ participant.gender + winner.name + char.age + comp.context, WLdata)
summary(WLdata_money_lm)
check_model(WLdata_money_lm)

WLdata_coaching_lm <- lm(prop.coach.to.winner ~ participant.gender + winner.name + char.age + comp.context, WLdata)
summary(WLdata_coaching_lm)
check_model(WLdata_coaching_lm)

# for some reason, my "check_model()" command isn't working on my lm. I don't know why this is, 
# but I know Shane is also having a similar error. 

# other tests to look at...

wilcox.athleticism <- wilcox.test(as.numeric(as.character(WLdata$athleticism.perception)) - 1)
print(wilcox.athleticism)

wilcox.intelligence <- wilcox.test(as.numeric(as.character(WLdata$intelligence.perception)), mu = 1)
print(wilcox.intelligence)


