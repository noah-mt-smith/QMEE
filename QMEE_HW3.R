# Noah Smith QMEE HW3

library(tidyverse)
WLdata <- read_csv("WL_soc_clean.csv")
summary(WLdata)

# need to change the relevant characters and numeric variables to factors.


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
# The competitive context refers to the whether participants read a scenario about a spelling bee
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
  theme_linedraw()
)

print(money.cont.cis.boxplot)

coaching.cont.cis.boxplot <- (ggplot( WLdata_cis, aes(x = comp.context, y = prop.coach.to.winner, fill = participant.gender)) +
                             geom_boxplot() +
                             labs(x="Competitive context", y = "Proportion of coaching hours allocated to winner", fill = "Gender") +
                             scale_fill_manual(values = c("blue","red"), labels = c("cis men", "cis women")) + 
                             theme_linedraw()
)

print(coaching.cont.cis.boxplot)

