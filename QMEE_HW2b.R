# NOAH SMITH QMEE HW2b

# first, we load our clean data using readRDS

WLdata_cleaner <- readRDS("WLdata_cleaner.rds")
## View(WLdata_cleaner)

# Now that our data is a bit more organized, we can do a bit of visualization for fun!
# first, let's visualize how gender influences the allocation of money. 

library(tidyverse)
ggplot(WLdata_cleaner, aes(x = participant.gender, y = prop.money.to.winner)) + geom_boxplot(fill = "gray") + theme_linedraw()

## see https://clauswilke.com/dataviz/avoid-line-drawings.html

# One immediate issue we see when we visualize is that the boxplots for 
# "nonbinary", "prefer not to say" and "transgender man" look very funky 
# compared to the "cisgender" boxplots. If we inspect the data, we see that the column 
# "participant.gender" includes a total of 170 responses and 5 levels (cisgender man, 
# cisgender woman, nonbinary, transgender man, and prefer not to say)
# However, the overwhelming majority of the responses are "cisgender man" 
# and "cisgender woman". 

num.cis.man <- (WLdata_cleaner$participant.gender == "cisgender man")
print(sum(num.cis.man)) 

# we have 58 responses that are "cisgender man"

num.cis.woman <- (WLdata_cleaner$participant.gender == "cisgender woman")
print(sum(num.cis.woman))

# we have 106 responses that are "cisgender woman"

print(length(WLdata_cleaner$participant.gender))

# 164 out of 170 people responded "cisgender man" or "cisgender woman" within our 
# gender factor. When plotting these data, then, the low power of the three less chosen recorded
# categories ("prefer not to say", "nonbinary", and "transgender man") means they will will not be very 
# informative if they are plotted. From the below plot, which includes all 5 levels of our 
# "participant.gender" factor, we see how irregular the distributions for these 3 categories 
# are in our response variable "prop.money.to.winner"

# Therefore, it may be worthwhile to create a plot that does not include these three levels of the factor.
# I am therefore using base r to create a dataframe that includes only "cisgender man" and "cisgender woman" in the gender category, 
## since the low power of the other three categories doesn't provide enough reliability.

## actually a very general problem in this kind of data analysis -- stuck
## between "ignore non-cis people" or "cope with the fact that we have
## really crappy data about non-cis people"

WLdata_cis_only = WLdata_cleaner[WLdata_cleaner$participant.gender == "cisgender man" | WLdata_cleaner$participant.gender == "cisgender woman",]
## View(WLdata_cis_only)

## BMB: or ...
WLdata_cis_only = WLdata_cleaner %>% filter(grepl("cisgender", participant.gender))


# I can check that the 6 non-cisgender responses have been removed by taking the length of my 
# "participant.gender" column

print(length(WLdata_cis_only$participant.gender))

# It is 164, indicating that the 6 unwanted responses have been eliminated. Now, 
# we can replot the boxplot using our new dataframe, and we will only see the two 
# cisgender responses.

ggplot(WLdata_cis_only, aes(x = participant.gender, y = prop.money.to.winner)) + geom_boxplot(fill = "gray") + theme_linedraw()

# we can also do the same with allocated coaching hours. 

ggplot(WLdata_cis_only, aes(x = participant.gender, y = prop.coach.to.winner)) + geom_boxplot() + theme_linedraw()

## BMB: if you want linedraw, use theme_set(theme_linedraw()) rather
## than repeating it

## based on the coaching boxplot, it looks like the overall distribution might be skewed towards 0

# we can check the distributions of our response variables using a histogram

histo.money.to.winner <- ggplot(WLdata_cleaner, aes(x = prop.money.to.winner)) + geom_histogram()
print(histo.money.to.winner)

## BMB: wider bins?

# based on this histogram, the "money.to.winner" dependent variable is left skewed, towards the upper bound of 1.

histo.coaching.to.winner <- ggplot(WLdata_cleaner, aes(x = prop.coach.to.winner)) + geom_histogram()
print(histo.coaching.to.winner)

# based on this histogram, the response variable "proportion.coach.to.winner" is right skewed, towards the lower bound of 0.

# we can also inspect the normality of these variables using QQ plots, 

## BMB: we do **not** care about normality of these variables ...

# money first
prop.money.to.winner = WLdata_cleaner$prop.money.to.winner
qqnorm(y = prop.money.to.winner, main = "QQ plot of proportion money allocated to winner")
qqline(y = prop.money.to.winner)

# then coaching
prop.coach.to.winner = WLdata_cleaner$prop.coach.to.winner
qqnorm(y = prop.coach.to.winner, main = "QQ plot of proportion coaching allocated to winner")
qqline(y = prop.coach.to.winner)

# our money plot deviates slightly from the line, and our coaching plot deviates strongly from the line, indicating that 
# they might be non-normal. To statistically test this, we can conduct a shapiro-wilks test on the residuals of the two dependent variables

lm_money_to_winner <- lm(prop.money.to.winner ~ winner.name, data = WLdata_cleaner)
shapiro.test(residuals(lm_money_to_winner))

lm_coach_to_winner <- lm(prop.coach.to.winner ~ winner.name, data = WLdata_cleaner)
shapiro.test(residuals(lm_coach_to_winner))

# According to these shapiro-wilks tests, neither of these response variables are normally distributed, indicating that we should 
# use non-parametric tests when eventually performing statistical tests on them, or transforming
## them before conducting parametric tests.


## BMB: this testing is unnecessary, or worse. We will discuss it later.

