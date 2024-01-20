# Noah Smith QMEE HW2

library(tidyverse)
setwd("/Users/noahsmith/QMEE")
WLdata <- read_csv("QMEE_WL_social_data.csv")
View(WLdata)
print(WLdata)

# we can check if there are any NAs or oddities in our data using 

problems(WLdata)

## the problems() function indicates nothing is wrong or abnormal (no NAs, odd values, etc.)

summary(WLdata)

# First, I would like to convert my character variables into factors for the sake of data hygeine and so 
# I don't run into any issues or weird bugs when I eventually run my analyses. I am going to 
# save a new datsaet with the mutated versions of the character variables from my old datasets.

summary(WLdata)

# the summary table tells us that all but four of our variables are characters, so I am changing these to factors

WLdata_factors <- (WLdata
                   %>% mutate(participant.gender = as.factor(participant.gender))
                   %>% mutate(scenario.number = as.factor(scenario.number))
                   %>% mutate(comp.context = as.factor(comp.context))
                   %>% mutate(char.age = as.factor(char.age))
                   %>% mutate(winner.name = as.factor(winner.name))
)

# also, "participant.age" is being stored as a character, when it's actually a numeric variable
# The only issue is that it includes several alphanumeric responses because of data collection,
# as I provided the options "above 23" and "below 17" because I expected the vast majority 
# of my sample population to sit within the age range of 17 - 23. I have a feeling
# very few people responded with "above 23" and "below 17", so it's possible I can replace those responses
# with NAs.

# so I'm also going to convert "participant.age" to a factor and then view its summary 
# table to see if it's acceptable for me to eliminate the non-numeric responses

WLdata_factors <- (WLdata_factors
                   %>% mutate(participant.age = as.factor(participant.age))
)

summary(WLdata_factors$participant.age)

# according to the summary table, only 4 people responded "greater than 24", while 2 
# people responded "less than 17". Seeing as these responses represent less than 5% 
# of the responses in that factor, I am comfortable making them NAs and potentially excluding
# them in any analyses. In future studies, I will be making my age recording only 
# numeric to avoid this issue.

WLdata_factors <- (WLdata_factors
                   %>% mutate(participant.age = case_when(
                     participant.age=="Greater than 23" ~ NA,
                     participant.age=="Less than 17" ~ NA,
                     TRUE ~ participant.age
                   )))


# now, upon inspecting the dataframe for the "participant.age" factor, I see that there are
# 6 responses that are "NA" where "less than 17" and "greater than 23" used to be. 

View(WLdata_factors)

# I can now convert "participant.age" to a numeric variable, and will rename my data to "cleaner"

WLdata_cleaner <- (WLdata_factors
                   %>% mutate(participant.age = as.numeric(participant.age))
)

# lastly, I see that my "participant.ID" variable is numeric according to the summary table,
# and I would like to change this to a factor just to avoid any weird stuff going on.

WLdata_cleaner <- (WLdata_cleaner
                   %>% mutate(participant.ID = as.factor(participant.ID))
)

summary(WLdata_cleaner)
View(WLdata_cleaner)

# Now that our data is a bit more organized, we can do a bit of visualization for fun!
# first, let's visualize how gender influences the allocation of money. 

ggplot(WLdata_cleaner, aes(x = participant.gender, y = prop.money.to.winner)) + geom_boxplot()

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

library(ggplot2)
ggplot(WLdata_cleaner, aes(x = participant.gender, y = prop.money.to.winner)) + geom_boxplot() + theme_linedraw()

# Therefore, it may be worthwhile to create a plot that does not include these three levels of the factor.
# I am therefore using base r to create a dataframe that includes only "cisgender man" and "cisgender woman" in the gender category, 
# since the low power of the other three categories doesn't provide enough reliability.

WLdata_cis_only = WLdata_cleaner[WLdata_cleaner$participant.gender == "cisgender man" | WLdata_cleaner$participant.gender == "cisgender woman",]
View(WLdata_cis_only)

# I can check that the 6 non-cisgender responses have been removed by taking the length of my 
# "participant.gender" column

print(length(WLdata_cis_only$participant.gender))

# It is 164, indicating that the 6 unwanted responses have been eliminated. Now, 
# we can replot the boxplot using our new dataframe, and we will only see the two 
# cisgender responses.

ggplot(WLdata_cis_only, aes(x = participant.gender, y = prop.money.to.winner)) + geom_boxplot() + theme_linedraw()

# we can also do the same with allocated coaching hours. 

ggplot(WLdata_cis_only, aes(x = participant.gender, y = prop.coach.to.winner)) + geom_boxplot() + theme_linedraw()

## based on the coaching boxplot, it looks like the overall distribution might be skewed towards 0

# we can check the distributions of our response variables using a histogram

histo.money.to.winner <- ggplot(WLdata_cleaner, aes(prop.money.to.winner)) + geom_histogram()
print(histo.money.to.winner)

# based on this histogram, the "money.to.winner" dependent variable is left skewed, towards the upper bound of 1.

histo.coaching.to.winner <- ggplot(WLdata_cleaner, aes(prop.coach.to.winner)) + geom_histogram()
print(histo.coaching.to.winner)

# based on this histogram, the response variable "proportion.coach.to.winner" is right skewed, towards the lower bound of 0.

# we can also inspect the normality of these variables using QQ plots, 

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
# them before conducting parametric tests.



