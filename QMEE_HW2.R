# Noah Smith QMEE HW2

library(tidyverse)
setwd("/Users/noahsmith/QMEE")
WLdata <- read_csv("QMEE_WL_social_data.csv")
View(WLdata)
print(WLdata)

# One immediate issue is that the column "participant.gender" includes 
# a total of 170 responses and 5 levels (cisgender man, cisgender woman, 
# nonbinary, transgender man, and prefer not to say)
# However, the overwhelming majority of the responses are "cisgender man" 
# and "cisgender woman". 

num.cis.man <- (WLdata$participant.gender == "cisgender man")
print(sum(num.cis.man)) 

# we have 58 responses that are "cisgender man"

num.cis.woman <- (WLdata$participant.gender == "cisgender woman")
print(sum(num.cis.woman))

# we have 106 responses that are "cisgender woman"

print(length(WLdata$participant.gender))

# 164 out of 170 people responded "cisgender man" or "cisgender woman" within our 
# gender factor. When plotting these data, then, the low power of the three less chosen recorded
# categories ("prefer not to say", "nonbinary", and "transgender man") means they will will not be very 
# informative if they are plotted. From the below plot, which includes all 5 levels of our 
# "participant.gender" factor, we see how irregular the distributions for these 3 categories 
# are in our response variable "prop.money.to.winner"

library(ggplot2)
ggplot(WLdata, aes(x = participant.gender, y = prop.money.to.winner)) + geom_boxplot() + theme_linedraw()

# Therefore, it may be worthwhile to create a plot that does not include these three levels of the factor.
# I am creating a dataframe that includes only "cisgender man" and "cisgender woman" in the gender category, 
# since the low power of the other three categories doesn't provide enough reliability.

WLdata_cis_only = WLdata[WLdata$participant.gender == "cisgender man" | WLdata$participant.gender == "cisgender woman",]
View(WLdata_cis_only)

# I can check that the 6 non-cisgender responses have been removed by taking the length of my 
# "participant.gender" column

print(length(WLdata_cis_only$participant.gender))

# It is 164, indicating that the 6 unwanted responses have been eliminated. Now, 
# we can replot the boxplot using our new dataframe, and we will only see the two 
# cisgender responses.

ggplot(WLdata_cis_only, aes(x = participant.gender, y = prop.money.to.winner)) + geom_boxplot() + theme_linedraw()

ggplot(WLdata_cis_only, aes(x = comp.context, y = prop.money.to.winner)) + geom_boxplot() + theme_linedraw()

ggplot(WLdata_cis_only, aes(x = comp.context, y = prop.coach.to.winner)) + geom_boxplot() + theme_linedraw()

# academic prop coaching hours is skewed low

