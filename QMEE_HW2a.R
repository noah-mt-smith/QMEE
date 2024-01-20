# Noah Smith QMEE HW2

library(tidyverse)
setwd("/Users/noahsmith/QMEE")
WLdata <- read_csv("QMEE_WL_social_data.csv")
View(WLdata)
print(WLdata)

# we can check if there are any NAs or oddities in our data using 

problems(WLdata)

## the problems() function indicates nothing is wrong or abnormal (no NAs, odd values, etc.)

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

# now, I'm going to save my cleaned dataframe as an RDS and then load that RDS into 
# my QMEE_HW2b.R file 

saveRDS(WLdata_cleaner, file = "WLdata_cleaner.rds")



