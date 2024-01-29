JD: I'm going to suggest you keep paragraphs to <1200 characters or so. They're particularly hard for me to read in text files because I have settings to help my bad eyes – but I think it's pretty good general advice as well. I broke a couple of paragraphs below, largely for my own convenience, sorry. Feel free to rethink those decisions.

# Assignment 2 description 

# Two scripts (both should be run from QMEE):

# QMEE_HW2a.R

this script loads my dataset (`QMEE_WL_social_data.csv`) and cleans it. The dataset had several variables which were stored as characters when they should have either been numeric or factor variables, so I changed the relevant variables using mutate(). Also, I decided that I should make my age variable "participant.age" into a numeric variable, but realized that it had a few alphabetical responses (6 out of 170) that would have to be removed before turning it into a numeric variable. I thus used mutate() again to specify which values within my "participant.age" variable I wanted to be read as NAs, and then I converted "participant.age" from a factor to a numeric variable. After the conversion of the appropriate variables from characters to factors (and numeric for "participant.age"), the summary tables were much more informative. Finally, I changed my "Participant.ID" variable (which was numeric) to a factor to avoid any potential weird calculations when I eventually model the data. After cleaning my data, I saved it as an RDS and then uploaded it into QMEE_HW2a.R

# QMEE_HW2b.R: 

This script loads the cleaned dataset (`WLdata_cleaner.rds`) that I created at the end of QMEE_HW2a.R. In this script, I first try to plot a boxplot with ggplot() that shows how participant gender influences the proportion of money allocated to winners. However, the plot includes 3 very low-powered gender levels, leading the visualization of these 3 columns within the boxplot to be quite uninformative. I thus create a new dataframe (using base R) that excludes the 3 low-powered gender levels. I then use ggplot() to create 2 boxplots with the new dataframe (one for how gender influences proportion of money allocated to the winner, the other for how gender influences the proportion of coaching hours allocated to the winner). The boxplots suggested that the data might be slightly skewed, so I plotted histograms and qqplots of both dependent variables of interest (money allocated to winner and coaching hours allocated to winner). The histograms and qqplots suggested that the data was non-normal, so I ran shapiro-wilks tests on the residuals of an lm of the data (not sure if I did this properly though). The test suggested that both response variables had non-normal residuals. 

# What investigations I'd like to do: 

#Right now, I would like to run a good statistical analysis that breaks down how money and coaching hours are allocated to the winner (if there is a significant difference from equal split; i.e., 0.5). I would also like to run analyses on how the response variables (allocating money and coaching to winners) interacts with participant gender and the competitive context of the scenario. If I am conducting parametric tests for those questions, then I'll need to resolve the skews in my two main response variables. I also want to run statistics on variables "athleticism.perception" and "intelligence.perception", but I am unsure whether these variables should be stored as factors or as numeric variables. 

Participants chose between five levels of population variance to report their perception of athleticism in the population and their perception of intelligence in the population. The values they chose between (along with accompanying graphical depictions of populations with these values) were SD = 0.33, 0.5, 1, 2, 3. Therefore, they were almost like a likert scale, which leads me to believe they should be stored as factors (i.e., categorical). However, they also represent deflation (for 0.33 and 0.5) and inflation (2 and 3) of the true IQ distribution's standard deviation (which is 1). Hence the average or median response for the two variables might be more informative of our sample's true perceptions if the variables are stored numerically instead of as factors. 

# Assignment 1 dataset description and explicit prediction: 

# I collected this dataset as part of my Fall 2023 experiment. In our first experiments, my supervisor and I tested whether randomly assigned winners performed better than randomly assigned losers in a subsequent, standardized round. Our results suggested that randomly assigned prior winners performed better than randomly assigned prior losers in a subsequent, standardized round (Smith & Dukas, 2024). Our Fall 2023 experiment examined another aspect of winning and losing: how we react to the wins and losses of others. 

We randomly assigned participants to read one of eight possible passages about a dyadic competition (one winner; one loser) between two fictional individuals. After participants read the passage, we asked them to distribute two finite resources (money and coaching hours) between the two competitors. We predicted that participants would favour winners (allocate more money and coaching hours to them) out of a desire to affiliate with the winner. Affiliating with more competent or athletic individuals could be beneficial to fitness, especially in the context of our ancestral environments--strong coalitions and good reciprocal trading partners may have been vital to survival and reproduction. To mitigate potential confounds present in the reading passages (such as competitive context, character age, or character names), we randomized participants to read one of eight passages, each of the eight being a different combination of competitive context (athletic; academic), character age (elementary; high school), and character and school name (Jamie and East Hills; Morgan and West Lake).  

# We predicted that participants would allocate a greater proportion of money and coaching hours to the winner of the fictional competition.

# In "NS_QMEE_hw1.R", I run two one-sample t-tests on whether participants allocated greater than 0.5 of their money and coaching hours to the winner.

# In this study, we also attempted to measure participants' perceptions of population variation in the traits of "intelligence" and "athleticism". Surprisingly, we know of no data that explicitly measures how humans perceive variation in ability within their population. The ability to perceive how individuals or group members vary in different skills may play an important role in our tendency to favour winners (if such a tendency exists). To understand how people perceive variation in the traits of intelligence and athleticism, we provided participants with a multiple-choice survey that included five graphical depictions of populations ranging from "low" to "high" variance (low meaning more population members grouped towards the average, high meaning fewer population members grouped towards the average). The participant then indicated which of the five graphical depictions aligned with their impressions of population variation, first for intelligence and then for athleticism.

# To construct the distributions we used for intelligence, we began with the standardized IQ distribution as a baseline, which has a standard deviation of 1. When creating the graphical depictions for the 5 population distribution options, we used the middle (third) option as a baseline, and thus made its standard deviation equal to 1 (identical to the standard IQ distribution). Options 1 and 2 were graphical representations of populations with a standard deviation (in intelligence) of 0.33 and 0.5, respectively. Options 4 and 5 were graphical representations of populations with a standard deviation (in intelligence) of 2 and 3, respectively. I.e., participants could respond by picking any of five populations, which had standard deviations in intelligence of 0.33, 0.5, 1, 2, or 3 (although participants could not actually see these numbers).

# For the athleticism distributions, we searched far and wide for strong data on true "athleticism" variation. However, much of the good athletic data comes from professional sports organizations (e.g., the NFL's yearly combine data). Unfortunately, such data suffers from large ceiling effects--e.g., the 100-yard dash data published by the NFL combine has a strong left skew. Thus we reused the standard deviation values from our IQ distributions. To indicate their perception of athleticism variation in the population, participants could choose 1 of 5 populations with standard deviations of 0.33, 0.5, 1, 2, and 3. 

# References
# Smith, N. M., & Dukas, R. (2024). Winner and loser effects in humans: evidence from randomized trials. Animal Behaviour, 207, 101-107.
