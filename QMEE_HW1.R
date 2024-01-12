#Noah Smith QMEE HW1

# This script runs from the main repo directory (QMEE)

# Additional files
## See "QMEE_WL_social_data.csv" for the dataset used to conduct this one-sample
## t-test.
## See "README.md" for an explanation of how the dataset was collected, 
## the context of the experiment, and our main explicit predictions. 

# Our predictions were that participants would allocate significantly more 
# 1) funds and 2) coaching hours to winners than to losers. We can statistically 
# test these predictions with two one-sample t-tests with mu = 0.5
# (for both proportion of money allocated to winner and proportion of coaching
# hours allocated to winner).

# import and view dataset

WL.social.data <- read.csv("QMEE_WL_social_data.csv")
View(WL.social.data)

# One-sample t-test with the null that participants allocate 0.5 of their money to the winner

prop.money.to.winner <- WL.social.data$prop.money.to.winner

prop.money.t.test <- t.test(prop.money.to.winner, mu = 0.5, alternative = "two.sided")

print(prop.money.t.test)

# One-sample t-test with the null that participants allocate 0.5 of their coaching hours to the winner

prop.coaching.to.winner <- WL.social.data$prop.coach.to.winner

prop.coaching.t.test <- t.test(prop.coaching.to.winner, mu = 0.5, alternative = "two.sided")

print(prop.coaching.t.test)