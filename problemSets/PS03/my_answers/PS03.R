
# Q1.1 Regression with outcome variable voteshare and explanatory variable
# difflog
DifflogReg <- lm(voteshare ~ difflog, data = incumbents_subset)
summary(DifflogReg)

# Q1.2 Scatterplot & regression line for voteshare and difflog
plot(voteshare ~ difflog, data = incumbents_subset)
abline(lm(voteshare ~ difflog, data = incumbents_subset))

# Q1.3 Saving residuals as an object
DifflogRes <- resid(DifflogReg)

# Q1.4 Writing the prediction equation
       # Y = 0.579031 + 0.041666(X)
       # Where Y = Predicted voteshare, X = difflog

# Q2.1 Regression with presvote as outcome variable and difflog as explanatory
       # variable
DifflogPresReg <- lm(presvote ~ difflog, data = incumbents_subset)
summary(DifflogPresReg)

# Q2.2 Scatter plot & regression line for presvote vs difflog
plot(presvote ~ difflog, data = incumbents_subset)
abline(lm(presvote ~ difflog, data = incumbents_subset))

# Q2.3 Saving residuals as an object
DifflogPresRes <- resid(DifflogPresReg)

# Q2.4 Writing the prediction equation
# Y = 0.507583 + 0.023837(X)
# Where Y = presvote, x = difflog

# Q3.1 Regression with voteshare as outcome variable and presvote as explanatory
# variable
VotePresReg <- lm(voteshare ~ presvote, data = incumbents_subset)
summary(VotePresReg)                  

# Q3.2 Scatter plot & regression line for voteshare vs presvote
plot(voteshare ~ presvote, data = incumbents_subset)
abline(lm(voteshare ~ presvote, data = incumbents_subset))

# Q3.3 Writing the prediction equation
# Y = 0.441330 + 0.388018(X)
# Where Y = voteshare, X = presvote

# Q4.1 Regression with DifflogRes as outcome variable and DifflogPresRes
# as explanatory variable
ResidualsReg <- lm(DifflogRes ~ DifflogPresRes)
summary(ResidualsReg)

# Q4.2 Scatter plot & regression line for DifflogRes vs DifflogPresRes
plot(DifflogRes ~ DifflogPresRes)
abline(lm(DifflogRes ~ DifflogPresRes))

# Q4.3 Writing the prediction equation
# Y = -4.860e-18 + 2.569e-01(X)
# Where Y = Residual for Q1 (voteshare vs difflog), X = Residual for Q2
# (voteshare vs presvote)


# Q5.1 Multiple regression with outcome variable voteshare and explanatory
# variables difflog and presvote. 
MultiVoteshareReg <- lm(voteshare ~ difflog + presvote, data = incumbents_subset)
summary(MultiVoteshareReg)


# Q5.2 Writing the prediction equation
# Predicted voteshare = 0.4486442 + 0.0355431(difflog) + 0.2568770(presvote)

# Q5.3 What is identtical between the results of Q4 and Q5?

# The Residual Standard error is the same in both models.
# The residuals for the models in both Q4 and Q5 tell us the variation from our
# expected values when taking into account both difflog and presvote. Therefore,
# we would expect the two values to be the same.