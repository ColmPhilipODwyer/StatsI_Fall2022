



### Question One ###

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112,
       98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#First I calculate the average IQ for the sample
AverageIQ <- mean(y)

# Then I calculate the standard deviation in the sample
StandardDeviation <- sd(y)

# Next I calculate the margin of error using the standard deviation and sample
# size

margin_error <- qt(0.95,df=24)*StandardDeviation/sqrt(25)

# Finally, I calculate the upper and lower intervals
UpperInterval <- mean(y) + margin_error
LowerInterval <- mean(y) - margin_error
UpperInterval
LowerInterval
# Intervals [93.95993, 102.9201]


# I conduct a one sample t-test
t.test(y, mu = 100, alternative = "greater")
# p-value = 0.7215
#Because p>0.05, we do not have evidence to reject the null-hypothesis,
# that the average student IQ in the school is the same or lower than the
# average IQ score (100) among all the schools in the country.

#Alternatively, we can calculate this manually
AverageIQ_t <- ((AverageIQ - 100)/(StandardDeviation/(sqrt(25))))
AverageIQ_p <- pt(AverageIQ_t, 24, lower.tail = FALSE)
AverageIQ_p
# AverageIQ_p = 0.7215383

                ### Question Two ###

# First, I create my relevant variables:

pairs(~ Expenditure + PersonalIncome + FinanciallyInsecure + Urban, data = Expenditure)


# Each of the three independent variables is positively associated with
# Expenditure on Shelters/Housing Assistance.

# Personal income and urban population appear to have a strong positive
# correlation.On the other hand, Financial Insecurity doesn't appear to have a
# strong association with either Urban population or Personal Income.



# I produce a boxplot in order to compare the average expenditure on
# shelters/housing assistance across the 4 regions.

boxplot(ExpenditureOnShelters ~ Region)
#This box plot shows that expenditure on shelters/housing assistance is
# highest in the Western region of the US.


# I produce a scatterplot of Expenditure on Shelters vs Personal Income,
# with different regions in different colours:

plot(PersonalIncome, ExpenditureOnShelters, col=Region)
abline(lm(ExpenditureOnShelters  ~ PersonalIncome))

# to better understand the data and see how states in different regions differ,
# I add 4 lines of best fit, one for each of the 4 regions:

Northeast <- subset(Expenditure, Region == "1")
Northcentral <-subset(Expenditure, Region == "2")
South <-subset(Expenditure, Region == "3")
West <-subset(Expenditure, Region == "4")

abline(lm(Northeast$Expenditure ~ Northeast$PersonalIncome), col = "red")
abline(lm(Northcentral$Expenditure ~ Northcentral$PersonalIncome), col = "blue")
abline(lm(South$Expenditure ~ South$PersonalIncome), col = "yellow")
abline(lm(West$Expenditure ~ West$PersonalIncome), col = "green")

# It appears from the data that the positive association between Personal
# Income and higher rates of expenditure on shelters and housing assistance
# is most significant in the Northeast, and is least significant in the
# Northcentral region, with the other two regions falling in betweeen.
# Northeast states also appear to have the highest Personal Incomes overall,
# whilst southern states have the lowest incomes.