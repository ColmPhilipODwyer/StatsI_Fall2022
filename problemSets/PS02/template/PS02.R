install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

pchisq(3.79, df=2, lower.tail=FALSE)
# P = 0.1503183
pchisq(3.59, df=1, lower.tail=FALSE)
# P = 0.05812824

random_sample_villages <- subset(Villages, female==0 | reserved==1)
female_random_lm <- lm(water ~ female, data = random_sample_villages)
summary(female_random_lm)
female_random_with_irrigation_lm <- lm(water ~ female + irrigation, data = random_sample_villages)
summary(female_random_with_irrigation_lm)