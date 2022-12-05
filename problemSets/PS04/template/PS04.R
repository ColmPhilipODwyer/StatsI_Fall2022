install.packages("car")
library(car)
data(Prestige)
help(Prestige)
professional <- ifelse(Prestige$type == "prof",
                        c(1), c(0))
prestige_model <- lm(prestige ~ income + professional + income*professional, data = Prestige)
summary(prestige_model)
