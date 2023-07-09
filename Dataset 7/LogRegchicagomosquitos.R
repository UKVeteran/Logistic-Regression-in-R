library(tidyverse)
library(stats)
library(ggplot2)

Mosquito <- read.csv("C:\\Users\\jau19\\OneDrive\\Desktop\\Data Analyst\\Logistic Regression R\\Dataset 7\\west-nile-virus-wnv-mosquito-test-results.csv")

summary(Mosquito$WEEK)

summary(Mosquito$SEASON)

ggplot(Mosquito, mapping = aes(x = Mosquito$WEEK)) +
  geom_bar(aes(fill = Mosquito$RESULT))

model <- glm(Mosquito$RESULT ~ Mosquito$WEEK, family = binomial)
summary(model)


