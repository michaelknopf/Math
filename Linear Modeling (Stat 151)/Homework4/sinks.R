setwd("~/Desktop/Stat 151/Homework4")
sink(file = "select.R")

summary(backward())

summary(forward())

summary(adjRsqr())

summary(AIC())

summary(BIC())

summary(Mallow())

sink()