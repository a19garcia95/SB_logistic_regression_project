NH11 <- readRDS("NatHealth2011.rds")
labs <- attributes(NH11)$labels

str(NH11$everwrk)
levels(NH11$everwrk)
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))

everwrk.out <- glm(everwrk ~ age_p + r_maritl,
               data=NH11, 
               family="binomial")
coef(summary(everwrk.out))

#age_p, divorced, separated, and never married are all higly significant (***)
#unknown marital status is significant (*)

#Predict the probability of working for each level of marital status.

predDat <- with(NH11,
                expand.grid(r_maritl = c(1:10)))

cbind(predDat, predict(everwrk.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

#install.packages('effects')
library(effects)
plot(allEffects(everwrk.out))
