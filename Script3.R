rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) }
control <- rnorm2(40, 21.4, 4.5)
CBT <- rnorm2(40, 16.9, 5.5)
CCT <- rnorm2(40, 19.1, 5.8)
combined <- c(control, CBT, CCT)
length(combined)
mean(combined)
max.len = max(length(control), length(CBT), length(CCT))
control = c(control, rep(NA, max.len - length(control)))
CBT = c(CBT, rep(NA, max.len - length(CBT)))
CCT = c(CCT, rep(NA, max.len - length(CCT)))
abc <- data.frame(control, CBT, CCT)
abc <- stack(abc)
library(dplyr)
group_by(abc, ind) %>%
  summarise(
    count = n(),
    mean = mean(values, na.rm = TRUE),
    sd = sd(values, na.rm = TRUE)
  )
a.res <- aov(values ~ ind, data = abc)
summary(a.res)
Fcrit <- qf(.99, df1=2, df2=117)
Fcrit