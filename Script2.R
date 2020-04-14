rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) }
a <- rnorm2(30,50.26,10.45)
b <- rnorm2(30,45.32,12.76)
c <- rnorm2(30,53.67,11.47)
combined <- c(a,b,c)
length(combined)
mean(combined)
max.len = max(length(a), length(b), length(c))
x = c(a, rep(NA, max.len - length(a)))
y = c(b, rep(NA, max.len - length(b)))
z = c(c, rep(NA, max.len - length(c)))
abc <- data.frame(x,y,z)
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
Fcrit <- qf(.99, df1=2, df2=87) 
Fcrit