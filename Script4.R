rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) }
curriculum_1 <- rnorm2(50, 170.5, 14.5)
curriculum_2 <- rnorm2(50, 168.3, 12.8)
curriculum_3 <- rnorm2(50, 167.6, 17.7)
curriculum_4 <- rnorm2(50, 172.8, 16.8)
combined <- c(curriculum_1, curriculum_2, curriculum_3, curriculum_4)
length(combined)
mean(combined)
max.len = max(length(curriculum_1), length(curriculum_2), length(curriculum_3), length(curriculum_4))
curriculum_1 = c(curriculum_1, rep(NA, max.len - length(curriculum_1)))
curriculum_2 = c(curriculum_2, rep(NA, max.len - length(curriculum_2)))
curriculum_3 = c(curriculum_3, rep(NA, max.len - length(curriculum_3)))
curriculum_4 = c(curriculum_4, rep(NA, max.len - length(curriculum_4)))
abc <- data.frame(curriculum_1, curriculum_2, curriculum_3, curriculum_4)
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
Fcrit <- qf(.95, df1=3, df2=196) 
Fcrit