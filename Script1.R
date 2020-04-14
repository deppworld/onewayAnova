Group1 <-c(51,45,33,45,67)
Group2 <-c(23,43,23,43,45)
Group3 <-c(56,76,74,87,56)
Combined_Groups <- data.frame(cbind(Group1, Group2,Group3))
Combined_Groups 
Stacked_Groups <- stack(Combined_Groups)
Stacked_Groups
Anova_Result <- aov(values ~ ind, data = Stacked_Groups)
View(Anova_Result)
summary(Anova_Result)
Fcrit <- qf(.95, df1=2, df2=12) 
Fcrit
