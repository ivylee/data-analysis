library(bnlearn)

#nb <- naive.bayes("SeriousDlqin2yrs", c("RevolvingUtilizationOfUnsecuredLines",
#"age", "NumberOfTime30.59DaysPastDueNotWorse", "DebtRatio", "MonthlyIncome",
#"NumberOfOpenCreditLinesAndLoans", "NumberOfTimes90DaysLate",
#"NumberRealEstateLoansOrLines", "NumberOfTime60.89DaysPastDueNotWorse",
#"NumberOfDependents"), dis_train)
#
#tan <- tree.bayes(dis_train, "SeriousDlqin2yrs",
#c("RevolvingUtilizationOfUnsecuredLines", "age",
#"NumberOfTime30.59DaysPastDueNotWorse", "DebtRatio", "MonthlyIncome",
#"NumberOfOpenCreditLinesAndLoans", "NumberOfTimes90DaysLate",
#"NumberRealEstateLoansOrLines", "NumberOfTime60.89DaysPastDueNotWorse",
#"NumberOfDependents"), mi="mi")

#labels
#gs
#fast.iamb
#naive
nb <- naive.bayes("SeriousDlqin2yrs", names(dis_train)[-1], dis_train)
nbcv <- bn.cv(dis_train, nb, loss="pred",
loss.args=list(target="SeriousDlqin2yrs"))

banned <-
data.frame(from=c("SeriousDlqin2yrs","SeriousDlqin2yrs","SeriousDlqin2yrs","SeriousDlqin2yrs","SeriousDlqin2yrs","SeriousDlqin2yrs","SeriousDlqin2yrs","SeriousDlqin2yrs","SeriousDlqin2yrs","SeriousDlqin2yrs","RevolvingUtilizationOfUnsecuredLines","NumberOfTime30.59DaysPastDueNotWorse","DebtRatio","MonthlyIncome","NumberOfOpenCreditLinesAndLoans","NumberOfTimes90DaysLate","NumberRealEstateLoansOrLines","NumberOfTime60.89DaysPastDueNotWorse","NumberOfDependents"),
to=c("RevolvingUtilizationOfUnsecuredLines","age","NumberOfTime30.59DaysPastDueNotWorse","DebtRatio","MonthlyIncome","NumberOfOpenCreditLinesAndLoans","NumberOfTimes90DaysLate","NumberRealEstateLoansOrLines","NumberOfTime60.89DaysPastDueNotWorse","NumberOfDependents","age","age","age","age","age","age","age","age","age"))
allowed <- data.frame(from=c("RevolvingUtilizationOfUnsecuredLines"),
to=c("DebtRatio"))

#gs <- gs(dis_train, blacklist=banned)
#iamb <- iamb(dis_train, blacklist=banned)
#iiamb <- inter.iamb(dis_train, blacklist=banned)
fiamb <- fast.iamb(dis_train, blacklist=banned)
choose.direction(fiamb, c("RevolvingUtilizationOfUnsecuredLines",
"DebtRatio"), data=dis_train, criterion="k2", debug=T)
fiamb2 <- set.arc(fiamb, "RevolvingUtilizationOfUnsecuredLines",
"DebtRatio")
fit <- bn.fit(fiamb2, dis_train, method="bayes")
