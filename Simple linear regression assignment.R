##########Delivery_time###########
View(delivery_time)
attach(delivery_time)
summary(delivery_time)
plot(delivery_time$Delivery.Time,delivery_time$Sorting.Time) #plot(X,Y)
cor(Delivery.Time,Sorting.Time) #cor(X,Y)

reg <- lm(Delivery.Time~Sorting.Time, data = delivery_time) # Y ~ X
summary(reg)
confint(reg, level = 0.95)
predict(reg,deliverytime)

reg_sqrt <- lm(Delivery.Time~sqrt(Sorting.Time),data = delivery_time)
summary(reg_sqrt)
confint(reg_sqrt, level=0.95)
predict(reg_sqrt,delivery_time)

reg_log <- lm(Delivery.Time~log(Sorting.Time),data = delivery_time)
summary(reg_log)
confint(reg_log, level = 0.95)
predict(reg_log,delivery_time)

reg1 <- lm(log(Delivery.Time)~Sorting.Time + I(Sorting.Time*Sorting.Time),data = delivery_time)
summary(reg1)
confint(reg1, level = 0.95)
predict(reg1,delivery_time)

###########Salary_hike############
View(SalaryData)
attach(SalaryData)
summary(SalaryData)
plot(SalaryData$YearsExperience,SalaryData$Salary) #plot(X,Y)
cor(YearsExperience,Salary) #cor(X,Y)

reg <- lm(YearsExperience~Salary, data = SalaryData) # Y ~ X
summary(reg)
confint(reg, level = 0.95)
predict(reg,SalaryData)

reg_sqrt <- lm(YearsExperience~sqrt(Salary),data = SalaryData)
summary(reg_sqrt)
confint(reg_sqrt, level=0.95)
predict(reg_sqrt,SalaryData)

reg_log <- lm(YearsExperience~log(Salary),data = SalaryData)
summary(reg_log)
confint(reg_log, level = 0.95)
predict(reg_log,SalaryData)

reg1 <- lm(log(YearsExperience)~Salary + I(Salary*Salary),data = SalaryData)
summary(reg1)
confint(reg1, level = 0.95)
predict(reg1,SalaryData)

