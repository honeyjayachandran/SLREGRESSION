# Load sal_data.csv dataset
library(readr)
sal_data <- read_csv("C:/Users/Sony/Downloads/simple linear regression assignment-4/Salary_Data.csv")
View(sal_data)

# Exploratory data analysis
summary(sal_data)

#Scatter plot
plot(sal_data$YearsExperience, sal_data$Salary)  # plot(X,Y)

?plot

attach(sal_data)


#Correlation Coefficient (r)
cor(sal_data$YearsExperience, sal_data$Salary)  # cor(X,Y)=0.9782416

# Simple Linear Regression model
reg <- lm(sal_data$Salary ~ sal_data$YearsExperience) # lm(Y ~ X)

summary(reg)  #MULTIPLE RSQRED=0.957

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(sal_data))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = sal_data, aes(x = sal_data$YearsExperience, y = sal_data$Salary)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = sal_data, aes(x=sal_data$YearsExperience, y=pred))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(sal_data,aes(sal_data$YearsExperience,sal_data$Salary))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logrithamic Model

# x = log(sal_data$YearsExperience); y = sal_data$Salary

plot(log(sal_data$YearsExperience), sal_data$Salary)
cor(log(sal_data$YearsExperience), sal_data$Salary) #COR=0.9240611

reg_log <- lm(sal_data$Salary ~ log(sal_data$YearsExperience))   # lm(Y ~ X)

summary(reg_log)  #MULTIPLE R SQRED=0.8539
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(sal_data))  #RMSE=10302.89

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = sal_data$YearsExperience and y = log(sal_data$Salary)

plot(sal_data$YearsExperience, log(sal_data$Salary))

cor(sal_data$YearsExperience, log(sal_data$Salary)) #COR=0.9653844

reg_exp <- lm(log(sal_data$Salary) ~ sal_data$YearsExperience)  #lm(log(Y) ~ X)

summary(reg_exp) # MULTIPLE R SQRED=0.932

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logsal <- predict(reg_exp)
sal <- exp(logsal)

error = sal_data$Salary - sal 
error

sqrt(sum(error^2)/nrow(sal_data))  #RMSE=7213.235

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(sal_data$YearsExperience, sal_data$Salary)
plot(sal_data$YearsExperience*sal_data$YearsExperience, sal_data$Salary)

cor(sal_data$YearsExperience*sal_data$YearsExperience, sal_data$Salary) 

plot(sal_data$YearsExperience*sal_data$YearsExperience, log(sal_data$Salary))

cor(sal_data$YearsExperience, log(sal_data$Salary))
cor(sal_data$YearsExperience*sal_data$YearsExperience, log(sal_data$Salary))#cor=0.9157747

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(sal_data$Salary) ~ sal_data$YearsExperience + I(sal_data$YearsExperience*sal_data$YearsExperience))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = sal_data$sal_data$Salary - expy

sqrt(sum(err^2)/nrow(sal_data))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = sal_data, aes(x = sal_data$YearsExperience + I(sal_data$YearsExperience^2), y = log(sal_data$Salary))) + 
     geom_point(color='blue') +
     geom_line(color='red',data = sal_data, aes(x=sal_data$YearsExperience+I(sal_data$YearsExperience^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(sal_data$Salary)~sal_data$YearsExperience + I(sal_data$YearsExperience*sal_data$YearsExperience) + I(sal_data$YearsExperience*sal_data$YearsExperience*sal_data$YearsExperience))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = sal_data, aes(x = sal_data$YearsExperience + I(sal_data$YearsExperience^2) + I(sal_data$YearsExperience^3), y = sal_data$Salary)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = sal_data, aes(x=sal_data$YearsExperience+I(sal_data$YearsExperience^2)+I(sal_data$YearsExperience^3), y=expy3))

################################
