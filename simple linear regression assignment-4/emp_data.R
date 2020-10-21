# Load emp_data.csv dataset
library(readr)
emp_data <- read.csv("C:/Users/Sony/Downloads/simple linear regression assignment-4/emp_data.csv")
View(emp_data)

# Exploratory data analysis
summary(emp_data)

#Scatter plot
plot(emp_data$Salary_hike, emp_data$Churn_out_rate)  # plot(X,Y)

?plot

attach(emp_data)


#Correlation Coefficient (r)
cor(emp_data$Salary_hike, emp_data$Churn_out_rate)             # cor(X,Y)=-0.9117216

# Simple Linear Regression model
reg <- lm(emp_data$Churn_out_rate ~ emp_data$Salary_hike) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)  #multiple r sqred=0.8312

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp_data))  #RMSE=3.997528

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = emp_data, aes(x = emp_data$Salary_hike, y = emp_data$Churn_out_rate)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = emp_data, aes(x=emp_data$Salary_hike, y=pred))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(emp_data,aes(emp_data$Salary_hike,emp_data$Churn_out_rate))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logrithamic Model

# x = log(emp_data$Salary_hike); y = emp_data$Churn_out_rate

plot(log(emp_data$Salary_hike), emp_data$Churn_out_rate)
cor(log(emp_data$Salary_hike), emp_data$Churn_out_rate) #corr=-0.9212077

reg_log <- lm(emp_data$Churn_out_rate ~ log(emp_data$Salary_hike))   # lm(Y ~ X)

summary(reg_log)  #multiple rsqred=0.8486
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(emp_data))  #RMSE=3.786004

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = emp_data$Salary_hike and y = log(emp_data$Churn_out_rate)

plot(emp_data$Salary_hike, log(emp_data$Churn_out_rate))

cor(emp_data$Salary_hike, log(emp_data$Churn_out_rate)) #cor=-0.9346361

reg_exp <- lm(log(emp_data$Churn_out_rate) ~ emp_data$Salary_hike)  #lm(log(Y) ~ X)

summary(reg_exp) #multiple r sqred=0.8735

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logemp <- predict(reg_exp)
emp <- exp(logemp)

error = emp_data$Churn_out_rate - emp 
error

sqrt(sum(error^2)/nrow(emp_data))  #RMSE=3.541549

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(emp_data$Salary_hike, emp_data$Churn_out_rate)
plot(emp_data$Salary_hike*emp_data$Salary_hike, emp_data$Churn_out_rate)

cor(emp_data$Salary_hike*emp_data$Salary_hike, emp_data$Churn_out_rate) #corr=-0.9962051

plot(emp_data$Salary_hike*emp_data$Salary_hike, log(emp_data$Churn_out_rate))

cor(emp_data$Salary_hike, log(emp_data$Churn_out_rate))
cor(emp_data$Salary_hike*emp_data$Salary_hike, log(emp_data$Churn_out_rate)) #cro=-0.925803

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(emp_data$Churn_out_rate) ~ emp_data$Salary_hike + I(emp_data$Salary_hike*emp_data$Salary_hike))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = emp_data$emp_data$Churn_out_rate - expy

sqrt(sum(err^2)/nrow(emp_data))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = emp_data, aes(x = emp_data$Salary_hike + I(emp_data$Salary_hike^2), y = log(emp_data$Churn_out_rate))) + 
     geom_point(color='blue') +
     geom_line(color='red',data = emp_data, aes(x=emp_data$Salary_hike+I(emp_data$Salary_hike^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(emp_data$Churn_out_rate)~emp_data$Salary_hike + I(emp_data$Salary_hike*emp_data$Salary_hike) + I(emp_data$Salary_hike*emp_data$Salary_hike*emp_data$Salary_hike))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = emp_data, aes(x = emp_data$Salary_hike + I(emp_data$Salary_hike^2) + I(emp_data$Salary_hike^3), y = emp_data$Churn_out_rate)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = emp_data, aes(x=emp_data$Salary_hike+I(emp_data$Salary_hike^2)+I(emp_data$Salary_hike^3), y=expy3))

################################
