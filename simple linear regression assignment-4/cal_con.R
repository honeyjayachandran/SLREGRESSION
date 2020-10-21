# Load cal_con.csv dataset
library(readr)
cal_con <- read_csv("C:/Users/Sony/Downloads/simple linear regression assignment-4/calories_consumed.csv")
View(cal_con)

# Exploratory data analysis
summary(cal_con)

#Scatter plot
plot(cal_con$`Calories Consumed`, cal_con$`Weight gained (grams)`)  # plot(X,Y)

?plot

attach(cal_con)


#Correlation Coefficient (r)
cor(cal_con$`Calories Consumed`, cal_con$`Weight gained (grams)`)    # cor(X,Y)=0.946991

# Simple Linear Regression model
reg <- lm(cal_con$`Weight gained (grams)` ~ cal_con$`Calories Consumed`) # lm(Y ~ X)

summary(reg) #multiple r sqred =103.3025

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(cal_con))  #RMSE=103.3025

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = cal_con, aes(x = cal_con$`Calories Consumed`, y = cal_con$`Weight gained (grams)`))+ 
     geom_point(color='blue') +
     geom_line(color='red',data = cal_con, aes(x=cal_con$`Calories Consumed` , y=pred))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(cal_con,aes(Calories Consumed,AT))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logrithamic Model

# x = log(cal_con$`Calories Consumed`); y = cal_con$`Weight gained (grams)`

plot(log(cal_con$`Calories Consumed`), cal_con$`Weight gained (grams)`)
cor(log(cal_con$`Calories Consumed`), cal_con$`Weight gained (grams)`)  #correlation=0.8987253

reg_log <- lm(cal_con$`Weight gained (grams)`~ log(cal_con$`Calories Consumed`))   # lm(Y ~ X)

summary(reg_log)  #multiple rsqred=0.8077
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(cal_con))  #RMSE=141.0054

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = cal_con$`Calories Consumed` and y = log(cal_con$`Weight gained (grams)`)

plot(cal_con$`Calories Consumed`, log(cal_con$`Weight gained (grams)`))

cor(cal_con$`Calories Consumed`, log(cal_con$`Weight gained (grams)`))#cor=0.9368037

reg_exp <- lm(log(cal_con$`Weight gained (grams)`) ~ cal_con$`Calories Consumed`)  #lm(log(Y) ~ X)

summary(reg_exp)  #multiple rsqurt value=0.8776

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logcal<- predict(reg_exp)
cal <- exp(logcal)

error = cal_con$`Weight gained (grams)`- cal 
error

sqrt(sum(error^2)/nrow(cal_con))  #RMSE=118.0452

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
 