# Load dv_time.csv ddv_time$`Delivery Time`aset
library(readr)
dv_time <- read_csv("C:/Users/Sony/Downloads/simple linear regression assignment-4/delivery_time.csv")
View(dv_time)

# Explordv_time$`Delivery Time`ory ddv_time$`Delivery Time`a analysis
summary(dv_time)

#Scdv_time$`Delivery Time`ter plot
plot(dv_time$`Sorting Time`, dv_time$`Delivery Time`)  # plot(X,Y)

?plot

attach(dv_time)


#Correldv_time$`Delivery Time`ion Coefficient (r)
cor(dv_time$`Sorting Time`, dv_time$`Delivery Time`)     # cor(X,Y)=0.8259973

# Simple Linear Regression model
reg <- lm(dv_time$`Delivery Time` ~ dv_time$`Sorting Time`) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)   #multiple r sqred=0.6823

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(dv_time))  #RMSE=2.79165

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for ddv_time$`Delivery Time`a
library(ggplot2)

?ggplot2

ggplot(data = dv_time, aes(x = dv_time$`Sorting Time`, y = dv_time$`Delivery Time`)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = dv_time, aes(x=dv_time$`Sorting Time`, y=pred))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(dv_time,aes(dv_time$`Sorting Time`,dv_time$`Delivery Time`))+stdv_time$`Delivery Time`_summary(fun.ddv_time$`Delivery Time`a=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logrithamic Model

# x = log(dv_time$`Sorting Time`); y = dv_time$`Delivery Time`

plot(log(dv_time$`Sorting Time`), dv_time$`Delivery Time`)
cor(log(dv_time$`Sorting Time`), dv_time$`Delivery Time`)  #cor=0.8339325

reg_log <- lm(dv_time$`Delivery Time` ~ log(dv_time$`Sorting Time`))   # lm(Y ~ X)

summary(reg_log)  #multiple r squred=0.6954
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(dv_time))  #RMSE=0.2733171

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = dv_time$`Sorting Time` and y = log(dv_time$`Delivery Time`)

plot(dv_time$`Sorting Time`, log(dv_time$`Delivery Time`))

cor(dv_time$`Sorting Time`, log(dv_time$`Delivery Time`))  #correlation=0.8431773

reg_exp <- lm(log(dv_time$`Delivery Time`) ~ dv_time$`Sorting Time`)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logdv <- predict(reg_exp)
dv <- exp(logdv)

error = dv_time$`Delivery Time` - dv
error

sqrt(sum(error^2)/nrow(dv_time))  #RMSE=2.94025

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
 
     

################################
