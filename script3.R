# Model Building 

plot(Freq ~ average.temperature, data=tripsperday)
abline(lm(Freq ~ average.temperature, data=tripsperday))

m1 <- lm(Freq ~ average.temperature, data=tripsperday)
m2 <- lm(Freq ~ average.temperature + precipitation, data=tripsperday)
m3 <- lm(Freq ~ average.temperature + precipitation + snow.fall, data=tripsperday)
m4 <- lm(Freq ~ average.temperature + precipitation + snow.fall + snow.depth, data=tripsperday)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

# Testing betaprecipitation=0 
m5 <- lm(Freq ~ average.temperature  + precipitation + snow.fall +snow.depth, data=tripsperday)
anova(m4,m5)

# Testing with complte data 

m6 <- lm(Freq ~ average.temperature + precipitation + snow.fall +snow.depth, data=tripsperday1)
plot(predict(m6), residuals(m6), xlab="Fitted", ylab="Residuals")

m7 <- lm(sqrt(Freq) ~ average.temperature  + precipitation + snow.fall +snow.depth, data=tripsperday1)
plot(predict(m7), residuals(m7), xlab="Fitted", ylab="Residuals")

# Testing without outliers 

m7 <- lm(Freq ~ average.temperature + precipitation + snow.fall +snow.depth, data=tripsperday1)
plot(predict(m7), residuals(m7), xlab="Fitted", ylab="Residuals")

m8 <- lm(sqrt(Freq) ~ average.temperature  + precipitation + snow.fall +snow.depth, data=tripsperday1)
plot(predict(m8), residuals(m8), xlab="Fitted", ylab="Residuals")



# Poisson model 

m9 <- glm(Freq ~ average.temperature  + precipitation + snow.fall +snow.depth, family = poisson, data=tripsperday1)
summary(m9)

# Overdispersion

halfnorm(residuals(m9))
plot(log(fitted(m9)), log((tripsperday1$Freq-fitted(m9))^2), xlab=expression(log(hat(mu))),
       ylab=expression(log((y-hat(mu))^2)))
abline(0,1)

m9 <- glm(Freq ~ average.temperature  + precipitation + snow.fall +snow.depth, family = poisson, data=tripsperday1)
summary(m9)

# Quasi poission 
m10 <- glm(Freq ~ average.temperature  + precipitation + snow.fall +snow.depth, family = quasipoisson(), data=tripsperday1)
summary(m9)

drop1(m10, test="F")
