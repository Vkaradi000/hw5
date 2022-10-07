Victoria Karadimas 

Lab 4 

Safinaz Ali And John Robinson


 load("/Users/victoria/Desktop/B2000/acs2017_ny/acs2017_ny_data.RData")
 attach(acs2017_ny)
 use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
 dat_use <- subset(acs2017_ny,use_varb) # 
 detach()
 attach(dat_use)
 summary(dat_use)


model_temp1 <- lm(INCWAGE ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg)
 summary(model_temp1)
lm(formula = INCWAGE ~ AGE + female + AfAm + Asian + Amindian + 
       race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + 
       educ_advdeg)

> plot(model_temp1)


NNobs <- length(INCWAGE)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.1) 
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)


to_be_predicted2 <- data.frame(AGE = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)

detach()

My group decided to test out if veteran status would affect income. Typically veterans have a higher paying salary

model_temp2 <- lm(INCWAGE ~ VETSTAT)
summary(model_temp2)
Call:
  lm(formula = INCWAGE ~ VETSTAT)

Residuals:
  Min     1Q Median     3Q    Max 
-72514 -40514 -20514  12486 566766 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    73793       2402  30.727   <2e-16 ***
  VETSTAT        -1279       2305  -0.555    0.579    
---
  Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 83250 on 46969 degrees of freedom
Multiple R-squared:  6.559e-06,	Adjusted R-squared:  -1.473e-05 
F-statistic: 0.3081 on 1 and 46969 DF,  p-value: 0.5789


plot(model_temp2)
stargazer(model_temp2, type = "text")
NNobs <- length(INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs))


plot(INCWAGE ~ jitter(AGE, factor = 15), pch = 20, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = data_graph)
plot(INCWAGE ~ jitter(AGE, factor = 6), pch = 20, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = data_graph)


to_be_predicted2 <- data.frame(AGE = 25:55, veteran = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)

detach()

After reviewing, it seems that the derivative of the regression line is negative. Which means that Veteran status income was lower than the average income.
If this data was extended to the rest of the US, I still believe that Vet status would affect income