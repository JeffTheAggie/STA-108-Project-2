#STA 108 Project #2

#Part I (Multiple Linear Regression)

## 6.28

###(a) Prepare a stem-and-Ieaf plot for each ofthe predictor variables. What noteworthy 
###    information is provided by your plots? 
Y = CDI$V8
stem(Y)
#Model I
Model1_X1 = CDI$V5
stem(Model1_X1)
Model1_X2 = CDI$V4
stem(Model1_X2)
Model1_X3 = CDI$V16
stem(Model1_X3)
#Model II
Model2_X1 = Model1_X1/Model1_X2
stem(Model2_X1)
Model2_X2 = CDI$V7
stem(Model2_X2)
Model2_X3 = Model1_X3
stem(Model2_X3)

###(b) Obtain the scatter plot matrix and the correlation matrix for each 
###    proposed model. Summarize the information provided. 
#Scatterplot Matrix
##Model I
data1 = CDI[,c(8,4:5,16)]
colnames(data1) = c("Y","Model1_X1", "Model1_X2", "Model1_X3")
pairs(data1)
#Correlation Matrix
cor(data1)
#Scatterplot Matrix
##Model II
data2 = CDI[,c(8,5/4,7,16)]
colnames(data2) = c("Y", "Model2_X1", "Model2_X2", "Model2_X3")
pairs(data2)
#Correlation Matrix
cor(data2)

###(c) For each proposed model, fit the first-order regression model (6.5) 
###    with three predictor variables.
#Fitting Model I
Model1_fit = lm(Y~Model1_X1+Model1_X2+Model1_X3, data = data1)
#Fitting Model II
Model2_fit = lm(Y~Model2_X1+Model2_X2+Model2_X3, data = data2)

###(d) Calculate R2 for each model. Is one model clearly preferable in terms of this measure?
summary(Model1_fit)$r.squared 
summary(Model2_fit)$r.squared 
#Model 2 is better since it's closer to 1

###(e) For each model, obtain the residuals and plot them against Y, 
###    each of the three predictor variables, 
###    and each of the two-factor interaction terms. 
###    Also prepare a normal probability plot for each of the two fitted models. 
###    Interpret your plots and state your findings. Is one model 
###    clearly preferable in terms of appropriateness? 
#Model I Residuals
Model1_residuals = Model1_fit$residuals
plot(data1$Model1_X1 * Model1_X2 * Model1_X3, Model1_residuals)
#Model II Residuals 
Model2_residuals = Model2_fit$residuals
plot(data2$Model2_X1 * data2$Model2_X2 * data2$Model2_X3, Model2_residuals)

###(f) Now  expand  both  models  proposed  above  by  adding  all  possible  
###    two-factor  in-teractions.  Note that, for a model withX1, X2, X3as 
###    the predictors, the two-factor interactions areX1X2, X1X3, X2X3.  
###    Repeat part d for the two expanded models.
#Model I Combinations for Interactions
#Fitting Model I
Model1_fit_int = lm(Y~Model1_X1+Model1_X2+Model1_X3+Model1_X1*Model1_X2+Model1_X2*Model1_X3+Model1_X2*Model1_X3, data = data1)
summary(Model1_fit_int)$r.squared 

#Model II Combinations for Interactions
#Fitting Model II
Model2_fit_int = lm(Y~Model2_X1+Model2_X2+Model2_X3+Model2_X1*Model2_X2+Model2_X2*Model2_X3+Model2_X2*Model2_X3, data = data2)
summary(Model2_fit_int)$r.squared 