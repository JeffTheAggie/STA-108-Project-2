#STA 108 Project 2

#Part I
Model1_X1 = CDI$`Total population
stem(CDI$`Total population`)
Model1_X2 = CDI$`Land area`
stem(CDI$`Land area`)
Model1_X3 = CDI$`Total personal income`
stem(CDI$`Total personal income`)
Model2_X1 = CDI$`Population Density`
stem(CDI$`Population Density`)
Model2_X2 = CDI$`Percent of population 65 or older`
stem(CDI$`Percent of population 65 or older`)
Model2_X3 = CDI$`Total personal income`
stem(CDI$`Total personal income`)
data1 = CDI[,c(8,5,4,16)]
colnames(data1) = c("Y","Model1_X1", "Model1_X2", "Model1_X3")
pairs(data1)
cor(data1)
data2 = CDI[,c(8,18,7,16)]
colnames(data2) = c("Y", "Model2_X1", "Model2_X2", "Model2_X3")
pairs(data2)
cor(data2)
Model1_fit = lm(Y~Model1_X1+Model1_X2+Model1_X3, data = CDI)
Model2_fit = lm(Y~Model2_X1+Model2_X2+Model2_X3, data = CDI)
summary(Model1_fit) 
summary(Model2_fit)$coefficients
Model1_residuals = Model1_fit$residuals
plot(Model1_X1, Model1_residuals)
abline(h=0, col = 'red')
plot(Model1_X2, Model1_residuals)
abline(h=0, col = 'red')
plot(Model1_X3, Model1_residuals)
abline(h=0, col = 'red')
Y_hat1 = Model1_fit$fitted.values
plot(Y_hat1, Model1_residuals)
abline(h=0, col = 'red')
plot(Model1_X1*Model1_X2, Model1_residuals)
abline(h=0, col = 'red')
plot(Model1_X1*Model1_X3, Model1_residuals)
abline(h=0, col = 'red')
plot(Model1_X2*Model1_X3, Model1_residuals)
abline(h=0, col = 'red')
qqnorm(Model1_residuals)
qqline(Model1_residuals, col = "red")
Model2_residuals = Model2_fit$residuals
plot(Model2_X1, Model2_residuals)
abline(h=0, col = 'red')
plot(Model2_X2, Model2_residuals)
abline(h=0, col = 'red')
plot(Model2_X3, Model2_residuals)
abline(h=0, col = 'red')
Y_hat2 = Model2_fit$fitted.values
plot(Y_hat2, Model2_residuals)
abline(h=0, col = 'red')
plot(Model2_X1*Model2_X2, Model2_residuals)
abline(h=0, col = 'red')
plot(Model2_X1*Model2_X3, Model2_residuals)
abline(h=0, col = 'red')
plot(Model2_X2*Model2_X3, Model2_residuals)
abline(h=0, col = 'red')
qqnorm(Model2_residuals)
qqline(Model2_residuals, col = "red")
Model1_fit_int = lm(Y~Model1_X1+Model1_X2+Model1_X3+Model1_X1*Model1_X2+Model1_X2*Model1_X3+Model1_X2*Model1_X3, data = CDI)
summary(Model1_fit_int)$r.squared
Model2_fit_int = lm(Y~Model2_X1+Model2_X2+Model2_X3+Model2_X1*Model2_X2+Model2_X2*Model2_X3+Model2_X2*Model2_X3, data = CDI)
summary(Model2_fit_int)$r.squared

#Part II
fit = lm(V8 ~ V5+V16, data=CDI) #v8=Y=Number of active physicians V5=X1=Total population V16=X2=Total personal income
anova(fit)
model.before = lm(V8 ~ V5+V16, data=CDI)
model.afterX3 = lm(V8 ~ V5+V16+V4, data=CDI)
SSE.before = sum(model.before$residuals^2)
SSE.afterX3 = sum(model.afterX3$residuals^2)
X3extra.SS = SSE.before - SSE.afterX3
X3extra.SS
X3partial.R2 = (SSE.before - SSE.afterX3)/(SSE.before)
X3partial.R2
model.before = lm(V8 ~ V5+V16, data=CDI)
model.afterX4 = lm(V8 ~ V5+V16+V7, data=CDI)
SSE.before = sum(model.before$residuals^2)
SSE.afterX4 = sum(model.afterX4$residuals^2)
X4extra.SS = SSE.before - SSE.afterX4
X4extra.SS
X4partial.R2 = (SSE.before - SSE.afterX4)/(SSE.before)
X4partial.R2

model.before = lm(V8 ~ V5+V16, data=CDI)
model.afterX5 = lm(V8 ~ V5+V16+V9, data=CDI)
SSE.before = sum(model.before$residuals^2)
SSE.afterX5 = sum(model.afterX5$residuals^2)
X5extra.SS = SSE.before - SSE.afterX5
X5extra.SS
X5partial.R2 = (SSE.before - SSE.afterX5)/(SSE.before)
X5partial.R2
partialR2.X3X4X5=c(X3partial.R2,X4partial.R2,X5partial.R2)
rank(partialR2.X3X4X5)
extraSS.X3X4X5=c(X3extra.SS,X4extra.SS,X5extra.SS)
rank(extraSS.X3X4X5)
anova(model.before, model.afterX5)
model.before = lm(V8 ~ V5+V16, data=CDI)
model.afterX3X4 = lm(V8 ~ V5+V16+V4+V7, data=CDI)
SSE.before = sum(model.before$residuals^2)
SSE.afterX3X4 = sum(model.afterX3X4$residuals^2)
X3X4extra.SS = SSE.before - SSE.afterX3X4
X3X4extra.SS
X3X4partial.R2 = (SSE.before - SSE.afterX3X4)/(SSE.before)
X3X4partial.R2
model.before = lm(V8 ~ V5+V16, data=CDI)
model.afterX3X5 = lm(V8 ~ V5+V16+V4+V9, data=CDI)
SSE.before = sum(model.before$residuals^2)
SSE.afterX3X5 = sum(model.afterX3X5$residuals^2)
X3X5extra.SS = SSE.before - SSE.afterX3X5
X3X5extra.SS
X3X5partial.R2 = (SSE.before - SSE.afterX3X5)/(SSE.before)
X3X5partial.R2
model.before = lm(V8 ~ V5+V16, data=CDI)
model.afterX4X5 = lm(V8 ~ V5+V16+V7+V9, data=CDI)
SSE.before = sum(model.before$residuals^2)
SSE.afterX4X5 = sum(model.afterX4X5$residuals^2)
X4X5extra.SS = SSE.before - SSE.afterX4X5
X4X5extra.SS
X4X5partial.R2 = (SSE.before - SSE.afterX4X5)/(SSE.before)
X4X5partial.R2
partialR2.X3X4.X3X5.X4X5=c(X3X4partial.R2,X3X5partial.R2,X4X5partial.R2)
rank(partialR2.X3X4.X3X5.X4X5)
extraSS.X3X4.X3X5.X4X5=c(X3X4extra.SS,X3X5extra.SS,X4X5extra.SS)
rank(extraSS.X3X4.X3X5.X4X5)
anova(model.before, model.afterX4X5)

