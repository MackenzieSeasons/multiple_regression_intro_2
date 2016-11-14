library(tidyverse)
my.data <- read_csv("regLectureData.csv")
glimpse(my.data)

library(apaTables)
apa.cor.table(my.data)
#always report a correlation matrix with predictors and criterion

#also always check for curvilinear relations
psych::pairs.panels(as.data.frame(my.data))
#looks good
#best predictor is iq based on the correlation matrix

#MULTIPLE REGRESSION ANALYSIS: OLD SCHOOL
#want to use age and IQ to predict vid score
#more specifically, combine age and IQ to create a new variable, y hat, that correlates as highly as possible with vid score
my.regression <- lm(VidScore ~ age + iq, data=my.data)
print(my.regression)
#y hat = 102.2333 - .371(age) + .329(iq)
summary(my.regression)
# R2 = .29, which means that age and IQ accounted for 29% of the variability, F = 40.45, p < .001

library(apaTables)
apa.reg.table(my.regression, filename = "myRegressionTable.doc")
#shows the same stuff with more!

#What is the estimated population mean video score for 43 y/olds with an IQ of 130? CI?
x_axis_range <- data.frame(age = c(43),iq=c(130))
CI_data <- predict(my.regression,
                   newdata = x_axis_range, interval = "confidence", level = 0.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
print(CI_data)
# the score we can predict fo the pop is 129, 95% CI [126, 131]

#What if we want the predicted score for a person who is 43 years old and has an IQ of 130? PI?
x_axis_range <- data.frame(age = c(43),iq=c(130))
PI_data <- predict(my.regression,
                   newdata = x_axis_range, interval = "prediction", level = 0.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
print(PI_data)
#score is 129, PI [110, 147]

#THE FOLLOWING IS THEORETICAL
#obtaining predicted vid scores (y hat)
predicted.values.VidScore <- predict(my.regression)
#then we correlated predicted scores (y hat) with actual vid scores (y)
bigR <- cor(predicted.values.VidScore, my.data$VidScore)
print(bigR)
bigR2 <- bigR * bigR
print(bigR2)
#R2 is the proportion variability in criterion scores (Y) accounted for by Y hat (i.e. iq and age)




# b weights - a 1 unit change in IQ results in subtracting .37 from Vid Score
# beta weights - a 1 unit standard deviation in IQ score results in subtracring .21 from Vid Score



