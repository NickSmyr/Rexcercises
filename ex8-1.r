data <- read.csv("survey_data_2019.txt" , header=TRUE , sep='\t')
data <- data[complete.cases(data[,"height"]),]
# We are going to use the variable X1bar - X2bar (x1bar avg value of males 
#, x2bar avg value of females)
males <- data[data$sex == 'M',]$height
females <- data[data$sex == 'F',]$height

hist(males)
hist(females)
#No outliers and the distr looks normal enough

#Calculating sample size
nrow(data[data$sex == 'M',]) -> N1
nrow(data[data$sex == 'F',]) -> N2

#Calculating the variances of each male and female data
var(males) -> v1
var(females) -> v2

mean(males) -> m1
mean(females) -> m2

#Calculating the t for 95 % confidence interval
#Degrees of freedom are N1+N2 sp
abs(qt(0.025, df=min(N1-1,N2-1))) -> t

#Finally calculating the margin of erroR
mt <- (sqrt(v1/N1 + v2/N2)) * t

##Therefore the confidence interval is the following
lowerbound <- (m1-m2) - mt
higherbound <- (m1-m2) + mt
message("95% Confidence interval is [ ", lowerbound , " , " , higherbound, " ]")
