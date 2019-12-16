data <- read.csv("survey_data_2019.txt" , header=TRUE , sep='\t')
data <- data[complete.cases(data[,"prob"]),]
# We are going to use the variable X1bar - X2bar (x1bar avg value of males , x2bar avg value of females)
males <- data[data$sex == 'M',]$prob
females <- data[data$sex == 'F',]$prob

#Examining the data
hist(males)
hist(females)
#No outliers and the distr looks normal enough

#Calculating sample size
nrow(data[data$sex == 'M',]) -> N1
nrow(data[data$sex == 'F',]) -> N2

#Calculating the sdeviastions of each male and female data
sd(males) -> s1
sd(females) -> s2

#sample sd
s <- s1/N1 + s2/N2

mean(males) -> m1
mean(females) -> m2
#Null hypothesis is avg(males) == avg(females)
#Alternative hypothesis is avg(males) > avg(females)

#estimated value
m <- m1-m2
#hypothesized value
h <- 0
# Statistic  t test
t <-  ( m - 0 ) / s
# P - value ( with t test using Student t probabiliy function )
# and one sided significane test ( so input value is t)
pval <- pt(df= N1+N2 , -abs(t))
#pval is 1.311082e-07 os the null hypothesis is rejected
