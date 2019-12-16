data <- read.csv("survey_data_2019.txt" , header=TRUE , sep='\t')
data <- data[complete.cases(data[,c("math","prob")]),]
# We are going to use the variable X1bar - X2bar (x1bar avg value of males 
#, x2bar avg value of females)
math <- data[data$sex == 'M',]$math
prob <- data[data$sex == 'F',]$prob

#Examining the data
hist(math)
hist(prob)
#No outliers and the distr looks normal enough

#Calculating sample size
nrow(data) -> N

#Calculating the sdeviastions of each data
sd(math) -> s1
sd(prob) -> s2

#sample sd
s <- s1/sqrt(N) + s2/sqrt(N)

mean(math) -> m1
mean(prob) -> m2
#Null hypothesis is avg(males) == avg(females)
#Alternative hypothesis is avg(males) > avg(females)

#estimated value
m <- m1-m2
#hypothesized value
h <- 0
# Statistic  t test
t <-  ( m - 0 ) / s
# P - value ( with t test using Student t probabiliy function )
# and two sided significance test 
pval <- 2*pt(df= N - 1 , -abs(t))
#pval is 1.311082e-07 os the null hypothesis is rejected
message("Pvalue = ", pval)
