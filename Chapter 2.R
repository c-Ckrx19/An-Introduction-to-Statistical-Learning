# Ex8
rm(list=ls())

# (a)
college = read.csv("College.csv")

# (b)
fix(college)
rownames(college) = college[,1]
fix(college)
college = college[,-1]
fix(college)

# (c)
# (i)
summary(college)

# (ii)
pairs(college[,1:10])

# (iii)
plot(college$Outstate, college$Private)

# (iv)
Elite = rep("No", nrow=(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)
plot(college$Outstate, college$Elite)

# (v)
par(mfrow=c(2,2))
hist(college$Apps)
hist(college$Top10perc)
hist(college$F.Undergrad)
hist(college$S.F.Ratio)

# (vi)
par(mfrow=c(1,1))
plot(college$perc.alumni, college$Private)
# On average, private schools have higher percentages of alumni who 
# donate than those of non-private schools.
plot(college$PhD, college$Grad.Rate)
# The percentage of faculty with Ph.D.'s is correlated with the 
# graduation rate.


# Ex9
rm(list=ls())
Auto = read.csv("Auto.csv")
Auto = na.omit(Auto)

# (a)
names(Auto)
# Quantitative: mpg, cylinders, displacement, weight, 
#               year, acceleration
# Qualitative: horsepower, origin

# (b)
range(Auto$mpg)
# [9.0, 46.6]
range(Auto$cylinders)
# [3, 8]
range(Auto$displacement)
# [68, 455]
range(Auto$weight)
# [1613, 5140]
range(Auto$year)
# [70, 82]
range(Auto$acceleration)
# [8.0, 24.8]

# (c)
mean(Auto$mpg)
sd(Auto$mpg)
# mean: 23.51587, sd: 7.825804
mean(Auto$cylinders)
sd(Auto$cylinders)
# mean: , sd: 
mean(Auto$displacement)
sd(Auto$displacement)
# mean: 5.458438, sd: 1.701577
mean(Auto$weight)
sd(Auto$weight)
# mean: 2970.262, sd: 847.9041
mean(Auto$year)
sd(Auto$year)
# mean: 75.99496, sd: 3.690005
mean(Auto$acceleration)
sd(Auto$acceleration)
# mean: 15.55567, sd: 2.749995

# (d)
new_Auto = Auto[-(10:85),]
range(new_Auto$mpg)
mean(new_Auto$mpg)
sd(new_Auto$mpg)
# range: [11.0, 46.6], mean: 24.43863, sd: 7.908184
range(new_Auto$cylinders)
mean(new_Auto$cylinders)
sd(new_Auto$cylinders)
# range: [3, 8], mean: 5.370717, sd: 1.653486
range(new_Auto$displacement)
mean(new_Auto$displacement)
sd(new_Auto$displacement)
# range: [68, 455], mean: 187.0498, sd: 99.63539
range(new_Auto$weight)
mean(new_Auto$weight)
sd(new_Auto$weight)
# range: [1649, 4997], mean: 2933.963, sd: 810.6429
range(new_Auto$year)
mean(new_Auto$year)
sd(new_Auto$year)
# range: [70, 82], mean: 77.15265, sd: 3.11123
range(new_Auto$acceleration)
mean(new_Auto$acceleration)
sd(new_Auto$acceleration)
# range: [8.5, 24.8], mean: 15.72305, sd: 2.680514

# (e)
plot(Auto$cylinders, Auto$displacement)
# More cylinders, more displacement
plot(Auto$cylinders, Auto$mpg)
# Mpg decreases as number of cylinders increases

# (f)
pairs(Auto)
# All of the predictors have some relationship with mpg. However, 
# having a close look to the data, we find that the number of 
# observations for each name is too small that including this 
# predictor may cause overfitting


# Ex10
rm(list=ls())

# (a)
library(MASS)
Boston
?Boston
# 506 rows, 14 columns
# Each row represents a house value and each column represents 
# a feature in different suburbs

# (b)
pairs(Boston)
# Predictors correlating with crime rate: nox, age, dis, rad, tax, 
# ptratio, lstat, medv

# (c)
plot(Boston$age, Boston$crim)
# Older houses, higher crime rate
plot(Boston$dis, Boston$crim)
# Longer distance to employment centres, lower crime rate
plot(Boston$rad, Boston$crim)
# Easier access to radial highways, higher crime rate
plot(Boston$tax, Boston$crim)
# Higher tax rate, higher crime rate
plot(Boston$ptratio, Boston$crim)
# Higher pupil-teacher ratio, higher crime rate

# (d)
hist(Boston$crim[Boston$crim>1], breaks = 30)
length(which(Boston$crim > 40))
range(Boston$crim)
# There are 6 suburbs whose crim rate is greater than 40 per capita 
# with the highest one reaching 88.98 per capita
hist(Boston$tax, breaks = 20)
# The peak of the tax rate can be found between 660/10000 
# and 680/10000
hist(Boston$ptratio)
# There is a skew towards a higher ratio, but there is no suburb have 
# particularly high pupil-teacher ratio since the highest ratio is 
# 22%

# (e)
length(which(Boston$chas == 1))
# 35 suburbs

# (f)
summary(Boston$ptratio)
# The median is 19.05.

# (g)
subset(Boston, Boston$medv == min(Boston$medv))
summary(Boston)
#          306       409 
# crim     38.3518   67.9208    Both are above 3rd quartile
# zn       0         0          The same as the median
# indus    18.1      18.1       Both are at 3rd quartile
# chas     0         0          Not bounded by the river
# nox      0.693     0.693      Higher than 3rd quartile
# rm       5.453     5.683      Below the 1st quartile
# age      100       100        The same as the maxium
# dis      1.4896    1.4254     Below the 1st quartile
# rad      24        24         At maximum
# tax      666.0     666.0      At maximum
# ptratio  20.2      20.2       At 3rd quartile
# black    396.9     384.97     At maximum; below the median but above
#                               the 1st quartile
# lstat    30.59     22.98      Above 3rd quartile
# medv     5         5          At minimum
# Although these two suberbs are not the worst for each and
# every predictor, they are not nice suberbs to live.

# (h)
length(which(Boston$rm > 7))
# 64 
length(which(Boston$rm > 8))
# 13
summary(subset(Boston, Boston$rm > 8))
summary(Boston)
# Significantly lower crime rate, lower lstat and higher median value
# of owner-occupied homes