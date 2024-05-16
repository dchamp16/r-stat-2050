##### Code for Week 3

# Day 1 - Histograms, variance

# Visualize impact of larger standard errors. 
hist(rnorm(1e5, mean = 10, sd = 1), xlab = "", main = "SD = 1", xlim = c(-10, 25))
hist(rnorm(1e5, mean = 10, sd = 2), xlab = "", main = "SD = 2", xlim = c(-10, 25))
hist(rnorm(1e5, mean = 10, sd = 4), xlab = "", main = "SD = 4", xlim = c(-10, 25))

tempData = read.csv("https://www.lock5stat.com/datasets3e/April14Temps.csv")
usStates = read.csv("https://www.lock5stat.com/datasets3e/USStates.csv")

set.seed(2040)
smallData = floor(runif(10, 40, 100))

sort(smallData)

## Variance
diff = smallData - mean(smallData)
diff^2
sum(diff^2)
sum(diff^2) / 9

min(smallData)
max(smallData)
max(smallData) - min(smallData)
n = length(smallData)
sum(smallData)
sum(smallData) / n
mean(smallData)

small.diff = smallData - mean(smallData)
small.diff
small.diff.sq = small.diff^2
small.diff.sq
small.diff.sq.sum = sum(small.diff.sq)
small.diff.sq.sum
small.var = small.diff.sq.sum / (n-1)
small.var

var(smallData)

small.sd = sqrt(small.var)
small.sd
sd(smallData)



## SF temps
plot(x = tempData$Year, y = tempData$SanFrancisco)
min(tempData$SanFrancisco)
max(tempData$SanFrancisco)
diff(range(tempData$SanFrancisco))
mean(tempData$SanFrancisco)
var(tempData$SanFrancisco)
sd(tempData$SanFrancisco)


### Day 2 - boxplots
set.seed(2040)
smallData = floor(runif(10, 40, 100))
medData = floor(runif(20, 40, 100))
medData2 = floor(runif(19, 40, 100))

sort(medData)
sort(medData2)

summary(medData, quantile.type = 5)
summary(medData2, quantile.type = 6)

boxplot(medData)
boxplot(medData, horizontal = TRUE)

## Temperatures
summary(tempData$SanFrancisco)
boxplot(tempData$SanFrancisco)


### IQ in US
summary(usStates$IQ)
boxplot(usStates$IQ)

nums <- c(41,42,49,54,57,59,65,66,67,74,78,81,84,86,86,89,94,96,97,97)
boxplot(nums)
summary(nums)
boxplot

# US States IQ
boxplot(IQ ~ Region, data = usStates)
boxplot(HouseholdIncome ~ Region, data = usStates)
boxplot(Population ~ Region, data = usStates)
boxplot(EighthGradeMath ~ Region, data = usStates)
boxplot(HighSchool ~ Region, data = usStates)
boxplot(College ~ Region, data = usStates)
boxplot(Smokers ~ Region, data = usStates)
boxplot(PhysicalActivity ~ Region, data = usStates)
boxplot(Obese ~ Region, data = usStates)
boxplot(HeavyDrinkers ~ Region, data = usStates)
boxplot(StudentSpending ~ Region, data = usStates)
boxplot(Insured ~ Region, data = usStates)



##outlier example

outData = c(0, floor(rnorm(19, 10, 2)))
boxplot(outData)