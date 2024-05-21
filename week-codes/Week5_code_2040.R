##### Code for Week 5

#localData =  read.csv("/Users/johnkidd/Documents/John/UVU/Classes/STAT 2040/2040 Ritems/Datasets/file.csv")
bb_money = read.csv("https://www.lock5stat.com/datasets3e/BaseballSalaries2019.csv")

### Random data
set.seed(2040)
bb_sample = sample(bb_money$Salary, 10)

## Generate many samples of 10 and take the mean
nSamps = 100000
samp_means = rep(0, nSamps)
for(i in 1:nSamps){
  samp_means[i] = mean(sample(bb_money$Salary, 10))
}

hist(samp_means, main = "n = 10", xlab = "Mean Salary")

## Find mean and SE. Compare to population
mean(bb_money$Salary)

mean(samp_means)
sd(samp_means)

### Try samples of increasing sizes
nSamps = 100000
samp_means_50 = rep(0, nSamps)
samp_means_100 = rep(0, nSamps)
samp_means_200 = rep(0, nSamps)
for(i in 1:nSamps){
  samp_means_50[i] = mean(sample(bb_money$Salary, 50))
  samp_means_100[i] = mean(sample(bb_money$Salary, 100))
  samp_means_200[i] = mean(sample(bb_money$Salary, 200))
}

hist(samp_means, main = "n = 10", xlab = "Mean Salary", xlim = c(0, 20))

mean(bb_money$Salary)

mean(samp_means)
sd(samp_means)

hist(samp_means_50, main = "n = 50", xlab = "Mean Salary", xlim = c(0, 20))
mean(samp_means_50)
sd(samp_means_50)

hist(samp_means_100, main = "n = 100", xlab = "Mean Salary", xlim = c(0, 20))
mean(samp_means_100)
sd(samp_means_100)

hist(samp_means_200, main = "n = 200", xlab = "Mean Salary", xlim = c(0, 20))
mean(samp_means_200)
sd(samp_means_200)

### Tuesday
# This code for demonstration only - 
set.seed(2040.2)
nSamps = 10000
n = 50
samp_means_check = rep(0, nSamps)
for(i in 1:nSamps){
  samp_means_check[i] = mean(sample(bb_money$Salary, n))
}
samp_SE = sd(samp_means_check)
allInts = cbind(samp_means_check - 2*samp_SE, samp_means_check + 2*samp_SE)
inInterval = mean(bb_money$Salary) > allInts[, 1] & mean(bb_money$Salary) < allInts[, 2] 

dotCol = c("red", "black")
dotplot(samp_means_check, bin = 50, by = inInterval, main = "", dot.col = dotCol)

## Wednesday
#### Using the normal distribution for proportions
qnorm((1 - .9)/2)
qnorm((1 - .95)/2)
qnorm((1 - .90)/2)
qnorm((1 - .99)/2)
qnorm((1 - .997)/2)
qnorm((1 - .8)/2)


28 - 4*qnorm((1 - .95)/2)
28 + 4*qnorm((1 - .95)/2)

5 - 2*qnorm((1 - .99)/2)
5 + 2*qnorm((1 - .99)/2)

-17 - 5*qnorm((1 - .90)/2)
-17 + 5*qnorm((1 - .90)/2)

14 - 2*qnorm((1 - .80)/2)
14 + 2*qnorm((1 - .80)/2)

105 - 10*qnorm((1 - .95)/2)
105 + 10*qnorm((1 - .95)/2)


#### Thursday - confidence intervals of means and t-distribution

### Comparing Normal to t-distribution
x = seq(-5, 5, length = 1e4)
normY = dnorm(x)
normT_2 = dt(x, 2)
normT_3 = dt(x, 3)
normT_4 = dt(x, 4)
normT_5 = dt(x, 5)
normT_10 = dt(x, 10)
normT_30 = dt(x, 30)

library(RColorBrewer)

plotCols = brewer.pal(7, "Dark2")
plot(x, normY, pch = 16, type="l", xlab = "X", ylab = "", main = "df = 2", 
        lwd = 3)
points(x, normT_2, type = "l", lwd = 3, lty = 2, col = plotCols[2])


plot(x, normY, pch = 16, type="l", xlab = "X", ylab = "", main = "df = 3", 
     lwd = 3)
points(x, normT_3, type = "l", lwd = 3, lty = 2, col = plotCols[3])


plot(x, normY, pch = 16, type="l", xlab = "X", ylab = "", main = "df = 4", 
     lwd = 3)
points(x, normT_4, type = "l", lwd = 3, lty = 2, col = plotCols[4])


plot(x, normY, pch = 16, type="l", xlab = "X", ylab = "", main = "df = 5", 
     lwd = 3)
points(x, normT_5, type = "l", lwd = 3, lty = 2, col = plotCols[5])


plot(x, normY, pch = 16, type="l", xlab = "X", ylab = "", main = "df = 10", 
     lwd = 3)
points(x, normT_10, type = "l", lwd = 3, lty = 2, col = plotCols[6])


plot(x, normY, pch = 16, type="l", xlab = "X", ylab = "", main = "df = 30", 
     lwd = 3)
points(x, normT_30, type = "l", lwd = 3, lty = 2, col = plotCols[7])


tStar = qt((1 - .95)/2, 200 - 1)
28.1 - tStar*2.5/sqrt(200)
28.1 + tStar*2.5/sqrt(200)

tStar = qt((1 - .90)/2, 100 - 1)
8.5 - tStar*1.4/sqrt(100)
8.5 + tStar*1.4/sqrt(100)

tStar = qt((1 - .99)/2, 50 - 1)
-2.9 - tStar*0.8/sqrt(50)
-2.9 + tStar*0.8/sqrt(50)


