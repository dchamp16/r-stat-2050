##### Code for Week 2

## Reading in example
d1.student =  read.csv("https://www.lock5stat.com/datasets3e/StudentSurvey.csv")


## Some basic R commands


# Extra Examples
d1.student = read.csv("https://www.lock5stat.com/datasets3e/StudentSurvey.csv")

table(d1.student$BirthOrder)

pie(table(d1.student$Sex), main = "Pie chart")
pie(table(d1.student$BirthOrder))

barplot(table(d1.student$Sex))
barplot(table(d1.student$BirthOrder))


barplot(table(d1.student$Sex, d1.student$BirthOrder), beside=FALSE, legend = TRUE)
barplot(table(d1.student$BirthOrder, d1.student$Sex), beside=FALSE, legend = TRUE)

barplot(table(d1.student$Smoke, d1.student$BirthOrder), beside=TRUE, legend = TRUE)
barplot(table(d1.student$BirthOrder, d1.student$Smoke), beside=TRUE, legend = TRUE)

barplot(table(d1.student$Piercings, d1.student$BirthOrder), beside=TRUE, legend = TRUE)
barplot(table(d1.student$BirthOrder, d1.student$Piercings), beside=TRUE, legend = TRUE)

barplot(table(d1.student$Piercings, d1.student$Sex), beside=TRUE, legend = TRUE)
barplot(table(d1.student$Sex, d1.student$Piercings), beside=TRUE, legend = TRUE)


# day 2
install.packages("epiDisplay")
library(epiDisplay)
set.seed(2040)
d2.smallData = floor(runif(10, 95, 105))
sort(d2.smallData)

d2.mam = read.csv("https://www.lock5stat.com/datasets3e/MammalLongevity.csv")
d2.student =  read.csv("https://www.lock5stat.com/datasets3e/StudentSurvey.csv")

stem(d2.smallData, scale = .5)
dotplot(d2.smallData, bin = 9)

d2.bigData = floor(runif(1000, 0, 100))
stem(d2.bigData, scale = .5)
dotplot(d2.bigData, bin = 99)


hist(d2.smallData, xlab = "", main = "", 
          breaks = c(96, 97.9, 99.9, 101.9, 103.9, 105.9))
plot(density(d2.smallData), xlab = "", main = "")

hist(d2.mam$Longevity, main = "a", xlab = "Longevity")
hist(d2.student$Pulse, main = "b", xlab = "Student Pulse")
hist(d2.student$Piercings, main = "c", xlab = "Student Piercings")
hist(d2.student$Exercise, main = "d", xlab = "Student Exercise")

hist(rnorm(1e5))
     
plot(density(rnorm(1e5)), main = "Symmetric/Bell Curve", xlab = "")

plot(density(rchisq(1e5, 15)), main = "Right Skewed", xlab = "")
plot(density(-rchisq(1e5, 15)), main = "Left Skewed", xlab = "")
plot(density(c(rchisq(1e5, 15), rnorm(1e5, mean = 45, sd = 7))), 
     main = "Bimodal - not symmetric", xlab = "")

## Different numbers of bins
# seq - sequence function. Creates a sequence of numbers from the first, to the second,
#       of the length specified. 

hist(d2.mam$Longevity, main = "5", xlab = "Longevity", 
     breaks = seq(0, 40, length = 5))
hist(d2.mam$Longevity, main = "10", xlab = "Longevity", 
     breaks = seq(0, 40, length = 10))
hist(d2.mam$Longevity, main = "15", xlab = "Longevity", 
     breaks = seq(0, 40, length = 15))
hist(d2.mam$Longevity, main = "20", xlab = "Longevity", 
      breaks = seq(0, 40, length = 20))

###############
### Day 3
set.seed(2040)
d3.small = sample(1:100, 10)
d3.small.s = sort(d3.small)

d3.student =  read.csv("https://www.lock5stat.com/datasets3e/StudentSurvey.csv")

gpa = d3.student$GPA

# n
length(gpa) ## missing values
sum(!is.na(gpa))

# mean
sum(gpa, na.rm = TRUE)
mean(gpa, na.rm = TRUE)

# median
median(gpa, na.rm = TRUE)

# mode
tempTable = table(gpa)
max(tempTable)
tempTable[which(tempTable == max(tempTable))]
tempTable[which(tempTable > 20)]

table(tempTable)


mean(gpa, na.rm = TRUE)
mean(c(0, gpa), na.rm = TRUE)

median(gpa, na.rm = TRUE)
median(c(0, gpa), na.rm = TRUE)


# histograms - look at shape, median, and mean
hist(gpa, breaks = seq(2, 4, by = .1))
abline(v = median(gpa, na.rm=TRUE), lty = 1)
abline(v = mean(gpa, na.rm=TRUE), lty = 4)
legend("topright", legend = c("Median", "Mean"), lty = c(1,4))

sWeight = d3.student$Weight
hist(sWeight)
abline(v = median(sWeight, na.rm=TRUE), lty = 1)
abline(v = mean(sWeight, na.rm=TRUE), lty = 4)
legend("topright", legend = c("Median", "Mean"), lty = c(1,4))

