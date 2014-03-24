library(ggplot2)

require(devEMF)
emf('goog.emf')

data <- read.csv("http://www.google.com/finance/historical?q=GOOG&startdate=Aug+19%2C+2004&enddate=Mar+1%2C+2013&output=csv ", sep=",", header=1)

# reverse data rows
goog = data[nrow(data):1, ]

print(names(data))
print(names(data)[c(2,5)])
print(goog[, c(2, 5)])
#print(goog)

write.csv(goog, file = "goog.csv")
write.table(goog, file = "goog.tab")

#axis = (1, lab = aapl[2])
#axis = (2, aapl[2])
#box()

# open value
#plot(aapl[,1], aapl[,5], xlab = "TIME", ylab = "PRICE ($)", type = "l", col="blue")
plot(as.numeric(goog[,2]), xlab = "DAY", ylab = "STOCK VALUE ($)", lty = 1, col="blue")
#plot(goog[,2], xlab = "DAY", ylab = "STOCK VALUE ($)", type = "l", col="blue")

# close value
#lines(as.numeric(goog[,5]), col="red")
#lines(goog[,5], col="red")
lines(goog[,5], type = "l", col="red")
#lines(goog[,2], type = "l", col="red")
#lines(goog[,3], type = "l", col="green")

#legend("topleft", names(data)[c(2,5)], fill = heat.colors(2))
#legend("topleft", names(data)[c(2,3,5)], lty = 1:3, col = c('blue', 'red', 'green'))
legend("topleft", names(data)[c(2,5)], lty = 1:2, col = c('blue', 'red'))
#legend("topleft", names(data)[c(2,5)], lty = 1, col = c('red', 'blue'))

#matplot(aapl[,1], aapl[,5], type = "l", col="red")
#plot(aapl[c(1:40), 1], aapl[c(1:40),5], xlab = "TIME", ylab = "PRICE ($)", type = "l", col="blue")
#plot(aapl[c(1:40),5], xlab = "TIME", ylab = "PRICE ($)", type = "l", col="blue")

# simple example
#x <- c(1,3,6,9,12)
#y <- c(1.5,2,7,8,15)
#plot(x,y, pch=15, col="blue")

# Example 2. Draw a plot, set a bunch of parameters.
#plot(x,y, xlab="x axis", ylab="y axis", main="my plot", ylim=c(0,20), xlim=c(0,20), pch=15, col="blue")
# fit a line to the points
#myline.fit <- lm(y ~ x)

# get information about the fit
#summary(myline.fit)

# draw the fit line on the plot
#abline(myline.fit)

# add some more points to the graph
#x2 <- c(0.5, 3, 5, 8, 12)
#y2 <- c(0.8, 1, 2, 4, 6)

#points(x2, y2, pch=16, col="green")

# header = TRUE ignores the first line, check.names = FALSE allows '+' in 'C++'
#benchmark <- read.table("", header = TRUE, row.names = "vwnd", check.names = FALSE)

# 't()' is matrix tranposition, 'beside = TRUE' separates the benchmarks, 'heat' provides nice colors
#barplot(t(as.matrix(benchmark)), beside = TRUE, col = heat.colors(6))
#barplot(t(as.matrix(benchmark)), beside = TRUE, col = heat.colors(6), xlab = "DAY", ylab = "STOCK VALUE ($)")

# 'cex' stands for 'character expansion', 'bty' for 'box type' (we don't want borders)
#legend("topright", names(benchmark), cex = 0.9, bty = "n", fill = heat.colors(6))

