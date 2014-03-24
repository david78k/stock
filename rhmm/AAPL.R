library(quantmod)

getSymbols("AAPL")
chartSeries(AAPL)
 <- window(AAPL, start = as.Date("2012-01-01"))
 <- cbind($AAPL.Close - $AAPL.Open, $AAPL.Volume)

library(RHmm)
# Baum-Welch Algorithm
hm_model <- HMMFit(obs = , nStates = 5)

# Viterbi Algorithm
VitPath <- viterbi (hm_model, )

# scatter plot
 <- cbind($AAPL.Close, VitPath)
chartSeries([,1])
addTA([[,2]==1,1],on=1,type="p",col=5,pch=25)
addTA([[,2]==2,1],on=1,type="p",col=6,pch=24)
addTA([[,2]==3,1],on=1,type="p",col=7,pch=23)
addTA([[,2]==4,1],on=1,type="p",col=8,pch=22)
addTA([[,2]==5,1],on=1,type="p",col=10,pch=21)

