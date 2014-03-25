library(quantmod)

require(devEMF)
#postscript('AAPL.eps')

getSymbols("AAPL")
chartSeries(AAPL, theme="white")
AAPL_Subset <- window(AAPL, start = as.Date("2012-01-01"))
AAPL_Train <- cbind(AAPL_Subset$AAPL.Close - AAPL_Subset$AAPL.Open, AAPL_Subset$AAPL.Volume)

library(RHmm)
# Baum-Welch Algorithm
hm_model <- HMMFit(obs = AAPL_Train, nStates = 5)

# Viterbi Algorithm
VitPath <- viterbi (hm_model, AAPL_Train)

# scatter plot
AAPL_Predict <- cbind(AAPL_Subset$AAPL.Close, VitPath$states)
chartSeries(AAPL_Predict[,1], theme="white.mono")
addTA(AAPL_Predict[AAPL_Predict[,2]==1,1],on=1,type="p",col=5,pch=25)
addTA(AAPL_Predict[AAPL_Predict[,2]==2,1],on=1,type="p",col=6,pch=24)
addTA(AAPL_Predict[AAPL_Predict[,2]==3,1],on=1,type="p",col=7,pch=23)
addTA(AAPL_Predict[AAPL_Predict[,2]==4,1],on=1,type="p",col=8,pch=22)
addTA(AAPL_Predict[AAPL_Predict[,2]==5,1],on=1,type="p",col=10,pch=21)

