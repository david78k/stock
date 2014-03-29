library(quantmod)

require(devEMF)
#postscript('AAPL-gaus.eps')

getSymbols("AAPL-gaus")
chartSeries(AAPL-gaus, theme="white")
AAPL-gaus_Subset <- window(AAPL-gaus, start = as.Date("2000-01-01"), end = as.Date("2013-03-01"))
AAPL-gaus_Train <- cbind(AAPL-gaus_Subset$AAPL-gaus.Close - AAPL-gaus_Subset$AAPL-gaus.Open, AAPL-gaus_Subset$AAPL-gaus.Volume)

library(RHmm)
# Baum-Welch Algorithm to find the model for the given observations
#hm_model <- HMMFit(obs = AAPL-gaus_Train, nStates = 5)
hm_model <- HMMFit(obs = AAPL-gaus_Train, nStates = 5, nMixt = 6, dis = "MIXTURE")

# Viterbi Algorithm to find the most probable state sequence
VitPath <- viterbi (hm_model, AAPL-gaus_Train)

# scatter plot
postscript('AAPL-gaus.eps')
AAPL-gaus_Predict <- cbind(AAPL-gaus_Subset$AAPL-gaus.Close, VitPath$states)
print(AAPL-gaus_Predict)

#layout(matrix(1:2, nrow=2))
layout(matrix(2:1, ncol=2))
print(matrix(2:1, ncol=2))

#chartSeries(AAPL-gaus_Predict[,1], #theme="white.mono", 
chartSeries(AAPL-gaus_Predict[,1], layout = layout(matrix(2:1)), # 1, 2, byrow = TRUE), #respect = TRUE), #theme="white.mono", 
TA="addTA(AAPL-gaus_Predict[AAPL-gaus_Predict[,2]==1,1], legend = \"one day?\", on=1, col=5,pch=25);
addTA(AAPL-gaus_Predict[AAPL-gaus_Predict[,2]==2,1],on=1,type='p',col=6,pch=24);
addTA(AAPL-gaus_Predict[AAPL-gaus_Predict[,2]==3,1],on=1,type='p',col=7,pch=23);
addTA(AAPL-gaus_Predict[AAPL-gaus_Predict[,2]==4,1],on=1,type='p',col=8,pch=22);
addTA(AAPL-gaus_Predict[AAPL-gaus_Predict[,2]==5,1],on=1,type='p',col=10,pch=21)
")
#addTA(AAPL-gaus_Predict[AAPL-gaus_Predict[,2]==1,1],on=1,type="p",col=5,pch=25)
#addTA(AAPL-gaus_Predict[AAPL-gaus_Predict[,2]==2,1],on=1,type="p",col=6,pch=24)
#addTA(AAPL-gaus_Predict[AAPL-gaus_Predict[,2]==3,1],on=1,type="p",col=7,pch=23)
#addTA(AAPL-gaus_Predict[AAPL-gaus_Predict[,2]==4,1],on=1,type="p",col=8,pch=22)
#addTA(AAPL-gaus_Predict[AAPL-gaus_Predict[,2]==5,1],on=1,type="p",col=10,pch=21)

