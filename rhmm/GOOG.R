library(quantmod)

require(devEMF)
#postscript('GOOG.eps')

getSymbols("GOOG")
chartSeries(GOOG, theme="white")
GOOG_Subset <- window(GOOG, start = as.Date("2000-01-01"), end = as.Date("2013-03-01"))
GOOG_Train <- cbind(GOOG_Subset$GOOG.Close - GOOG_Subset$GOOG.Open, GOOG_Subset$GOOG.Volume)

library(RHmm)
# Baum-Welch Algorithm to find the model for the given observations
hm_model <- HMMFit(obs = GOOG_Train, nStates = 5)

# Viterbi Algorithm to find the most probable state sequence
VitPath <- viterbi (hm_model, GOOG_Train)

# scatter plot
postscript('GOOG.eps')
GOOG_Predict <- cbind(GOOG_Subset$GOOG.Close, VitPath$states)
#layout(matrix(1:2, nrow=2))
layout(matrix(1:2, ncol=2))
#chartSeries(GOOG_Predict[,1], #theme="white.mono", 
chartSeries(GOOG_Predict[,1], #layout = layout(matrix(c(1, 1), 1, 2, byrow = TRUE), #respect = TRUE), #theme="white.mono", 
TA="addTA(GOOG_Predict[GOOG_Predict[,2]==1,1],on=1, col=5,pch=25);
addTA(GOOG_Predict[GOOG_Predict[,2]==2,1],on=1,type='p',col=6,pch=24);
addTA(GOOG_Predict[GOOG_Predict[,2]==3,1],on=1,type='p',col=7,pch=23);
addTA(GOOG_Predict[GOOG_Predict[,2]==4,1],on=1,type='p',col=8,pch=22);
addTA(GOOG_Predict[GOOG_Predict[,2]==5,1],on=1,type='p',col=10,pch=21)
")
#addTA(GOOG_Predict[GOOG_Predict[,2]==1,1],on=1,type="p",col=5,pch=25)
#addTA(GOOG_Predict[GOOG_Predict[,2]==2,1],on=1,type="p",col=6,pch=24)
#addTA(GOOG_Predict[GOOG_Predict[,2]==3,1],on=1,type="p",col=7,pch=23)
#addTA(GOOG_Predict[GOOG_Predict[,2]==4,1],on=1,type="p",col=8,pch=22)
#addTA(GOOG_Predict[GOOG_Predict[,2]==5,1],on=1,type="p",col=10,pch=21)

