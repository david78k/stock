library(quantmod)
getSymbols("^TWII")
chartSeries(TWII)
TWII_Subset <- window(TWII, start = as.Date("2012-01-01"))
TWII_Train <- cbind(TWII_Subset$TWII.Close - TWII_Subset$TWII.Open, TWII_Subset$TWII.Volume)

library(RHmm)
# Baum-Welch Algorithm
hm_model <- HMMFit(obs = TWII_Train, nStates = 5)

# Viterbi Algorithm
VitPath <- viterbi (hm_model, TWII_Train)

# scatter plot
TWII_Predict <- cbind(TWII_Subset$TWII.Close, VitPath$states)
chartSeries(TWII_Predict[,1])
addTA(TWII_Predict[TWII_Predict[,2]==1,1],on=1,type="p",col=5,pch=25)
addTA(TWII_Predict[TWII_Predict[,2]==2,1],on=1,type="p",col=6,pch=24)
addTA(TWII_Predict[TWII_Predict[,2]==3,1],on=1,type="p",col=7,pch=23)
addTA(TWII_Predict[TWII_Predict[,2]==4,1],on=1,type="p",col=8,pch=22)
addTA(TWII_Predict[TWII_Predict[,2]==5,1],on=1,type="p",col=10,pch=21)
