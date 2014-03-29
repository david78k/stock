library(quantmod)

require(devEMF)
#postscript('FB.eps')

getSymbols("FB")
chartSeries(FB, theme="white")
FB_Subset <- window(FB, start = as.Date("2000-01-01"), end = as.Date("2013-03-01"))
FB_Train <- cbind(FB_Subset$FB.Close - FB_Subset$FB.Open, FB_Subset$FB.Volume)

library(RHmm)
# Baum-Welch Algorithm to find the model for the given observations
hm_model <- HMMFit(obs = FB_Train, nStates = 5)

# Viterbi Algorithm to find the most probable state sequence
VitPath <- viterbi (hm_model, FB_Train)

# scatter plot
postscript('FB.eps')
FB_Predict <- cbind(FB_Subset$FB.Close, VitPath$states)
#layout(matrix(1:2, nrow=2))
layout(matrix(2:1, ncol=2))
print(matrix(2:1, ncol=2))
#chartSeries(FB_Predict[,1], #theme="white.mono", 
chartSeries(FB_Predict[,1], layout = layout(matrix(2:1)), # 1, 2, byrow = TRUE), #respect = TRUE), #theme="white.mono", 
TA="addTA(FB_Predict[FB_Predict[,2]==1,1], legend = \"one day?\", on=1, col=5,pch=25);
addTA(FB_Predict[FB_Predict[,2]==2,1],on=1,type='p',col=6,pch=24);
addTA(FB_Predict[FB_Predict[,2]==3,1],on=1,type='p',col=7,pch=23);
addTA(FB_Predict[FB_Predict[,2]==4,1],on=1,type='p',col=8,pch=22);
addTA(FB_Predict[FB_Predict[,2]==5,1],on=1,type='p',col=10,pch=21)
")
#addTA(FB_Predict[FB_Predict[,2]==1,1],on=1,type="p",col=5,pch=25)
#addTA(FB_Predict[FB_Predict[,2]==2,1],on=1,type="p",col=6,pch=24)
#addTA(FB_Predict[FB_Predict[,2]==3,1],on=1,type="p",col=7,pch=23)
#addTA(FB_Predict[FB_Predict[,2]==4,1],on=1,type="p",col=8,pch=22)
#addTA(FB_Predict[FB_Predict[,2]==5,1],on=1,type="p",col=10,pch=21)

