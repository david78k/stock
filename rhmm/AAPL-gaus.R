library(quantmod)

require(devEMF)
#postscript('AAPL-gaus.eps')

getSymbols("AAPL")
chartSeries(AAPL, theme="white")
trainset <- window(AAPL, start = as.Date("2000-01-01"), end = as.Date("2013-03-01"))
#AAPL_Subset <- window(AAPL, start = as.Date("2000-01-01"), end = as.Date("2013-03-01"))
#AAPL_Train <- cbind(AAPL_Subset$AAPL.Close - AAPL_Subset$AAPL.Open, AAPL_Subset$AAPL.Volume)
train <- cbind(trainset$AAPL.Close - trainset$AAPL.Open)
#AAPL_Train <- cbind(AAPL_Subset$AAPL.Close - AAPL_Subset$AAPL.Open)
#print(AAPL_Train)

testset <- window(AAPL, start = as.Date("2013-03-01"), end = as.Date("2014-03-01"))
test <- cbind(testset$AAPL.Close - testset$AAPL.Open)

library(RHmm)
# Baum-Welch Algorithm to find the model for the given observations
#hm_model <- HMMFit(obs = AAPL_Train, nStates = 5)
hm_model <- HMMFit(obs = AAPL_Train, nStates = 5, nMixt = 4, dis = "MIXTURE")

# Viterbi Algorithm to find the most probable state sequence
VitPath <- viterbi (hm_model, AAPL_Train)

# scatter plot
postscript('AAPL-gaus.eps')
AAPL_Predict <- cbind(trainset$AAPL.Close, VitPath$states)
#AAPL_Predict <- cbind(AAPL_Subset$AAPL.Close, VitPath$states)
#print(AAPL_Subset[,4] - AAPL_Predict [,1])

# predict next stock value m = nMixt, n = nStates
#sum(a[last(v),] * .colSums((matrix(unlist(a), nrow=4,ncol=5)) * (matrix(unlist(a), nrow=4,ncol=5)), m=4,n=5))
# gaussian mixture HMM: nrow = nMixture, ncol = nStates
print(hm_model$HMM$transMat[last(VitPath$states),])
print(hm_model$HMM$distribution)
print(hm_model$HMM$distribution$mean)
#print(hm_model$HMM$distribution$mean[, seq(1, ncol(hm_model$HMM$distribution$mean), by = 2)])
print(unlist(hm_model$HMM$distribution$mean))
#print(unlist(hm_model$HMM$distribution$mean[1,]))
#print(matrix(unlist(hm_model$HMM$distribution$mean[1,])))
#print(matrix(unlist(hm_model$HMM$distribution$proportion[1,])))

#testopen <- testset$AAPL.Open
#for (i in 0: length(testopen)) {
for (i in 0: 3) {
	testopen <- testset$AAPL.Open[i, ]
	testclose <- testset$AAPL.Close[i, ]
	actual <- testset$AAPL.Open[i + 1, ]
	#testrow <- testopen[i, ]
	#print(testrow)
	#print(testset$AAPL.Open[i, ])

# predict 
change <- sum(hm_model$HMM$transMat[last(VitPath$states),] * .colSums((matrix(unlist(hm_model$HMM$distribution$mean), nrow=4,ncol=5)) * (matrix(unlist(hm_model$HMM$distribution$proportion), nrow=4,ncol=5)), m=4,n=5))
#sum(hm_model$HMM$transMat[last(VitPath$states),] * .colSums((matrix(unlist(hm_model$HMM$distribution$mean[1,]), nrow=4,ncol=5)) * (matrix(unlist(hm_model$HMM$distribution$proportion[1,]), nrow=4,ncol=5)), m=4,n=5))
print(change)
#print(tail(AAPL_Subset$AAPL.Close))
#head5 <- head(testset$AAPL.Close)
#print(head5)
pred <- testclose + change
#pred <- (tail(AAPL_Subset$AAPL.Close) + change)
print(pred)

#actual <- head(testset$AAPL.Close)
#actual <- head(testset$AAPL.Open)
print(actual)

# MAPE = sum(|pred - actual|/|actual|)*100/n
#MAPE <- pred$AAPL.Close - actual$AAPL.Close
#MAPE <- abs((pred$AAPL.Close - actual$AAPL.Close)/actual$AAPL.Close)
#MAPE <- abs((pred$AAPL.Close - 420.05)/420.05) * 100
#print(MAPE)

# single HMM
#sum(hm_model$HMM$transMat[last(VitPath$states),] * .colSums((matrix(unlist(hm_model$HMM$distribution$mean), nrow=1,ncol=5)) * (matrix(unlist(hm_model$HMM$distribution$proportion), nrow=1,ncol=5)), m=1,n=5))

#chartSeries(testset, theme="white")
#chartSeries(test, theme="white")

# Forward-backward 
#fb <- forwardBackward(hm_model, test, FALSE)
#print(fb)
#print(AAPL_Subset[,4] - AAPL_Predict [,1])

#layout(matrix(1:2, nrow=2))
#layout(matrix(2:1, ncol=2))
#layout(1:2)
#print(matrix(2:1, ncol=2))

# show the states with predicted closing value
#chartSeries(pred)
#chartSeries(pred, TA = "addTA(actual, on = 1)")
#chartSeries(pred, TA = "addTA(pred - change, on = 1)")

}

#chartSeries(AAPL_Predict[,1], #theme="white.mono", 
#chartSeries(AAPL_Predict[,1], layout = layout(matrix(2:1)), # 1, 2, byrow = TRUE), #respect = TRUE), #theme="white.mono", 
#TA="addTA(AAPL_Predict[AAPL_Predict[,2]==1,1], legend = \"one day?\", on=1, col=5,pch=25);
#addTA(AAPL_Predict[AAPL_Predict[,2]==2,1],on=1,type='p',col=6,pch=24);
#addTA(AAPL_Predict[AAPL_Predict[,2]==3,1],on=1,type='p',col=7,pch=23);
#addTA(AAPL_Predict[AAPL_Predict[,2]==4,1],on=1,type='p',col=8,pch=22);
#addTA(AAPL_Predict[AAPL_Predict[,2]==5,1],on=1,type='p',col=10,pch=21)
#")

