require(devEMF)
library(quantmod)
library(RHmm)
#postscript('MSFT-gaus.eps')

getSymbols("MSFT")
chartSeries(MSFT, theme="white")
trainset <- window(MSFT, start = as.Date("2000-01-01"), end = as.Date("2013-04-01"))
#print(trainset)
#MSFT_Subset <- window(MSFT, start = as.Date("2000-01-01"), end = as.Date("2013-04-01"))
#MSFT_Train <- cbind(MSFT_Subset$MSFT.Close - MSFT_Subset$MSFT.Open, MSFT_Subset$MSFT.Volume)
train <- cbind(trainset$MSFT.Close - trainset$MSFT.Open)
#print(train)

testset <- window(MSFT, start = as.Date("2013-04-01"), end = as.Date("2014-04-01"))
test <- cbind(testset$MSFT.Close - testset$MSFT.Open)
#print(testset)

# Baum-Welch Algorithm to find the model for the given observations
#hm_model <- HMMFit(obs = MSFT_Train, nStates = 5)
hm_model <- HMMFit(obs = train, nStates = 5, nMixt = 4, dis = "MIXTURE")

# Viterbi Algorithm to find the most probable state sequence
VitPath <- viterbi (hm_model, train)

# scatter plot
postscript('MSFT-gaus.eps')
MSFT_Predict <- cbind(trainset$MSFT.Close, VitPath$states)
#MSFT_Predict <- cbind(MSFT_Subset$MSFT.Close, VitPath$states)
#print(MSFT_Subset[,4] - MSFT_Predict [,1])

# predict next stock value m = nMixt, n = nStates
#sum(a[last(v),] * .colSums((matrix(unlist(a), nrow=4,ncol=5)) * (matrix(unlist(a), nrow=4,ncol=5)), m=4,n=5))
# gaussian mixture HMM: nrow = nMixture, ncol = nStates
#print(hm_model$HMM$transMat[last(VitPath$states),])
#print(hm_model$HMM$distribution)
#print(hm_model$HMM$distribution$mean)
#print(hm_model$HMM$distribution$mean[, seq(1, ncol(hm_model$HMM$distribution$mean), by = 2)])
#print(unlist(hm_model$HMM$distribution$mean))
#print(matrix(unlist(hm_model$HMM$distribution$proportion[1,])))

# add a new colum "Pred"
testset <- cbind(testset, Pred = 0)
#testset <- cbind(testset$MSFT.Close, Pred = 0)
#print(testset)

#chartSeries(testset, theme="white")
#chartSeries(test, theme="white")

# number of rows of test set data
rows = nrow(testset)

#MAPEsum = 0
MAPEsum <- 0
#MAPEsum <- MAPEsum + 3.35
#print (MAPEsum)

#MAPEsum <- abs(MAPEsum / 2.71)
#print(MAPEsum)

#MAPEsum <- MAPEsum + 5.18
#print(MAPEsum)

# predict and update HMM to include the new actual value
#for (i in 1: 251) {
#for (i in 1: 3) {
for (i in 1: rows) {
	#if (i == rows) break

	if(i != 0) {
		testrow <- testset[i, ]
		#print(testrow)
		todayopen <- testset$MSFT.Open[i, ]
		todayclose <- testset$MSFT.Close[i, ]
	}

	# predict the closing value of today
	change <- sum(hm_model$HMM$transMat[last(VitPath$states),] * .colSums((matrix(unlist(hm_model$HMM$distribution$mean), nrow=4,ncol=5)) * (matrix(unlist(hm_model$HMM$distribution$proportion), nrow=4,ncol=5)), m=4,n=5))
	#sum(hm_model$HMM$transMat[last(VitPath$states),] * .colSums((matrix(unlist(hm_model$HMM$distribution$mean[1,]), nrow=4,ncol=5)) * (matrix(unlist(hm_model$HMM$distribution$proportion[1,]), nrow=4,ncol=5)), m=4,n=5))
	print(change)

	pred <- todayopen + change
	#testrow$Pred <- pred
	#print(pred)

	# update today's predicted value
	testset[i, ]$Pred <- pred
	print(testset[i, ])

	# MAPE = sum(|pred - actual|/|actual|)*100/n
	diff = (abs ((pred - todayclose)/ todayclose))[1,]$MSFT.Open
	#print ("diff")
	#print (diff)
	#MAPEsum <- MAPEsum + diff$MSFT.Open
	MAPEsum <- sum(MAPEsum, diff[1,1])
	#MAPEsum = MAPEsum + abs((pred - todayclose)/todayclose)
	#print ("MAPEsum")
	#print(MAPEsum)
	#MAPE <- MAPEsum*100/rows
	#print("MAPE")
	#print(MAPE)

	# NRMSE = sqrt(sum((pred - actual)^2) / n)

	# ROC

	# [Optional] Returns: sell or buy
	# if stock increased sell, otherwise buy

	# single HMM
	#sum(hm_model$HMM$transMat[last(VitPath$states),] * .colSums((matrix(unlist(hm_model$HMM$distribution$mean), nrow=1,ncol=5)) * (matrix(unlist(hm_model$HMM$distribution$proportion), nrow=1,ncol=5)), m=1,n=5))

	# Forward-backward 
	#fb <- forwardBackward(hm_model, test, FALSE)
	#print(fb)
	#print(MSFT_Subset[,4] - MSFT_Predict [,1])

	# update train data
	train <- rbind (train, todayclose - todayopen)
	
	# update HMM with the new data
	# Baum-Welch Algorithm to find the model for the given observations
	hm_model <- HMMFit(obs = train, nStates = 5, nMixt = 4, dis = "MIXTURE")

	# Viterbi Algorithm to find the most probable state sequence
	VitPath <- viterbi (hm_model, train)
}

MAPE <- MAPEsum*100/rows
print(MAPE)

# plot actual with predicted values added
# compare actual closing value and predicted closing value
#chartSeries(testset[2:rows, 4], theme='white', col = 'green', name = "MSFT", legend = "Actual",
chartSeries(testset[1:rows, 1], theme= chartTheme('white', up.col = 'blue'), name = "MSFT", legend = "Actual",
	TA = "addTA(testset[1:rows, 7], on = 1, col='red')") # 
#chartSeries(testset[2:rows, 1], theme='white.mono', name = 'Actual', TA = "addTA(testset[2:rows, 7], on = 1, col='yellow', legend = \"Predicted\")") # 
#chartSeries(testset[, 1], name = 'Actual', TA = "addTA(testset[, 7], on = 1, col='blue', legend = \"Predicted\")") # 
#chartSeries(testset[, 1], TA = "addTA(testset[, 7], on = 1, col=26, legend = \"Predicted\")") # blue
#chartSeries(testset[, 1], TA = "addTA(testset[, 7], on = 1, col=col2rgb("blue"), legend = \"Predicted\")") # 
#chartSeries(testset[, 1], TA = "addTA(testset[, 7], on = 1, col=7, legend = \"Predicted\")") # yellow
#chartSeries(testset[, 1], TA = "addTA(testset[, 7], on = 1, col=10)") # red
#chartSeries(testset[, 1], TA = "addTA(testset[, 7], on = 1, col=8)") # grey?
#chartSeries(testset[, 1], TA = "addTA(testset[, 7], on = 1, col=6)") # pink
#chartSeries(testset[, 1], TA = "addTA(testset[, 7], on = 1, col=9)") # black

#chartSeries(testset)

#chartSeries(MSFT_Predict[,1], layout = layout(matrix(2:1)), # 1, 2, byrow = TRUE), #respect = TRUE), #theme="white.mono", 
#TA="addTA(MSFT_Predict[MSFT_Predict[,2]==1,1], legend = \"one day?\", on=1, col=5,pch=25);
#addTA(MSFT_Predict[MSFT_Predict[,2]==2,1],on=1,type='p',col=6,pch=24);
#addTA(MSFT_Predict[MSFT_Predict[,2]==3,1],on=1,type='p',col=7,pch=23);
#addTA(MSFT_Predict[MSFT_Predict[,2]==4,1],on=1,type='p',col=8,pch=22);
#addTA(MSFT_Predict[MSFT_Predict[,2]==5,1],on=1,type='p',col=10,pch=21)
#")

