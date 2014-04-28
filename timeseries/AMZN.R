require(devEMF)
library(quantmod)
library(RHmm)
library(parallel)
#png('AMZN.png')

getSymbols("AMZN" , from = '1900-01-01', to = '2014-04-01', src = 'yahoo')
#getSymbols("AMZN", src = "google")
#getSymbols("AMZN", from = '1900-01-01')
#getSymbols("AMZN")
print(head(AMZN))
print(tail(AMZN))
chartSeries(AMZN, theme="white")

#trainset <- window(AMZN, start = as.Date("1900-01-01"), end = as.Date("2013-04-01"))
trainsetraw <- window(AMZN, start = as.Date("1900-01-01"), end = as.Date("2013-04-01"))
print(ncol(trainsetraw)-1)
print(trainsetraw[,1:ncol(trainsetraw)-1])
trainset <- na.omit(trainsetraw[,1:ncol(trainsetraw)-1])
print(trainset)

#AMZN_Subset <- window(AMZN, start = as.Date("1900-01-01"), end = as.Date("2013-04-01"))
#AMZN_Train <- cbind(AMZN_Subset$AMZN.Close - AMZN_Subset$AMZN.Open, AMZN_Subset$AMZN.Volume)
train <- cbind(trainset$AMZN.Close - trainset$AMZN.Open)
#print(train)

testset <- window(AMZN, start = as.Date("2013-04-02"), end = as.Date("2014-04-01"))
test <- cbind(testset$AMZN.Close - testset$AMZN.Open)
print(testset)

# Baum-Welch Algorithm to find the model for the given observations
#hm_model <- HMMFit(obs = AMZN_Train, nStates = 5)
hm_model <- HMMFit(obs = train, nStates = 5, nMixt = 4, dis = "MIXTURE")
print(hm_model)

# Viterbi Algorithm to find the most probable state sequence
VitPath <- viterbi (hm_model, train)
print(VitPath)

# scatter plot
png('AMZN.png')
AMZN_Predict <- cbind(trainset$AMZN.Close, VitPath$states)
#AMZN_Predict <- cbind(AMZN_Subset$AMZN.Close, VitPath$states)
#print(AMZN_Subset[,4] - AMZN_Predict [,1])

# predict next stock value m = nMixt, n = nStates
#sum(a[last(v),] * .colSums((matrix(unlist(a), nrow=4,ncol=5)) * (matrix(unlist(a), nrow=4,ncol=5)), m=4,n=5))
# gaussian mixture HMM: nrow = nMixture, ncol = nStates
#print(hm_model$HMM$transMat[last(VitPath$states),])
#print(hm_model$HMM$distribution$mean[, seq(1, ncol(hm_model$HMM$distribution$mean), by = 2)])
#print(unlist(hm_model$HMM$distribution$mean))
#print(matrix(unlist(hm_model$HMM$distribution$proportion[1,])))

# add a new colum "Pred"
testset <- cbind(testset, Pred = 0)
#testset <- cbind(testset$AMZN.Close, Pred = 0)
#print(testset)

#chartSeries(testset, theme="white")
#chartSeries(test, theme="white")

# number of rows of test set data
rows = nrow(testset)

MAPEsum = 0
NRMSEsum = 0

# predict and update HMM to include the new actual value
#for (i in 1: 251) {
#for (i in 1: 3) {
for (i in 1: rows) {
	#if (i == rows) break

	if(i != 0) {
		testrow <- testset[i, ]
		#print(testrow)
		todayopen <- testset$AMZN.Open[i, ]
		actual <- testset$AMZN.Close[i, ]
		#todayclose <- testset$AMZN.Close[i, ]
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
	#diff = (abs ((pred - actual)/ actual))[1,]$AMZN.Close
	diff = (abs ((pred - actual)/ actual))[1,]$AMZN.Open
	#print (diff)
	#MAPEsum <- MAPEsum + diff$AMZN.Open
	MAPEsum <- sum(MAPEsum, diff[1,1])
	#MAPEsum = MAPEsum + abs((pred - actual)/todayclose)
	#print(MAPEsum)
	#MAPE <- MAPEsum*100/rows
	#print(MAPE)

	# NRMSE = sqrt(sum((pred - actual)^2) / n)
	NRMSEsum <- sum(NRMSEsum, (pred - actual)^2) 

	# ROC

	# [Optional] Returns: sell or buy
	# if stock would increase sell, otherwise buy

	# single HMM
	#sum(hm_model$HMM$transMat[last(VitPath$states),] * .colSums((matrix(unlist(hm_model$HMM$distribution$mean), nrow=1,ncol=5)) * (matrix(unlist(hm_model$HMM$distribution$proportion), nrow=1,ncol=5)), m=1,n=5))

	# update train data
	train <- rbind (train, actual - todayopen)
	
	# update HMM with the new data
	# Baum-Welch Algorithm to find the model for the given observations
	hm_model <- HMMFit(obs = train, nStates = 5, nMixt = 4, dis = "MIXTURE")

	# Viterbi Algorithm to find the most probable state sequence
	VitPath <- viterbi (hm_model, train)
}

print(paste0("[Stat] Rows = ", rows))

print(MAPEsum)
MAPE <- MAPEsum*100/rows
sprintf("[Stat] MAPE = %.7f", MAPE)

actuals <- testset$AMZN.Close
ymax = max (actuals)
ymin = min (actuals)
NRMSE <- sqrt(NRMSEsum)/(rows * (ymax - ymin))
sprintf("[Stat] NRMSE = %.7f", NRMSE)

# plot actual with predicted values added
# compare actual closing value and predicted closing value
#chartSeries(testset[2:rows, 4], theme='white', col = 'green', name = "AMZN", legend = "Actual",
chartSeries(testset[1:rows, 1], theme= chartTheme('white', up.col = 'blue'), name = "AMZN", legend = "Actual",
	TA = "addTA(testset[1:rows, ncol(testset)], on = 1, col='red')") # 
#chartSeries(testset[2:rows, 1], theme='white.mono', name = 'Actual', TA = "addTA(testset[2:rows, 7], on = 1, col='yellow', legend = \"Predicted\")") # 
#chartSeries(testset[, 1], name = 'Actual', TA = "addTA(testset[, 7], on = 1, col='blue', legend = \"Predicted\")") # 
#chartSeries(testset)

# plot eps
postscript('AMZN.eps')
chartSeries(testset[1:rows, 1], theme= chartTheme('white', up.col = 'blue'), name = "AMZN", legend = "Actual",
	TA = "addTA(testset[1:rows, ncol(testset)], on = 1, col='red')") # 
