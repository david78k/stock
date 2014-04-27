require(devEMF)
library(quantmod)
library(RHmm)
library(parallel)
#postscript('LNKD.eps')

getSymbols("LNKD", src = "google")
#getSymbols("LNKD")
chartSeries(LNKD, theme="white")
#trainset <- window(LNKD, start = as.Date("2000-01-01"), end = as.Date("2013-04-01"))
trainsetraw <- window(LNKD, start = as.Date("2000-01-01"), end = as.Date("2013-04-01"))
print(ncol(trainsetraw)-1)
print(trainsetraw[,1:ncol(trainsetraw)-1])
trainset <- na.omit(trainsetraw[,1:ncol(trainsetraw)-1])
print(trainset)

#LNKD_Subset <- window(LNKD, start = as.Date("2000-01-01"), end = as.Date("2013-04-01"))
#LNKD_Train <- cbind(LNKD_Subset$LNKD.Close - LNKD_Subset$LNKD.Open, LNKD_Subset$LNKD.Volume)
train <- cbind(trainset$LNKD.Close - trainset$LNKD.Open)
#print(train)

testset <- window(LNKD, start = as.Date("2013-04-02"), end = as.Date("2014-04-01"))
test <- cbind(testset$LNKD.Close - testset$LNKD.Open)
print(testset)

# Baum-Welch Algorithm to find the model for the given observations
#hm_model <- HMMFit(obs = LNKD_Train, nStates = 5)
hm_model <- HMMFit(obs = train, nStates = 5, nMixt = 4, dis = "MIXTURE")
print(hm_model)

# Viterbi Algorithm to find the most probable state sequence
VitPath <- viterbi (hm_model, train)
print(VitPath)

# scatter plot
postscript('LNKD.eps')
LNKD_Predict <- cbind(trainset$LNKD.Close, VitPath$states)
#LNKD_Predict <- cbind(LNKD_Subset$LNKD.Close, VitPath$states)
#print(LNKD_Subset[,4] - LNKD_Predict [,1])

# predict next stock value m = nMixt, n = nStates
#sum(a[last(v),] * .colSums((matrix(unlist(a), nrow=4,ncol=5)) * (matrix(unlist(a), nrow=4,ncol=5)), m=4,n=5))
# gaussian mixture HMM: nrow = nMixture, ncol = nStates
#print(hm_model$HMM$transMat[last(VitPath$states),])
#print(hm_model$HMM$distribution$mean[, seq(1, ncol(hm_model$HMM$distribution$mean), by = 2)])
#print(unlist(hm_model$HMM$distribution$mean))
#print(matrix(unlist(hm_model$HMM$distribution$proportion[1,])))

# add a new colum "Pred"
testset <- cbind(testset, Pred = 0)
#testset <- cbind(testset$LNKD.Close, Pred = 0)
#print(testset)

#chartSeries(testset, theme="white")
#chartSeries(test, theme="white")

# number of rows of test set data
rows = nrow(testset)

MAPEsum = 0
NRMSEsum = 0
#MAPEsum <- 0

# predict and update HMM to include the new actual value
#for (i in 1: 251) {
#for (i in 1: 3) {
for (i in 1: rows) {
	#if (i == rows) break

	if(i != 0) {
		testrow <- testset[i, ]
		#print(testrow)
		todayopen <- testset$LNKD.Open[i, ]
		actual <- testset$LNKD.Close[i, ]
		#todayclose <- testset$LNKD.Close[i, ]
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
	diff = (abs ((pred - actual)/ actual))[1,]$LNKD.Open
	#print (diff)
	#MAPEsum <- MAPEsum + diff$LNKD.Open
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
	train <- rbind (train, todayclose - todayopen)
	
	# update HMM with the new data
	# Baum-Welch Algorithm to find the model for the given observations
	hm_model <- HMMFit(obs = train, nStates = 5, nMixt = 4, dis = "MIXTURE")

	# Viterbi Algorithm to find the most probable state sequence
	VitPath <- viterbi (hm_model, train)
}

print(paste0("[Stat] Rows = ", rows))
#print(rows)

MAPE <- MAPEsum*100/rows
sprintf("[Stat] MAPE = %s\n", MAPE)
#cat("[Stat] MAPE = ", MAPE)
#print(MAPE)

actuals <- testset$LNKD.Close
ymax = max (actuals)
ymin = min (actuals)
NRMSE <- sqrt(NRMSEsum)/(rows * (ymax - ymin))
print(paste0("[Stat] NRMSE = ", NRMSE))
#cat("NRMSE = ", NRMSE)
#print(NRMSE)

# plot actual with predicted values added
# compare actual closing value and predicted closing value
#chartSeries(testset[2:rows, 4], theme='white', col = 'green', name = "LNKD", legend = "Actual",
chartSeries(testset[1:rows, 1], theme= chartTheme('white', up.col = 'blue'), name = "LNKD", legend = "Actual",
	TA = "addTA(testset[1:rows, ncol(testset)], on = 1, col='red')") # 
#chartSeries(testset[2:rows, 1], theme='white.mono', name = 'Actual', TA = "addTA(testset[2:rows, 7], on = 1, col='yellow', legend = \"Predicted\")") # 
#chartSeries(testset[, 1], name = 'Actual', TA = "addTA(testset[, 7], on = 1, col='blue', legend = \"Predicted\")") # 
#chartSeries(testset)

