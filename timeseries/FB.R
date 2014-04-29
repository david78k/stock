require(devEMF)
library(quantmod)
library(RHmm)
library(parallel)
#png('FB.png')

getSymbols("FB" , from = '1900-01-01', to = '2014-04-01', src = 'yahoo')
print(head(FB))
print(tail(FB))
chartSeries(FB, theme="white")

#trainset <- window(FB, start = as.Date("1900-01-01"), end = as.Date("2013-04-01"))
trainsetraw <- window(FB, start = as.Date("1900-01-01"), end = as.Date("2013-04-01"))
print(ncol(trainsetraw)-1)
print(trainsetraw[,1:ncol(trainsetraw)-1])
trainset <- na.omit(trainsetraw[,1:ncol(trainsetraw)-1])
print(trainset)

#FB_Subset <- window(FB, start = as.Date("1900-01-01"), end = as.Date("2013-04-01"))
train <- cbind(trainset$FB.Close - trainset$FB.Open)
#print(train)

testset <- window(FB, start = as.Date("2013-04-02"), end = as.Date("2014-04-01"))
test <- cbind(testset$FB.Close - testset$FB.Open)
print(testset)

# Baum-Welch Algorithm to find the model for the given observations
# Gaussian mixture HMM: nStates = number of states, nMixt = number of Gaussian distributions 
#hm_model <- HMMFit(obs = train, nStates = 5, nMixt = 4, dis = "MIXTURE")
hm_model <- HMMFit(obs = train, nStates = 5, nMixt = 4, dis = "MIXTURE", control=list(verbose=1, init="KMEANS"))
print(hm_model)

# Viterbi Algorithm to find the most probable state sequence
VitPath <- viterbi (hm_model, train)
print(VitPath)

# scatter plot
png('FB.png')
FB_Predict <- cbind(trainset$FB.Close, VitPath$states)

# add a new colum "Pred"
testset <- cbind(testset, Pred = 0)
#print(testset)

# number of rows of test set data
rows = nrow(testset)

MAPEsum = 0
NRMSEsum = 0

# predict and update HMM to include the new actual value
#for (i in 1: 251) {
#for (i in 1: 3) {
for (i in 1: rows) {
	if(i != 0) {
		testrow <- testset[i, ]
		#print(testrow)
		todayopen <- testset$FB.Open[i, ]
		actual <- testset$FB.Close[i, ]
		#todayclose <- testset$FB.Close[i, ]
	}

	# predict the closing value of today
	change <- sum(hm_model$HMM$transMat[last(VitPath$states),] * .colSums((matrix(unlist(hm_model$HMM$distribution$mean), nrow=4,ncol=5)) * (matrix(unlist(hm_model$HMM$distribution$proportion), nrow=4,ncol=5)), m=4,n=5))
	print(change)

	pred <- todayopen + change
	#testrow$Pred <- pred
	#print(pred)

	# update today's predicted value
	testset[i, ]$Pred <- pred
	print(testset[i, ])

	# MAPE = sum(|pred - actual|/|actual|)*100/n
	diff = (abs ((pred - actual)/ actual))[1,]$FB.Open
	#print (diff)
	MAPEsum <- sum(MAPEsum, diff[1,1])
	#print(MAPEsum)

	# NRMSE = sqrt(sum((pred - actual)^2) / n)
	NRMSEsum <- sum(NRMSEsum, (pred - actual)^2) 

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

actuals <- testset$FB.Close
ymax = max (actuals)
ymin = min (actuals)
NRMSE <- sqrt(NRMSEsum)/(rows * (ymax - ymin))
sprintf("[Stat] NRMSE = %.7f", NRMSE)

# plot actual with predicted values added
# compare actual closing value and predicted closing value
chartSeries(testset[1:rows, 1], theme= chartTheme('white', up.col = 'blue'), name = "FB", legend = "Actual",
	TA = "addTA(testset[1:rows, ncol(testset)], on = 1, col='red')") # 

# plot eps
postscript('FB.eps')
chartSeries(testset[1:rows, 1], theme= chartTheme('white', up.col = 'blue'), name = "FB", legend = "Actual",
	TA = "addTA(testset[1:rows, ncol(testset)], on = 1, col='red')") # 
