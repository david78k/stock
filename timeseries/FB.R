	require(devEMF)
	library(quantmod)
	library(RHmm)
	library(parallel)

	getSymbols("FB", from = '1900-01-01', to = '2014-04-01')
	print(head(FB))
	print(tail(FB))

	actuals = Cl( FB )
	print(length(actuals))

	pred = EMA( actuals, length(actuals) - 253 )
	#ema = EMA( tail( Cl( FB ), 300 ), 200 )
	#ema = cbind(tail(Cl(FB), 300), ema)
	ema = cbind(actuals, pred)
	print(tail(ema, 255))
	#print( as.numeric( last( spyEMA ) ) )
	ema = tail(ema, 253)

	pred = ema[,1]
	actual = ema[,2]
	print (pred)
	print (actual)

	# MAPE = sum(|pred - actual|/|actual|)*100/n
	diff = (abs ((pred - actual)/ actual))
	#diff = (abs ((pred - actual)/ actual))[1,]$FB.Open
	print (diff)
	#MAPEsum <- MAPEsum + diff$FB.Open
	#MAPEsum <- sum(MAPEsum, diff[1,1])
	#MAPEsum = MAPEsum + abs((pred - actual)/todayclose)
	#print(MAPEsum)
	#MAPE <- MAPEsum*100/rows
	#print(MAPE)
	#MAPE <- MAPEsum*100/rows
	#print("[Stat] MAPE = ", MAPE)

	# NRMSE = sqrt(sum((pred - actual)^2) / n)
	#NRMSEsum <- sum(NRMSEsum, (pred - actual)^2) 

	#actuals <- testset$FB.Close
	#ymax = max (actuals)
	#ymin = min (actuals)
	#NRMSE <- sqrt(NRMSEsum)/(rows * (ymax - ymin))
	#print(paste0("[Stat] NRMSE = ", NRMSE))
