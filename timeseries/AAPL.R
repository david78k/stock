	require(devEMF)
	library(quantmod)

	#options('digits')
	#options(digits=7)

	getSymbols("AAPL" , from = '1900-01-01', to = '2014-04-01', src = 'yahoo')
	#getSymbols("AAPL", from = '1900-01-01', to = '2014-04-01', src = 'yahoo')
	#getSymbols("AAPL", src = 'yahoo')
	print(head(AAPL))
	print(tail(AAPL))

	actuals = Cl( AAPL )
	print(length(actuals))

	pred = EMA( actuals, 2 )
	#pred = EMA( actuals, length(actuals) - 253 )
	#print( as.numeric( last( spyEMA ) ) )

	#pred = ema[,1]
	rows = length(pred)
	#for (i in 2: rows) {
	for (i in 2: rows - 2) {
		pred[rows - i,] = pred[rows - i - 1,]	
	}
	ema = cbind(actuals, pred)
	ema = tail(ema, 253)
	print (ema)
	pred = ema[,1]
	actual = ema[,2]

	# MAPE = sum(|pred - actual|/|actual|)*100/n
	diff = abs ((pred - actual)/ actual)
	print (diff)
	MAPEsum = sum(diff)
	print(MAPEsum)
	rows = length(diff)
	print(paste0("[Stat] Rows = ", rows))
	MAPE <- MAPEsum*100/rows
	sprintf("[Stat] MAPE = %.7f", MAPE)

	# NRMSE = sqrt(sum((pred - actual)^2) / n)
	NRMSEsum <- sum((pred - actual)^2) 
	ymax = max (actual)
	ymin = min (actual)
	NRMSE <- sqrt(NRMSEsum)/(rows * (ymax - ymin))
	sprintf("[Stat] NRMSE = %.7f", NRMSE)
