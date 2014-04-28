	require(devEMF)
	library(quantmod)

	getSymbols("AMZN", from = '1900-01-01', to = '2014-04-01')
	print(head(AMZN))
	print(tail(AMZN))

	actuals = Cl( AMZN )
	print(length(actuals))

	pred = EMA( actuals, length(actuals) - 253 )
	#ema = EMA( tail( Cl( AMZN ), 300 ), 200 )
	#ema = cbind(tail(Cl(AMZN), 300), ema)
	ema = cbind(actuals, pred)
	print(tail(ema, 255))
	#print( as.numeric( last( spyEMA ) ) )
	ema = tail(ema, 253)

	pred = ema[,1]
	actual = ema[,2]
	print (pred)
	print (actual)

	# MAPE = sum(|pred - actual|/|actual|)*100/n
	diff = abs ((pred - actual)/ actual)
	print (diff)
	MAPEsum = sum(diff)
	print(MAPEsum)
	rows = length(diff)
	print(paste0("[Stat] Rows = ", rows))
	MAPE <- MAPEsum*100/rows
	print(paste0("[Stat] MAPE = ", MAPE))

	# NRMSE = sqrt(sum((pred - actual)^2) / n)
	NRMSEsum <- sum((pred - actual)^2) 
	ymax = max (actual)
	ymin = min (actual)
	NRMSE <- sqrt(NRMSEsum)/(rows * (ymax - ymin))
	print(paste0("[Stat] NRMSE = ", NRMSE))
