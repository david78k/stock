library( quantmod )
 
getSymbols( "SPY", from="1900-01-01" )
#print(SPY)

spyEMA = EMA( tail( Cl( SPY ), 300 ), 200 )
spyEMA = cbind(tail(Cl(SPY), 300), spyEMA)
print(spyEMA)
#print( as.numeric( last( spyEMA ) ) )
 
spyEMA = EMA( tail( Cl( SPY ), 400 ), 200 )
#print(spyEMA)
#print( as.numeric( last( spyEMA ) ) )

#getSymbols( "GOOG", from="1900-01-01" ) # duplicate bug - missing
#getSymbols( "GOOG", from="1900-01-01", src="google" ) # error
getSymbols( "GOOG", src="google" )
#getSymbols( "GOOG", src="google", from="1900-01-01", auto.assign=FALSE ) # not working
#print(GOOG)

getSymbols( "MSFT", from="1900-01-01" ) # 1986
#getSymbols( "MSFT", from="1900-01-01", src="google" )
#print(MSFT)

getSymbols( "AMZN", from="1900-01-01" ) # 1997
#getSymbols( "AMZN", from="1900-01-01", src="google" )
#print(AMZN)

getSymbols( "AAPL", from="1900-01-01" )
#getSymbols( "AAPL", from="1900-01-01", src="google" )
#print(AAPL)

