library( quantmod )
 
getSymbols( "SPY", from="1900-01-01" )
 
spyEMA = EMA( tail( Cl( SPY ), 300 ), 200 )
print( as.numeric( last( spyEMA ) ) )
 
spyEMA = EMA( tail( Cl( SPY ), 400 ), 200 )
print( as.numeric( last( spyEMA ) ) )
