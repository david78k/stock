library(quantmod)
getSymbols("^TWII")
chartSeries(TWII)
TWII_Subset <- window(TWII, start = as.Date("2012-01-01"))
TWII_Train <- cbind(TWII_Subset$TWII.Close - TWII_Subset$TWII.Open, TWII_Subset$TWII.Volume)

library(RHmm)
# Baum-Welch Algorithm
hm_model <- HMMFit(obs = TWII_Train, nStates = 5)

# Viterbi Algorithm
VitPath <- viterbi (hm_model, TWII_Train)
