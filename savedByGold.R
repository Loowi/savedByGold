rm(list = ls(all = TRUE))

library(PerformanceAnalytics)
library(quantmod)

# retrive data of S&P 500 and Gold (Yahoo and Fred)
getSymbols(c("^GSPC"), from="1968-01-01")
getSymbols("GOLDAMGBD228NLBM",src="FRED")
GOLDAMGBD228NLBM$GOLDAMGBD228NLBM.Adjusted <- GOLDAMGBD228NLBM$GOLDAMGBD228NLBM

# combine the adjusted close values in one (xts) data.frame
tickers <- c("GSPC","GOLDAMGBD228NLBM")
spotData <- Ad(get(tickers[1]))
for (i in 2:length(tickers)) {
  spotData <- merge(spotData, Ad(get(tickers[i])))
}
spotData <- spotData["1970/"]

# handle NA values (four common alternatives)
spotData <- na.spline(spotData)  # cubic spline interpolation

# calculate approximate monthly returns
return_lag <- 21
returnData <- na.omit(ROC(spotData, return_lag, type = "discrete"))
names(returnData) <- c("GSPC","GOLD")

# save data as RData
save(returnData, file = "returnsGoldSP500.rda")

# Convert to non-overlapping monthly values
returnData.monthly <- returnData[ endpoints(returnData, on="months", k=1), ]

# Draw scatterplot and regression line for the whole dataset since 1970
kala = as.data.frame(returnData.monthly)
reg1 <- lm(kala$GOLD ~ kala$GSPC)
plot(kala$GSPC, kala$GOLD)
abline(reg1)

# Scatterplot and regression for the dataset where monthly.change of GSPC is <-5%
sigChanges <- subset(kala , kala$GSPC < -0.05)
reg2 <- lm(sigChanges$GOLD ~ sigChanges$GSPC)
plot(sigChanges$GSPC, sigChanges$GOLD)
abline(reg2)

# Annual rolling correlation between Gold and S&P 500
gold <- spotData[,2]
names(gold) <- 'GOLD'
head(gold, 5)
SP500 <- Cl(GSPC)
colnames(SP500)[1] <- 'GSPC'

chart.RollingCorrelation(gold, SP500, legend.loc="bottomleft", colorset = "blue", main = "Rolling 12-month correlation",width=254)