library(caret)

load("IBM_final.RData")
# load("MSFT_final.RData")
# load("AMZN_final.RData")
# load("BA_final.RData")
# load("XOM_final.RData")
# load("AAPL_final.RData")
est_spy = vector(length=(nrow(stock_price_final)-100))
set.seed(1)
for(d in sample(101:nrow(stock_price_final), 5)){
      dat = stock_price_final[(d-100):(d-1),]
      set.seed(1)
      svmProfile = rfe(dat[,-c(1,ncol(dat))],
                       dat$response,
                       sizes = c(1:5,10,15,20,25,30,50,70,90),
                       rfeControl = rfeControl(functions = caretFuncs,
                                               number = 200),
                       method = "svmRadial")
      predictors(svmProfile)
      trellis.par.set(caretTheme())
      plot(svmProfile, type = c("g", "o"))
}  




