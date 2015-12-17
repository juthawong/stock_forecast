source('models.R', echo=TRUE)

measurePerform = function(pret,aret){
#       if(!day%in% c(1,2,5)) print("Invalid day value!") 
      if(length(pret)!=length(aret)) print("Predicted and acturual price differ in length!")
      # measure return 1 day
      estReturnSharpe = pret[1:(length(pret)-1)]
      realReturnSharpe = stock_price_final[102:nrow(dataset),2]

      print("##### Percentage of correct predictions #####")
      print(mean(sign(estReturnSharpe) == sign(realReturnSharpe)))
      print("##### Sharpe Ratio ######")
      print(computeSharpe(realReturnSharpe,estReturnSharpe)) 
}

#The two inputs are actual return and estimated return
computeSharpe = function(actual,est){
      pnl = computePNL(actual,est)
      mean_pnl = mean(pnl)
      std_pnl = sqrt(var(pnl))
      Sharpe = mean_pnl*sqrt(252)/std_pnl
      return(Sharpe)
}

computePNL = function(actual,est){
      pnl = vector(length = length(actual))
      for(i in 1:length(pnl)){
            pnl[i]=sign(est[i])*actual[i]
      }
      return(pnl)
}

#Compute portfolio Sharpe ratio
#Choose a specific model
# n = 8
n = 6
load("SPYReturn.RData")

load("IBM_final.RData")
aReturnSharpe = stock_price_final[102:nrow(stock_price_final),2]
pReturn = predSlideWindow(n)
pReturnSharpe = pReturn[1:(length(pReturn)-1)]

len = length(aReturnSharpe)
length(SPYreturn) = len
aReturnSharpe = aReturnSharpe - SPYreturn
pReturnSharpe = pReturnSharpe - SPYreturn
pnl_IBM = computePNL(aReturnSharpe,pReturnSharpe)


load("MSFT_final.RData")
aReturnSharpe = stock_price_final[102:nrow(stock_price_final),2]
pReturn = predSlideWindow(n)
pReturnSharpe = pReturn[1:(length(pReturn)-1)]

len = length(aReturnSharpe)
length(SPYreturn) = len
aReturnSharpe = aReturnSharpe - SPYreturn
pReturnSharpe = pReturnSharpe - SPYreturn

pnl_MSFT = computePNL(aReturnSharpe,pReturnSharpe)

load("AMZN_final.RData")
aReturnSharpe = stock_price_final[102:nrow(stock_price_final),2]
pReturn = predSlideWindow(n)
pReturnSharpe = pReturn[1:(length(pReturn)-1)]

len = length(aReturnSharpe)
length(SPYreturn) = len
aReturnSharpe = aReturnSharpe - SPYreturn
pReturnSharpe = pReturnSharpe - SPYreturn

pnl_AMZN = computePNL(aReturnSharpe,pReturnSharpe)

load("BA_final.RData")
aReturnSharpe = stock_price_final[102:nrow(stock_price_final),2]
pReturn = predSlideWindow(n)
pReturnSharpe = pReturn[1:(length(pReturn)-1)]

len = length(aReturnSharpe)
length(SPYreturn) = len
aReturnSharpe = aReturnSharpe - SPYreturn
pReturnSharpe = pReturnSharpe - SPYreturn

pnl_BA = computePNL(aReturnSharpe,pReturnSharpe)

load("XOM_final.RData")
aReturnSharpe = stock_price_final[102:nrow(stock_price_final),2]
pReturn = predSlideWindow(n)
pReturnSharpe = pReturn[1:(length(pReturn)-1)]

len = length(aReturnSharpe)
length(SPYreturn) = len
aReturnSharpe = aReturnSharpe - SPYreturn
pReturnSharpe = pReturnSharpe - SPYreturn

pnl_XOM = computePNL(aReturnSharpe,pReturnSharpe)

load("AAPL_final.RData")
aReturnSharpe = stock_price_final[102:nrow(stock_price_final),2]
pReturn = predSlideWindow(n)
pReturnSharpe = pReturn[1:(length(pReturn)-1)]

len = length(aReturnSharpe)
length(SPYreturn) = len
aReturnSharpe = aReturnSharpe - SPYreturn
pReturnSharpe = pReturnSharpe - SPYreturn

pnl_AAPL = computePNL(aReturnSharpe,pReturnSharpe)

minLength = min(length(pnl_IBM), 
                length(pnl_AAPL), 
                length(pnl_MSFT), 
                length(pnl_AMZN),
                length(pnl_BA),
                length(pnl_XOM))

length(pnl_AAPL) = minLength
length(pnl_IBM) = minLength
length(pnl_MSFT) = minLength
length(pnl_AMZN) = minLength
length(pnl_BA) = minLength
length(pnl_XOM) = minLength

pnl_total = cbind(pnl_IBM,pnl_AAPL,pnl_MSFT,pnl_AMZN,pnl_BA,pnl_XOM) 

pnl = rowSums(pnl_total)
# load("SPYReturn.RData")
mean_pnl = mean(pnl)
std_pnl = sqrt(var(pnl))
Sharpe = mean_pnl*sqrt(252)/std_pnl
averageSharpe = sum(pnl)/(minLength*6)
#The Portfolio Sharpe ratio is:
print(Sharpe)
#The average Sharpe ratio is:
print(averageSharpe)



