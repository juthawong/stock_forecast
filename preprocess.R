#############################################################
###############  Perform data transformation  ###############
#############################################################
#1. Compute stock returns over specific periods of time

#1.1 function that calculculate the interpolation of target maturity
linear_interpolate = function(val1,val2,pos1,pos2,pos_target){
  weight_p1 = (pos2-pos_target)/(pos2-pos1);
  weight_p2 = (pos_target - pos1)/(pos2-pos1);
  valtarget = val1*weight_p1+val2*weight_p2;
  return(valtarget)
}

#1.2 function that locates the target maturity
determine_gap = function(m,n,mature=10){
  i = 1;
  #AAPL and IBM
  if (mature ==10) {       
    while(10+(n-1)*i<=m) i = i+1;
  }
  #MFST and etc.
  else if (mature ==25) {
    while(25+(n-1)*i<=m) i = i+1;
  }
  return(i-1);
}

#1.3 function that compute the interpolation of greeks for one row of data
compute_interpolation_greeks = function(inputrow,dahead=60,numpt=10,m=10){
  #'''First choose the number of days ahead we want to predict, and the number of option prices to interpolate. '''
  days_ahead = dahead;
  number_of_points = numpt;      
  inputlist = as.numeric(inputrow)[(numpt+3):(number_of_points*6+2)]
  
  #'''Calculate the number of days between two interpolated option prices.'''
  
  gap_between_points = determine_gap(days_ahead,number_of_points,mature=m);   
  #AAPL and IBM
  if (m==10) {       
    interpolated_maturity_value = seq(from = 10, by= gap_between_points, length.out = number_of_points)
  }
  #MSFT and etc.
  else if (m==25) {
    interpolated_maturity_value = seq(from = 25, by= gap_between_points, length.out = number_of_points)
  }
  #'''Output pricelist'''     
  output_pricelist = vector(length=(number_of_points*4))  # 4 greeks for each date
  breakpoint = number_of_points
  knownmatpoints = inputlist[1:breakpoint]
  
  for(j in 1:4){
    for(i in 1:(length(output_pricelist)/4)){
      if(interpolated_maturity_value[i] %in% inputlist[1:number_of_points]){
        output_pricelist[4*i-4+j]=inputlist[breakpoint + 4 * (which(inputlist[1:breakpoint] == interpolated_maturity_value[i])-1) +j]
      }else{
        position = length(knownmatpoints[(0<knownmatpoints)&(knownmatpoints<interpolated_maturity_value[i])])
        if(position == 0) print("Invalid interpolated maturity value detected!")
        output_pricelist[4*i-4+j] = linear_interpolate(inputlist[number_of_points+4*position-4+j],inputlist[number_of_points+4*position-4+j+4],
                                                       inputlist[position],inputlist[position+1],
                                                       interpolated_maturity_value[i])  
      }
    }      
  }      
  return(output_pricelist)
}


#1.4 function that compute the interpolation of option prices for one row of data
compute_interpolation_price = function(inputrow,dahead=60,numpt=10,m=10){
  #'''First choose the number of days ahead we want to predict, and the number of option prices to interpolate. '''
  days_ahead = dahead;
  number_of_points = numpt;      
  inputlist = as.numeric(inputrow)[3:(number_of_points*2+2)]
  
  #'''Calculate the number of days between two interpolated option prices.'''
  
  gap_between_points = determine_gap(days_ahead,number_of_points);     
  #AAPL and IBM
  if (m==10) {       
    interpolated_maturity_value = seq(from = 10, by= gap_between_points, length.out = number_of_points)
  }
  #MSFT and etc.
  else if (m==25) {
    interpolated_maturity_value = seq(from = 25, by= gap_between_points, length.out = number_of_points)
  }
  #'''Output pricelist'''
  
  output_pricelist = vector(length=number_of_points)
  breakpoint = length(inputlist)/2
  endhalfinputlist = inputlist[(breakpoint+1):length(inputlist)]
  
  for(i in 1:length(output_pricelist)){
    if(interpolated_maturity_value[i] %in% inputlist[breakpoint+1:length(inputlist)]){
      output_pricelist[i]=inputlist[which(inputlist[(breakpoint+1):length(inputlist)] == interpolated_maturity_value[i])]
    }else{
      position = length(endhalfinputlist[(0<endhalfinputlist)&(endhalfinputlist<interpolated_maturity_value[i])])
      if(position == 0) print("Invalid interpolated maturity value detected!")
      output_pricelist[i] = linear_interpolate(inputlist[position],inputlist[position+1],
                                               inputlist[breakpoint+position],inputlist[breakpoint+position+1],
                                               interpolated_maturity_value[i])  
    }
  }
  return(output_pricelist)
}


#2 Modify data frames and merge them after interpolation

#2.1 function that adjust data structure and perform the main data transformation algorithm
data_mdf<-function(AAPL,stock_price,company="AAPL") {

#determine company
if(company == "AAPL" | company =="IBM") {
  mature=10
}
else {
  mature=25
}

##Load and edit Data
greeks=10:13
colnames(stock_price)=c("date",colnames(stock_price)[-1])
stock_price$date=as.Date(stock_price$date,format="%Y-%m-%d")
stock_price=stock_price[,c(1,5)]
AAPL$date=as.Date(as.character(AAPL$date),format="%Y%m%d")
AAPL$exdate=as.Date(as.character(AAPL$exdate),format="%Y%m%d")
AAPL$mature=AAPL$exdate-AAPL$date
AAPL_t=AAPL[apply(AAPL[,greeks],1,function(x) sum(is.na(x)))==0,]
AAPL=AAPL_t
flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  return(new)
}
stock_price=flip(stock_price)


stock_price=stock_price[6:dim(stock_price)[1],]
rownames(stock_price)=NULL

#AAPL$strike_price[AAPL$date<as.Date("2014-06-19")]=AAPL$strike_price[AAPL$date<as.Date("2014-06-19")]/7
#stock_price$Close[stock_price$date<as.Date("2014-06-19")]=stock_price$Close[stock_price$date<as.Date("2014-06-19")]/7
stock_forreturn=stock_price

AAPL_C=AAPL[AAPL$cp_flag=="C",]
AAPL_P=AAPL[AAPL$cp_flag=="P",]

#AAPL_P_tmp=AAPL_P[AAPL_P$mature<=60,]
#min(with(AAPL_P_tmp,tapply(AAPL_P_tmp$mature,AAPL_P_tmp$date,function(x) length(unique(x)))))



##Set constants
Date=stock_price$date
a=10
b=10
colstock=dim(stock_price)[2]
range=60
datelength=500


##leave space in the matrix
stock_price[,(colstock+1):(colstock+a)]=rep(0)
stock_price[,(colstock+a+1):(colstock+a+b)]=rep(0)
stock_price[,(colstock+a+b+1):(colstock+a+b+4*10)]=rep(0)


stock_price_C=stock_price
stock_price_P=stock_price

##assign p points
for(i in 1:datelength) {
  tmp=AAPL_C[which(AAPL_C$date==Date[i]),]
  matr=unique(tmp$mature)
  matr=c(matr[matr<=range],matr[matr>range][1])
  fprice=stock_price_C$Close[i]
  
  for(j in 1:length(matr)) {
    tmp_tmp=tmp[which(tmp$mature==matr[j]),]
    pdiff=abs(tmp_tmp$strike_price/1000-fprice)
    stock_price_C[i,(colstock+j)]=(tmp_tmp[pdiff==min(pdiff),]$best_bid[1]+tmp_tmp[pdiff==min(pdiff),]$best_offer[1])/2
    stock_price_C[i,(colstock+a+j)]=matr[j]
    stock_price_C[i,(colstock+a+b+(j-1)*4+1):(colstock+a+b+j*4)]=tmp_tmp[pdiff==min(pdiff),greeks][1,]
  }
}

for(i in 1:datelength) {
  tmp=AAPL_P[which(AAPL_P$date==Date[i]),]
  matr=unique(tmp$mature)
  matr=c(matr[matr<=range],matr[matr>range][1])
  fprice=stock_price_P$Close[i]
  
  for(j in 1:length(matr)) {
    tmp_tmp=tmp[which(tmp$mature==matr[j]),]
    pdiff=abs(tmp_tmp$strike_price/1000-fprice)
    stock_price_P[i,(colstock+j)]=(tmp_tmp[pdiff==min(pdiff),]$best_bid[1]+tmp_tmp[pdiff==min(pdiff),]$best_offer[1])/2
    stock_price_P[i,(colstock+a+j)]=matr[j]
    stock_price_P[i,(colstock+a+b+(j-1)*4+1):(colstock+a+b+j*4)]=tmp_tmp[pdiff==min(pdiff),greeks][1,]
  }
}




#compute return function
compute_return<-function(x,d,a) {
  x_t=x[d:dim(x)[1],]
  n=dim(x_t)[1]
  x1=x_t[1:(n-a),2]
  x2=x_t[(a+1):n,2]
  return(log(x2)-log(x1))
}



#call interpulation function
stock_c_tmp=stock_price_C[1:500,]
stock_price_ctmpa=t(apply(stock_c_tmp,1,function(x) compute_interpolation_price(x,m=mature)))
stock_price_ctmpb=t(apply(stock_c_tmp,1,function(x) compute_interpolation_greeks(x,m=mature)))
stock_price_C=cbind(stock_price_C[1:500,1:2],stock_price_ctmpa,stock_price_ctmpb)


stock_p_tmp=stock_price_P[1:500,]
stock_price_ptmpa=t(apply(stock_p_tmp,1,function(x) compute_interpolation_price(x,m=mature)))
stock_price_ptmpb=t(apply(stock_p_tmp,1,function(x) compute_interpolation_greeks(x,m=mature)))
stock_price_P=cbind(stock_price_P[1:500,1:2],stock_price_ptmpa,stock_price_ptmpb)



#get response variable
response=matrix(rep(0),nrow=500)
for(i in 1:datelength) {
  response[i,]=stock_price[(Date-Date[i])>=1,]$Close[1]
}


#set return
ta=compute_return(stock_forreturn,1,1)
ta=ta[5:(5+500-1)]
tb=compute_return(stock_forreturn,1,2)
tb=tb[4:(4+500-1)]
tc=compute_return(stock_forreturn,1,5)
tc=tc[1:(1+500-1)]


#cbind final data set
stock_price_final=cbind(stock_price_C,stock_price_P[,-(1:2)],ta,tb,tc,response)
names(stock_price_final)=c("date","Close","P_C1","P_C2","P_C3","P_C4","P_C5","P_C6","P_C7","P_C8","P_C9","P_C10","C1delta","C1gamma","C1vega","C1theta","C2delta","C2gamma","C2vega","C2theta","C3delta","C3gamma","C3vega","C3theta","C4delta","C4gamma","C4vega","C4theta","C5delta","C5gamma","C5vega","C5theta","C6delta","C6gamma","C6vega","C6theta","C7delta","C7gamma","C7vega","C7theta","C8delta","C8gamma","C8vega","C8theta","C9delta","C9gamma","C9vega","C9theta","C10delta","C10gamma","C10vega","C10theta",
"P_P1","P_P2","P_P3","P_P4","P_P5","P_P6","P_P7","P_P8","P_P9","P_P10","P1delta","P1gamma","P1vega","P1theta","P2delta","P2gamma","P2vega","P2theta","P3delta","P3gamma","P3vega","P3theta","P4delta","P4gamma","P4vega","P4theta","P5delta","P5gamma","P5vega","P5theta","P6delta","P6gamma","P6vega","P6theta","P7delta","P7gamma","P7vega","P7theta","P8delta","P8gamma","P8vega","P8theta","P9delta","P9gamma","P9vega","P9theta","P10delta","P10gamma","P10vega","P10theta","return1","return2","return5","response")

return(stock_price_final)
}

#2.2 function that perform final process that modify data to return space
library(quantmod)
preprocess = function(name){
  # get date
  AAPL = as.data.frame(stock_price_final[2:nrow(stock_price_final),1])
  colnames(AAPL) = "date"
  
  # data = stock_price_final[1:441,]
  data = stock_price_final
  stockReturnToday = with(data, diff(log(data$Close)))
  AAPL$stk_ret_td = stockReturnToday
  
  PC = data[,3:12]
  cRet = matrix(nrow = (nrow(data)-1),ncol = 10)
  for(i in 1:10) cRet[,i] = with(PC, diff(log(PC[,i])))
  AAPL = cbind(AAPL,as.data.frame(cRet))
  
  PP = data[,53:62]
  pRet = matrix(nrow = (nrow(data)-1),ncol = 10)
  for(i in 1:10) pRet[,i] = with(PP, diff(log(PP[,i])))
  
  Cgreek = data[,13:52]
  changeGreekC = matrix(nrow = (nrow(data)-1),ncol = 40)
  for(i in 1:40) changeGreekC[,i] = Delt(Cgreek[,i])[2:nrow(data)]
  
  pGreek = data[,63:102]
  changeGreekP = matrix(nrow = (nrow(data)-1),ncol = 40)
  for(i in 1:40) changeGreekP[,i] = Delt(pGreek[,i])[2:nrow(data)]
  
  AAPL = cbind(AAPL,changeGreekC,pRet,changeGreekP)
  
  names(AAPL)[3:12] = names(stock_price_final)[3:12]
  names(AAPL)[13:52] = names(stock_price_final)[13:52]
  names(AAPL)[53:62] = names(stock_price_final)[53:62]
  names(AAPL)[63:102] = names(stock_price_final)[63:102]
  
  leng = nrow(AAPL)
  return2 = stock_price_final[2:leng,104]
  return5 = stock_price_final[2:leng,105]
  
  AAPL_final = cbind(AAPL[1:(nrow(AAPL)-1),],return2,return5,AAPL$stk_ret_td[2:nrow(AAPL)])
  names(AAPL_final)[105]= "response"
  stock_price_final = AAPL_final
  save(stock_price_final,file=paste(name,".RData",sep = ""))
}


#3 Modify data and save the transformed data

setwd("C:/Users/Peter/Desktop/Math 191/Project")
#AAPL=read.csv("AAPL.csv")
load("AAPL.RData")
stock_price=read.csv("AAPL_stock.csv")
stock_price_final=data_mdf(AAPL,stock_price,company="AAPL")
preprocess("AAPL")


load("AMZN.RData")
stock_price=read.csv("AMZN_stock.csv")
stock_price_final=data_mdf(AAPL,stock_price,company="AMZN")
preprocess("AMZN")

load("BA.RData")
stock_price=read.csv("BA_stock.csv")
stock_price_final=data_mdf(AAPL,stock_price,company="BA")
preprocess("BA")


load("IBM.RData")
stock_price=read.csv("IBM_stock.csv")
stock_price_final=data_mdf(AAPL,stock_price,company="IBM")
preprocess("IBM")

load("MSFT.RData")
stock_price=read.csv("MSFT_stock.csv")
stock_price_final=data_mdf(AAPL,stock_price,company="MSFT")
preprocess("MSFT")

load("XOM.RData")
stock_price=read.csv("XOM_stock.csv")
stock_price_final=data_mdf(AAPL,stock_price,company="XOM")
preprocess("XOM")


