#mengimpor library yang diperlukan
library('YieldCurve')

#membersih direktori dan memori (environment)
rm(list=ls()) 

## Nelson-Siegel-Svensson
SV=function(yield,maturity)
{
  func <- function(x,yield,maturity){
    (0.5*sum((yield-x[1]-x[2]*exp(-maturity/x[5])-x[3]*maturity/x[5]*exp(-maturity/x[5])-
                x[4]*maturity/x[6]*exp(-maturity/x[6]))^2))
  } 
  fit=optim(par=c(0,0,0,0,1,1), fn=func, yield=yield, maturity=maturity, gr = NULL,
            method = c("L-BFGS-B"),
            lower = c(0,-Inf,-Inf,-Inf,0,0), upper = rep(Inf,6),
            control = list(maxit=500))
  names(fit$par)=c("beta0","beta1","beta2","beta3","tau1","tau2")
  x=fit$par
  yfit=x[1]+x[2]*exp(-maturity/x[5])+x[3]*maturity/x[5]*exp(-maturity/x[5])+
    x[4]*maturity/x[6]*exp(-maturity/x[6])
  mse=mean((yield-yfit)^2)
  plot(maturity,yield,ylim=c(min(c(yield,yfit)),max(c(yield,yfit))),
       main="NSS fit at 08/06/22", ##
       xlab="Time to Maturity (year)",
       ylab="Yield to Maturity",sub="MSE: 3.949956e-06") ##
  lines(maturity,yfit,col=2)
  fit
  cat("MSE :", mse,"\n")
  return(x)
}

#mengimpor file CSV
dataall2<-read.csv("D:\\Data SBN 19-22.csv")
head(dataall2)
str(dataall2)

#membagi layar R:Graphics menjadi 2 kolom dan 2 baris
par(mfrow=c(3,2)) 

## bulan Maret 2022
#NSS fit at 02/03/22
data.1=dataall2[which(dataall2$Trade.Date=="02/03/22"),]
data.1$Yield=as.numeric(data.1$Yield)
attach(data.1)
SV(Yield,Tenor)
Svensson(rate=Yield,maturity=Tenor)
detach(data.1)

#NSS fit at 03/03/22
data.2=dataall2[which(dataall2$Trade.Date=="03/03/22"),]
data.2$Yield=as.numeric(data.2$Yield)
attach(data.2)
SV(Yield,Tenor)
Svensson(rate=Yield,maturity=Tenor)
detach(data.2)

#NSS fit at 04/03/22
data.3=dataall2[which(dataall2$Trade.Date=="04/03/22"),] 
data.3$Yield=as.numeric(data.3$Yield)
attach(data.3)
SV(Yield,Tenor)
Svensson(rate=Yield,maturity=Tenor)
detach(data.3)

#NSS fit at 07/03/22
data.4=dataall2[which(dataall2$Trade.Date=="07/03/22"),]
data.4$Yield=as.numeric(data.4$Yield)
attach(data.4)
SV(Yield,Tenor)
Svensson(rate=Yield,maturity=Tenor)
detach(data.4)

#NSS fit at 08/03/22
data.5=dataall2[which(dataall2$Trade.Date=="08/03/22"),] 
data.5$Yield=as.numeric(data.5$Yield)
attach(data.5)
SV(Yield,Tenor)
Svensson(rate=Yield,maturity=Tenor)
detach(data.5)


## bulan Juni 2022

#NSS fit at 02/06/22
data1=dataall2[which(dataall2$Trade.Date=="02/06/22"),]
data1$Yield=as.numeric(data1$Yield)
attach(data1)
SV(Yield,Tenor)
Svensson(rate=Yield,maturity=Tenor)
detach(data1)

#NSS fit at 03/06/22
data2=dataall2[which(dataall2$Trade.Date=="03/06/22"),]
data2$Yield=as.numeric(data2$Yield)
attach(data2)
SV(Yield,Tenor)
Svensson(rate=Yield,maturity=Tenor)
detach(data2)

#NSS fit at 06/06/22
data3=dataall2[which(dataall2$Trade.Date=="06/06/22"),] 
data3$Yield=as.numeric(data3$Yield)
attach(data3)
SV(Yield,Tenor)
Svensson(rate=Yield,maturity=Tenor)
detach(data3)

#NSS fit at 07/06/22
data4=dataall2[which(dataall2$Trade.Date=="07/06/22"),]
data4$Yield=as.numeric(data4$Yield)
attach(data4)
SV(Yield,Tenor)
Svensson(rate=Yield,maturity=Tenor)
detach(data4)

#NSS fit at 08/06/22
data5=dataall2[which(dataall2$Trade.Date=="08/06/22"),] 
data5$Yield=as.numeric(data5$Yield)
attach(data5)
SV(Yield,Tenor)
Svensson(rate=Yield,maturity=Tenor)
detach(data5)


#menggunakan library('YieldCurve')
# Svensson(rate=Yield,maturity=Tenor)
#dengan m = 1/lambda
# Nelson.Siegel(rate=Yield,maturity=Tenor)
#dengan m=lambda dan pada estimasi koefisien, nilai pendekatan tau1 = 10*lambda 


## Nelson-Siegel
NS=function(yield,maturity)
{
  func <- function(x,yield,maturity){
    (0.5*sum((yield-x[1]-x[2]*exp(-maturity/x[4])-x[3]*maturity/x[4]*exp(-maturity/x[4]))^2))
  } 
  fit=optim(par=c(0,0,0,1), fn=func, yield=yield, maturity=maturity, gr = NULL,
            method = c("L-BFGS-B"),
            lower = c(0,-Inf,-Inf,0), upper = rep(Inf,4),
            control = list(maxit=500))
  names(fit$par)=c("beta0","beta1","beta2","tau1")
  x=fit$par
  yfit=x[1]+x[2]*exp(-maturity/x[4])+x[3]*maturity/x[4]*exp(-maturity/x[4])
  mse=mean((yield-yfit)^2)
  plot(maturity,yield,ylim=c(min(c(yield,yfit)),max(c(yield,yfit))),
       main="NS fit at 08/06/22",
       xlab="Time to Maturity (year)",
       ylab="Yield to Maturity",
       sub="MSE: 3.949962e-06")
  lines(maturity,yfit,col=2)
  fit
  cat("MSE :", mse,"\n")
  return(x)
}

#membagi layar R:Graphics menjadi 2 kolom dan 2 baris
par(mfrow=c(3,2)) 


## bulan Maret 2022

#NS fit at 02/03/22
attach(data.1)
NS(Yield,Tenor)
Nelson.Siegel(rate=Yield,maturity=Tenor)
detach(data.1)

#NS fit at 03/03/22
attach(data.2)
NS(Yield,Tenor)
Nelson.Siegel(rate=Yield,maturity=Tenor)
detach(data.2)

#NS fit at 04/03/22
attach(data.3)
NS(Yield,Tenor)
Nelson.Siegel(rate=Yield,maturity=Tenor)
detach(data.3)

#NS fit at 07/03/22
attach(data.4)
NS(Yield,Tenor)
Nelson.Siegel(rate=Yield,maturity=Tenor)
detach(data.4)

#NS fit at 08/03/22
attach(data.5)
NS(Yield,Tenor)
Nelson.Siegel(rate=Yield,maturity=Tenor)
detach(data.5)


## bulan Juni 2022

#NS fit at 02/06/22
attach(data1)
NS(Yield,Tenor)
Nelson.Siegel(rate=Yield,maturity=Tenor)
detach(data1)

#NS fit at 03/06/22
attach(data2)
NS(Yield,Tenor)
Nelson.Siegel(rate=Yield,maturity=Tenor)
detach(data2)

#NS fit at 06/06/22
attach(data3)
NS(Yield,Tenor)
Nelson.Siegel(rate=Yield,maturity=Tenor)
detach(data3)

#NS fit at 07/06/22
attach(data4)
NS(Yield,Tenor)
Nelson.Siegel(rate=Yield,maturity=Tenor)
detach(data4)

#NS fit at 08/06/22
attach(data5)
NS(Yield,Tenor)
Nelson.Siegel(rate=Yield,maturity=Tenor)
detach(data5)


## tanggal di bulan Maret
date.m=c("02/03/22",	"03/03/22",	"04/03/22",	"07/03/22",	"08/03/22")

## tanggal di bulan Juni
date.j=c("02/06/22",	"03/06/22",	"06/06/22",	"07/06/22",	"08/06/22")

## MSE - NSS
SV3=function(dataall=dataall2, date)
  {
  #Gunakan dataall dari file .csv
  par(mfrow=c(2,2))
  MSE=c()
  for(i in date)
  {
    data=dataall[which(dataall$Trade.Date==i),]
    data$Yield=as.numeric(data$Yield)
    attach(data)
    func <- function(x,yield,maturity){
      (0.5*sum((yield-x[1]-x[2]*exp(-maturity/x[5])-x[3]*maturity/x[5]*exp(-maturity/x[5])-
                  x[4]*maturity/x[6]*exp(-maturity/x[6]))^2))
    } 
    fit=optim(par=c(0,0,0,0,1,1), fn=func, yield=Yield, maturity=Tenor, gr = NULL,
              method = c("L-BFGS-B"),
              lower = c(0,-Inf,-Inf,-Inf,0,0), upper = rep(Inf,6),
              control = list(maxit=500))
    names(fit$par)=c("beta0","beta1","beta2","beta3","tau1","tau2")
    x=fit$par
    yfit=x[1]+x[2]*exp(-Tenor/x[5])+x[3]*Tenor/x[5]*exp(-Tenor/x[5])+
      x[4]*Tenor/x[6]*exp(-Tenor/x[6])
    mse=mean((Yield-yfit)^2)
    MSE[i]=mse
  }
  df=data.frame('Date'=date,'MSE.NSS'=MSE)
  rownames(df)=NULL
  detach(data)
  #return(list('df'=df,'params'=x))
  return(df)
}

## MSE - NS
NS3=function(dataall=dataall2,date)
{
  #Gunakan dataall dari file .csv
  par(mfrow=c(2,2))
  MSE=c()
  for(i in date)
  {
    data=dataall[which(dataall$Trade.Date==i),]
    data$Yield=as.numeric(data$Yield)
    attach(data)
    func <- function(x,yield,maturity){
      (0.5*sum((yield-x[1]-x[2]*exp(-maturity/x[4])-x[3]*maturity/x[4]*exp(-maturity/x[4]))^2))
    } 
    fit=optim(par=c(0,0,0,1), fn=func, yield=Yield, maturity=Tenor, gr = NULL,
              method = c("L-BFGS-B"),
              lower = c(0,-Inf,-Inf,0), upper = rep(Inf,4),
              control = list(maxit=500))
    names(fit$par)=c("beta0","beta1","beta2","tau1")
    x=fit$par
    yfit=x[1]+x[2]*exp(-Tenor/x[4])+x[3]*Tenor/x[4]*exp(-Tenor/x[4])
    mse=mean((Yield-yfit)^2)
    MSE[i]=mse
  }
  df=data.frame('Date'=date,'MSE.NS'=MSE)
  rownames(df)=NULL
  detach(data)
  return(df)
}

#Data MSE bulan Maret 2022
nss.2=SV3(dataall2,date=date.m)
ns.2=NS3(dataall2,date=date.m)
dif1=nss.2$MSE.NSS-ns.2$MSE.NS ##Selisih MSE NSS dan MSE NS (lebih dari 0 berarti NS lebih baik, dan sebaliknya)

#Data MSE bulan Juni 2022
nss2=SV3(dataall2,date=date.j)
ns2=NS3(dataall2,date=date.j)
dif2=nss2$MSE.NSS-ns2$MSE.NS ##Selisih MSE NSS dan MSE NS (lebih dari 0 berarti NS lebih baik, dan sebaliknya)

MSE_NSS=c(nss.2$MSE.NSS,nss2$MSE.NSS)
MSE_NS=c(ns.2$MSE.NS,ns2$MSE.NS)
difference=c(dif1,dif2)
Date=c(date.m,date.j)

table2=data.frame(Date,'MSE.NSS'=MSE_NSS,'MSE.NS'=MSE_NS,'Difference'=difference)
table2
write.csv(table2,'D:\\MSE Maret Juni 2022.csv',row.names=FALSE)

