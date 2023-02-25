#Financial Time Series
#packages: ggplot2,TTR

#Southern Oscillation Index (SOI) (indicator of intensity of El Nino effect)
#http://www.bom.gov.au/climate/current/soihtm1.shtml
#The aim is to detect patterns
SOI = read.delim("SOI.txt", header = TRUE, sep = "", dec = ".");
print(SOI);
X = c(t(as.matrix(SOI[,2:13])));
dt = seq(as.Date("1876/1/1"), as.Date("2018/12/1"), "months");
SOI = data.frame(dt,X);
SOIg = ggplot(SOI, aes(x = dt, y = X)) + geom_line() +
  labs(title="Southern Oscillation Index", x="Date", y="Value") +
  scale_x_date(breaks = pretty(SOI$dt,n=10),date_labels = "%Y") +
  scale_y_continuous(breaks = pretty(SOI$X,n=10));
#scale_x_date(date_breaks = "25 years",date_labels = "%b %Y")
#scale_x_date(limits = c(dt[10], dt[20]))
SOIg

#Average number of sunspots
#http://www.sidc.be/silso/infosnytot
#GOAL: forecast future activity
SUNSPOTS = read.csv("sunspots.csv",sep=";");
X = SUNSPOTS[,2];
dt = seq(as.Date("1701/1/1"), as.Date("2018/12/1"), "years");
SUNSPOTS = data.frame(dt,X)
SUNSPOTSg = ggplot(SUNSPOTS, aes(x = dt, y = X)) + geom_line() +
  labs(title="Average Numbers of Sunspots", x="Date", y="Sunspots") +
  scale_x_date(breaks = pretty(SUNSPOTS$dt,n=10),date_labels = "%Y") +
  scale_y_continuous(breaks = pretty(SUNSPOTS$X,n=10))
SUNSPOTSg

#Yearly Temperature Anomalies
#https://data.giss.nasa.gov/gistemp/graphs/
TEMP= read.csv("temperature.csv",sep=",");
lowess = TEMP[,3]
X = TEMP[,2];
dt = seq(as.Date("1880/1/1"), as.Date("2017/12/1"), "years");
TEMP = data.frame(dt,X)
TEMPg = ggplot(TEMP, aes(x = dt, y = X)) + geom_line() +
  labs(title="Yearly Temperature Anomalies", x="Date", y="Yearly Temperature Anomaly") +
  scale_x_date(breaks = pretty(TEMP$dt,n=10),date_labels = "%Y") +
  scale_y_continuous(breaks = pretty(TEMP$X,n=10))
TEMPg

#Standard and Poor's 500 (500 largest U.S. publicly traded companies by market value)
SP500 = read.table("sp500.txt");
X = SP500$V1;
dt = seq(as.Date("1926/1/1"), as.Date("1991/12/1"), "months");
SP500 = data.frame(dt,X);
SP500g = ggplot(SP500, aes(x = dt, y = X)) + geom_line() +
  labs(title="S&P500 Monthly Returns", x="Date", y="Monthly Return") +
  scale_x_date(breaks = pretty(SP500$dt,n=10),date_labels = "%Y") +
  scale_y_continuous(breaks = pretty(SP500$X,n=10));
SP500g

#EUR/GBP Exchange Rate
FX = read.csv("FX.csv",sep=",");
print(FX);
FX = as.matrix(FX);
FX = FX[FX[,2]!="ND",];
dt = as.character(FX[,1]);
print(dt[1:10]);
dt = as.Date(dt,format = "%d/%m/%Y")
X = as.numeric(FX[,3])/as.numeric(FX[,2]);
FX = data.frame(dt,X)
FXg = ggplot(FX, aes(x = dt, y = X)) + geom_line() +
  labs(title="EUR/GBP Dayly Exchange Rates", x="Date", y="Exchange Rate") +
  scale_x_date(breaks = pretty(FX$dt,n=10),date_labels = "%Y") +
  scale_y_continuous(breaks = pretty(FX$X,n=10))
FXg

#Parametric Detrending
#Fit a linear trend
T = length(TEMP$dt);
t = 1:T;
lM = lm(TEMP$X~t)
lreg = lM$coefficients[1]+t*lM$coefficients[2];
G1 = TEMPg + geom_line(aes(TEMP$dt,lreg),color="green",size=1.2);
G1;
lCloseness = sqrt(sum((TEMP$X-lreg)^2)/T);
print(lCloseness);
lRoughness = sum((lreg[1:(T-3)]-3*lreg[2:(T-2)]+3*lreg[3:(T-1)]-lreg[4:T])^2)
print(lRoughness);
#Fit a quadratic trend
qM = lm(TEMP$X~cbind(t,t^2));
qreg = qM$coefficients[1]+t*qM$coefficients[2]+t^2*qM$coefficients[3];
G2 = G1 + geom_line(aes(TEMP$dt,qreg),color="red",size=1.2);
G2;
qCloseness = sqrt(sum((TEMP$X-qreg)^2)/T);
print(qCloseness);
qRoughness = sum((qreg[1:(T-3)]-3*qreg[2:(T-2)]+3*qreg[3:(T-1)]-qreg[4:T])^2)
print(qRoughness);
#Closeness is improved but... doesn't make much sense!
#A periodic trend would be ideal for monthly data

#Non-parametric detrending, Simple Returns and Log Returns
SP500g
#get the log return
x = log(1+SP500$X)
#plot the original series
p = cumsum(x);
P = exp(p);
ggplot(SP500,aes(x = SP500$dt, y = P)) + geom_line() +
  labs(title="S&P500 Values", x="Date", y="Value") +
  scale_x_date(breaks = pretty(SP500$dt,n=10),date_labels = "%Y") +
  scale_y_continuous(breaks = pretty(P,n=10));
SP500g2 = ggplot(SP500,aes(x = SP500$dt, y = p)) + geom_line() +
  labs(title="S&P500 Values", x="Date", y="Value") +
  scale_x_date(breaks = pretty(SP500$dt,n=10),date_labels = "%Y") +
  scale_y_continuous(breaks = pretty(p,n=10));

#Exponential Moving Average on log prices
strend = EMA(p,1,n=6);
ltrend = EMA(p,1,n=24);
SP500g2 + geom_line(aes(SP500$dt,strend),color="green",size=1.2) +
  geom_line(aes(SP500$dt,ltrend),color="orange",size=1.2)

#Exponential Moving Average on log returns
#(relatively more interesting)
strend = EMA(x,1,n=6);
ltrend = EMA(x,1,n=24);
SP500g3 = SP500g + geom_line(aes(SP500$dt,strend),color="green",size=1.2) +
  geom_line(aes(SP500$dt,ltrend),color="orange",size=1.2)
SP500g3;

#Try with different time windows
SP500g3 + scale_x_date(limits = c(SP500$dt[550],SP500$dt[650]))

#When the short trend > long-term trend the security has 'momentum'
momentum =  strend-ltrend
ggplot(SP500,aes(x = SP500$dt, y = momentum)) + geom_line(color="blue") +
  labs(title="S&P500 Momentum", x="Date", y="Momentum") +
  scale_x_date(breaks = pretty(SP500$dt,n=10),date_labels = "%Y") +
  scale_y_continuous(breaks = pretty(momentum,n=10)) + 
  geom_hline(yintercept=0,linetype=2,size=1.2);

#Differencing - the standard technique for detrending financial time series.
ggplot(SP500,aes(x = SP500$dt, y = P)) + geom_line() +
  labs(title="S&P500 Values", x="Date", y="Value") +
  scale_x_date(breaks = pretty(SP500$dt,n=10),date_labels = "%Y") +
  scale_y_continuous(breaks = pretty(P,n=10));
T = length(P);
#Simple Returns
r = (P[2:T]-P[1:(T-1)])/P[1:(T-1)]
#Log Returns
x = diff(log(P))
ggplot(SP500[2:T,],aes(x = dt, y = r)) + geom_line() +
  labs(title="S&P500 Values", x="Date", y="Value") +
  scale_x_date(breaks = pretty(SP500$dt,n=10),date_labels = "%Y") +
  scale_y_continuous(breaks = pretty(r,n=10)) +
  geom_line(aes(dt,x),color="red",size=0.3)

#Typical features of returns
library(fBasics) # basicStats below is avialable from this package
da = read.table("d-ibm3dx7008.txt",header=T)
da[1:3,]
ibm = da[,2]
sibm = 100*ibm
basicStats(sibm)
s1 = skewness(sibm)[1]
t1 = s1/sqrt(6/length(sibm))
t1
#t1 is asymptotically distributed according to a standard normal
pv = 2*(1-pnorm(t1))
pv
#In this case, skewness is significantly different from zero
libm = log(ibm+1)*100
t.test(libm)
normalTest(libm,method="jb")
