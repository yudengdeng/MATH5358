#Chapter 3
rm(list = ls())
anscombe <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/anscombe.txt",header=TRUE)
attach(anscombe)
head(anscombe)

#Figure 3.1 on page 46
par(mfrow=c(2,2))
plot(x1,y1,xlim=c(4,20),ylim=c(3,14),main="Data Set 1")
abline(lsfit(x1,y1))
plot(x2,y2,xlim=c(4,20),ylim=c(3,14),main="Data Set 2")
abline(lsfit(x2,y2))
plot(x3,y3,xlim=c(4,20),ylim=c(3,14),main="Data Set 3")
abline(lsfit(x3,y3))
plot(x4,y4,xlim=c(4,20),ylim=c(3,14),main="Data Set 4")
abline(lsfit(x4,y4))

#Regression output on page 47
m1 <- lm(y1~x1)
summary(m1)
m2 <- lm(y2~x2)
summary(m2)
m3 <- lm(y3~x3)
summary(m3)
m4 <- lm(y4~x4)
summary(m4)

#Figure 3.2 on page 48
par(mfrow=c(2,2))
plot(x1,m1$residuals,ylab="Residuals",xlim=c(4,20),ylim=c(-3.5,3.5),main="Data Set 1")
plot(x2,m2$residuals,ylab="Residuals",xlim=c(4,20),ylim=c(-3.5,3.5),main="Data Set 2")
plot(x3,m3$residuals,ylab="Residuals",xlim=c(4,20),ylim=c(-3.5,3.5),main="Data Set 3")
plot(x4,m4$residuals,ylab="Residuals",xlim=c(4,20),ylim=c(-3.5,3.5),main="Data Set 4")

#Figure 3.3 on page 50
par(mfrow=c(1,2))
plot(x2,y2,,ylim=c(3,10))
abline(lsfit(x2,y2))
plot(x2,m2$residuals,ylab="Residuals",ylim=c(-2,2),main="Data Set 2")

detach(anscombe)

#Huber's Example
huber <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/huber.txt",header=TRUE)
attach(huber)
head(huber)

#Regression output on page 54
mBad <- lm(YBad~x)
summary(mBad)
mGood <- lm(YGood~x)
summary(mGood)

#Figure 3.7 on page 55
par(mfrow=c(1,2))
plot(x,YBad,ylim=c(-12,3))
abline(lsfit(x,YBad))
plot(x,YGood,ylim=c(-12,3))
abline(lsfit(x,YGood))

#Leverage values in Table 3.3 on page 57
lm.influence(mBad)$hat
lm.influence(mGood)$hat

#Regression output and Figure 3.8 on page 58
xq <- x^2
mBadq <- lm(YBad~x+I(x^2))
summary(mBadq)
xx <- c(-4:10)
yy <- summary(mBadq)$coef[1] + summary(mBadq)$coef[2]*xx + summary(mBadq)$coef[3]*xx^2
par(mfrow=c(1,1))
plot(xx,yy,ylim=c(-3,3),type="l",ylab="YBad",xlab="x")
points(x,YBad)

detach(huber)


rm(list = ls())
#Treasure's bond example
bonds <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/bonds.txt",header=TRUE)
attach(bonds)
bonds
#Figure 3.9 on page 63
par(mfrow=c(1,1))
plot(CouponRate,BidPrice,xlab="Coupon Rate (%)", ylab="Bid Price ($)",ylim=c(85,120),xlim=c(2,14))
abline(lsfit(CouponRate,BidPrice))

#Regression output on page 63 
m1 <- lm(BidPrice~CouponRate)
summary(m1)

#95% confidence intervals on page 63
round(confint(m1,level=0.95),3)

#Table 3.4 on page 62
leverage1 <- hatvalues(m1)
StanRes1 <- rstandard(m1)
residual1 <- m1$residuals
cbind(Case,CouponRate,BidPrice,round(leverage1,3),round(residual1,3),round(StanRes1,3))
which(abs(as.numeric(StanRes1))>2) 

#Figure 3.10 on page 64
plot(CouponRate,StanRes1,xlab="Coupon Rate (%)", ylab="Standardized Residuals",xlim=c(2,14))
abline(h=2,lty=2)
abline(h=-2,lty=2)
identify(CouponRate,StanRes1,Case)
# Click near a point to identify its Case. 
# This continues until you select "Stop" after clicking the right mouse button.

#Regression output on page 66
m2 <- update(m1, subset=(1:35)[-c(4,13,35)])
summary(m2)

#Figure 3.11 on page 65
plot(CouponRate[-c(4,13,35)],BidPrice[-c(4,13,35)],xlab="Coupon Rate (%)", ylab="Bid Price ($)",ylim=c(85,120),xlim=c(2,14),main="Regular Bonds")
abline(m2)

#Figure 3.12 on page 67
StanRes2 <- rstandard(m2)
plot(CouponRate[-c(4,13,35)],StanRes2,xlab="Coupon Rate (%)", ylab="Standardized Residuals",xlim=c(2,14),main="Regular Bonds")
abline(h=2,lty=2)
abline(h=-2,lty=2)

#Cook's distance
#Figure 3.13 on page 68
cd1 <- cooks.distance(m1)
plot(CouponRate,cd1,xlab="Coupon Rate (%)", ylab="Cook's Distance")
abline(h=4/(35-2),lty=2)
identify(CouponRate,cd1,Case)
# Click near a point to identify its Case. 
# This continues until you select "Stop" after clicking the right mouse button.

detach(bonds)

#production data example
production <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/production.txt",header=TRUE)
attach(production)
head(production)

m1 <- lm(RunTime~RunSize)
summary(m1)

#Figure 3.14 on page 70
par(mfrow=c(2,2))
plot(m1)

detach(production)


#contract cleaning data example
cleaning <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/cleaning.txt",header=TRUE)
head(cleaning)
dim(cleaning)
attach(cleaning)
#Figure 3.15 on page 71
par(mfrow=c(1,1))
plot(Crews,Rooms,xlab="Number of Crews",ylab="Number of Rooms Cleaned")
abline(lsfit(Crews,Rooms))
#Regression output on pages 72 and 73
m1 <- lm(Rooms~Crews)
summary(m1)
predict(m1,newdata=data.frame(Crews=c(4,16)),interval="prediction",level=0.95)

#Figure 3.16 on page 73
StanRes1 <- rstandard(m1)
plot(Crews,StanRes1,xlab="Number of Crews", ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1,xlab="Fitted values", ylab="Standardized Residuals")

#Figure 3.17 on page 74
sabs <- sqrt(abs(StanRes1))
plot(Crews,sabs,xlab="Number of Crews", ylab="Square Root(|Standardized Residuals|)")
abline(lsfit(Crews,sabs))

#Figure 3.18 on page 75
par(mfrow=c(2,2))
plot(m1)

#Figure 3.19 on page 75
sqrt(tapply(Rooms,Crews,var))
sds <- c(3.000000,4.966555,4.690416,6.642665,7.927123,7.28991,12.000463)
xx <- c(2,4,6,8,10,12,16)
par(mfrow=c(1,1))
plot(xx,sds,xlab="Number of Crews", ylab="Standard deviation(Rooms Cleaned)")
abline(lsfit(xx,sds))

## Data Transformation
#Poisson distribution
par(mfrow=c(1,1))
plot(1:60, dpois(1:60, lambda=30), type='h')

#Regression output on page 77
sqrtcrews <- sqrt(Crews)
sqrtrooms <- sqrt(Rooms)
m2 <- lm(sqrtrooms~sqrtcrews)
summary(m2)
predict(m2,newdata=data.frame(sqrtcrews=c(2,4)),interval="prediction",level=0.95)

#Figure 3.20 on page 78
par(mfrow=c(1,2))
plot(sqrt(Crews),sqrt(Rooms),xlab="Square Root(Number of Crews)",ylab="Square Root(Number of Rooms Cleaned)")
abline(lsfit(sqrt(Crews),sqrt(Rooms)))
StanRes2 <- rstandard(m2)
plot(sqrtcrews,StanRes2,xlab="Square Root(Number of Crews)", ylab="Standardized Residuals")

#Figure 3.21 on page 78
par(mfrow=c(2,2))
plot(m2)

#Compare the prediction interval
pred_m2 <- predict(m2,newdata=data.frame(sqrtcrews=c(2,4)),interval="prediction",level=0.95)
pred_m2[,2] <- pred_m2[,2]^2
pred_m2[,3] <- pred_m2[,3]^2
predict(m1,newdata=data.frame(Crews=c(4,16)),interval="prediction",level=0.95)
pred_m2
detach(cleaning)




#Supermarket sales data
confood1 <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/confood1.txt",header=TRUE)
attach(confood1)

#Figure 3.22 on page 80
par(mfrow=c(1,1))
plot(Price,Sales)
abline(lsfit(Price,Sales))

#Figure 3.23 on page 81
plot(log(Price),log(Sales),xlab="log(Price)",ylab="log(Sales)")
abline(lsfit(log(Price),log(Sales)))

#Regression output on page 82
m1 <- lm(log(Sales)~log(Price))
summary(m1)

#Figure 3.24 on page 82
StanRes1 <- rstandard(m1)
plot(log(Price),StanRes1,xlab="log(Price)", ylab="Standardized Residuals")

detach(confood1)



#Artificial data
responsetransformation <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/responsetransformation.txt",header=TRUE)
attach(responsetransformation)

#Figure 3.25 on page 84
plot(x,y)

#Figure 3.26 on page 85
plot(x,y)
m1 <- lm(y~x)
par(mfrow=c(1,2))
StanRes1 <- rstandard(m1)
absrtsr1 <- sqrt(abs(StanRes1))
plot(x,StanRes1,ylab="Standardized Residuals")
plot(x,absrtsr1,ylab="Square Root(|Standardized Residuals|)")

#Figure 3.27 on page 86
par(mfrow=c(2,3))
plot(density(y,bw="SJ",kern="gaussian"),type="l",
main="Gaussian kernel density estimate",xlab="y")
rug(y)
boxplot(y,ylab="Y")
qqnorm(y, ylab = "Y")
qqline(y, lty = 2, col=2)
sj <- bw.SJ(x,lower = 0.05, upper = 100)
plot(density(x,bw=sj,kern="gaussian"),type="l",
main="Gaussian kernel density estimate",xlab="x")
rug(x)
boxplot(x,ylab="x")
qqnorm(x, ylab = "x")
qqline(x, lty = 2, col=2)

#Figure 3.28 on page 87
#alr3 is too old to be installed
install.packages(remotes)
remotes::install_github("cran/alr3")
library(alr3)
par(mfrow=c(1,1))
inverseResponsePlot(m1,key=TRUE)
# Click on the section of the plot that you wish to put the figure legend

#Figure 3.29 on page 88
inverseResponsePlot(m1,lam=c(-1,-0.5, -0.33, -0.25, 0, 0.25, 0.33, 0.5,1))
lambda <- c(-1,-0.5, -0.33, -0.25, 0, 0.25, 0.33, 0.5,1)
RSS <- c(46673.9,24090.7,15264.2,11637.1,3583.8,440,266,880.2,7136.9)
plot(lambda,RSS,type="l",ylab=expression(RSS(lambda)),xlab=expression(lambda))

#Figure 3.30 on page 92
library(MASS)
par(mfrow=c(1,2))
boxcox(m1,lambda=seq(0.28,0.39,length=20))
boxcox(m1,lambda=seq(0.325,0.34,length=20))

#Regression output & Figure 3.31 on page 93
ty <- y^(1/3)
par(mfrow=c(2,2))
sj <- bw.SJ(ty,lower = 0.05, upper = 100)
plot(density(ty,bw=sj,kern="gaussian"),type="l",
main="Gaussian kernel density estimate",xlab=expression(Y^(1/3)))
rug(ty)
boxplot(ty,ylab=expression(Y^(1/3)))
qqnorm(ty, ylab = expression(Y^(1/3)))
qqline(ty, lty = 2, col=2)
m2 <- lm(ty~x)
plot(x,ty,ylab=expression(Y^(1/3)))
abline(m2)
summary(m2)

detach(responsetransformation)


#alr3 is too old to be installed
#remotes::install_github("cran/alr3")
library(alr3)
data(salarygov)
attach(salarygov)

#Figure 3.32 on page 96
m1 <- lm(MaxSalary~Score)
par(mfrow=c(2,2))
plot(Score,MaxSalary)
abline(m1,lty=2,col=2)
StanRes1 <- rstandard(m1)
absrtsr1 <- sqrt(abs(StanRes1))
plot(Score,StanRes1,ylab="Standardized Residuals")
plot(Score,absrtsr1,ylab="Square Root(|Standardized Residuals|)")
abline(lsfit(Score,absrtsr1),lty=2,col=2)

detach(salarygov)
#Output from R on page 96
summary(tranxy <- powerTransform(cbind(MaxSalary,Score)~1, salarygov))

#Figure 3.33 on page 97
par(mfrow=c(3,2))
plot(density(MaxSalary,bw="SJ",kern="gaussian"),type="l",
main="Gaussian kernel density estimate",xlab="MaxSalary")
rug(MaxSalary)
plot(density(Score,bw="SJ",kern="gaussian"),type="l",
     main="Gaussian kernel density estimate",xlab="Score")
rug(Score)

boxplot(MaxSalary,ylab="MaxSalary")
boxplot(Score,ylab="Score")

qqnorm(MaxSalary, ylab = "MaxSalary")
qqline(MaxSalary, lty = 2, col=2)
qqnorm(Score, ylab = "Score")
qqline(Score, lty = 2, col=2)

#Figure 3.34 on page 97
par(mfrow=c(1,1))
plot(sqrt(Score),log(MaxSalary),xlab=expression(sqrt(Score)))
abline(lsfit(sqrt(Score),log(MaxSalary)),lty=2,col=2)

#Figure 3.35 on page 98
par(mfrow=c(3,2))
plot(density(log(MaxSalary),bw="SJ",kern="gaussian"),type="l",
main="Gaussian kernel density estimate",xlab="log(MaxSalary)")
rug(log(MaxSalary))
boxplot(log(MaxSalary),ylab="log(MaxSalary)")
qqnorm(log(MaxSalary), ylab = "log(MaxSalary)")
qqline(log(MaxSalary), lty = 2, col=2)
sj <- bw.SJ(sqrt(Score),lower = 0.05, upper = 100)
plot(density(sqrt(Score),bw=sj,kern="gaussian"),type="l",
main="Gaussian kernel density estimate",xlab=expression(sqrt(Score)))
rug(sqrt(Score))
boxplot(sqrt(Score),ylab=expression(sqrt(Score)))
qqnorm(sqrt(Score), ylab=expression(sqrt(Score)))
qqline(sqrt(Score), lty = 2, col=2)

#Figure 3.36 on page 99
m2 <- lm(log(MaxSalary)~sqrt(Score))
par(mfrow=c(1,2))
StanRes2 <- rstandard(m2)
absrtsr2 <- sqrt(abs(StanRes2))
plot(sqrt(Score),StanRes2,ylab="Standardized Residuals",xlab=expression(sqrt(Score)))
plot(sqrt(Score),absrtsr2,ylab="Square Root(|Standardized Residuals|)",xlab=expression(sqrt(Score)))
abline(lsfit(sqrt(Score),absrtsr2),lty=2,col=2)

#R output on page 99
summary(tranx <- bctrans(~Score))

#Figure 3.37 on page 100
m3 <- lm(MaxSalary~sqrt(Score))
par(mfrow=c(1,1))
inverse.response.plot(m3,key=TRUE)
#Click on the plot where you want to put the legend

#Figure 3.38 on page 101
par(mfrow=c(2,2))
plot(density(MaxSalary^-0.25,bw="SJ",kern="gaussian"),type="l",
main="Gaussian kernel density estimate",xlab=expression(MaxSalary^-0.25))
rug(MaxSalary^-0.25)
boxplot(MaxSalary^-0.25,ylab=expression(MaxSalary^-0.25))
qqnorm(MaxSalary^-0.25,ylab=expression(MaxSalary^-0.25))
qqline(MaxSalary^-0.25,lty=2, col=2)

#Figure 3.39 on page 102
par(mfrow=c(2,2))
plot(sqrt(Score),MaxSalary^-0.25,xlab=expression(sqrt(Score)),ylab=expression(MaxSalary^-0.25))
abline(lsfit(sqrt(Score),MaxSalary^-0.25),lty=2,col=2)
m3 <- lm(MaxSalary^-0.25~sqrt(Score))
StanRes3 <- rstandard(m3)
absrtsr3 <- sqrt(abs(StanRes3))
plot(sqrt(Score),StanRes3,ylab="Standardized Residuals",xlab=expression(sqrt(Score)))
plot(sqrt(Score),absrtsr3,ylab="Square Root(|Standardized Residuals|)",xlab=expression(sqrt(Score)))
abline(lsfit(sqrt(Score),absrtsr3),lty=2,col=2)

detach(salarygov)
