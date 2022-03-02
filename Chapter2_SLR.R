#chapter 2
#Simple linear regression
rm(list = ls())
dta_production <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/production.txt",header = TRUE)
head(dta_production)
attach(dta_production) #dont forget to "detach()" after using

#Figure 2.1 on page 16
#obtain the scatter plot
par(mfrow=c(1,1))
plot(dta_production$RunSize,dta_production$RunTime,xlab="Run Size", ylab="Run Time")

#R output on page 19
result_m1 <- lm(RunTime~RunSize)
summary(result_m1)

#Figure 2.3 on page 20
#scatter plot with fitted line
plot(RunSize,RunTime,xlab="Run Size", ylab="Run Time")
abline(lsfit(RunSize,RunTime))

#t-value on page 23
#t-critical value for t(0.5/2,20-2)
tval <- qt(1-0.05/2,18)
tval

#95% confidence intervals on page 24
#95% CI for coefficients
round(confint(result_m1,level=0.95),3)

#R output on page 27
predict(result_m1,newdata=data.frame(RunSize=c(50,100,150,200,250,300,350)),interval="confidence",level=0.95)
predict(result_m1,newdata=data.frame(RunSize=c(50,100,150,200,250,300,350)),interval="prediction",level=0.95)

#R output on page 30
anova(result_m1)
detach(dta_production)

### Simple linear regression with dummy variables
dta_changeover_times <- read.table("https://gattonweb.uky.edu/sheather/book/docs/datasets/changeover_times.txt",header=TRUE)
head(dta_changeover_times)


#R output on page 31
result_m1 <- lm(Changeover~New,data=dta_changeover_times)
summary(result_m1)

#Figure 2.5 on page 32
par(mfrow=c(2,2))
plot(dta_changeover_times$New,dta_changeover_times$Changeover,xlab="Dummy variable, New",ylab="Change Over Time")
abline(lsfit(dta_changeover_times$New,dta_changeover_times$Changeover))
boxplot(dta_changeover_times$Changeover~dta_changeover_times$New,xlab="Dummy variable, New",ylab="Change Over Time")
boxplot(dta_changeover_times$Changeover~dta_changeover_times$Method,ylab="Change Over Time",xlab="Method")

#t-value on page 33
tval <- qt(1-0.05/2,118)
tval



