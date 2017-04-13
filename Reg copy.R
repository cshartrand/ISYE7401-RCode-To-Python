#Simple Linear Regression
########################

setwd("C:/Courses/Advanced Statistical Modeling/7401 Spring16")

  ex=read.table("GPA.txt", h=T)
  attach(ex)

 #scatter plot

  plot(Score,GPA)

  #Regression

  a=lm(GPA~Score)
  summary(a)

  #Residuals and Fitted values

  names(a)
  a$res
  a$fit

  #Adding fitted line to the plot

  abline(a)
  segments(Score,a$fit,Score,GPA,col=2)

  #Multiple linear regression
  ###########################

  setwd("C:/Courses/Advanced Statistical Modeling/7401 Spring16")
  library(faraway)
  data(gala)
  a=lm(Species~Area+Elevation+Nearest+Scruz+Adjacent,gala)
  summary(a)
  a=lm(Species~Area,gala)
  summary(a)

  a=lm(Species~Area+Elevation+Nearest+Scruz+Adjacent,gala)
  a1=lm(Species~Elevation+Adjacent,gala)
  anova(a1,a)
  anova(a1)
  anova(lm(Species~Elevation+Adjacent,gala))
  anova(lm(Species~Adjacent+Elevation,gala))
  library(relaimpo)
  calc.relimp(a1)

  #Confidence regions

  library(ellipse)
  plot(ellipse(a1,c(2,3),level=.95),type="l")
  lines(ellipse(a1,c(2,3),level=.9))

  #prediction

  library(faraway)
  data(fat,package="faraway")
  lmod <- lm(brozek ~ age + weight + height + neck + chest + abdom + hip + thigh + knee + ankle + biceps + forearm + wrist, data=fat)
  x <- model.matrix(lmod)
  (x0 <- apply(x,2,median))
  (y0 <- sum(x0*coef(lmod)))
  predict(lmod,new=data.frame(t(x0)))
  predict(lmod,new=data.frame(t(x0)),interval="prediction")
  predict(lmod,new=data.frame(t(x0)),interval="confidence")


  #Explanation
  setwd("C:/Courses/Advanced Statistical Modeling/7401 Spring16")
  studios=read.table("studios.txt",h=T)
  a=lm(y~x1+x2,studios)
  summary(a)

  data(newhamp, package="faraway")
  colSums(newhamp[newhamp$votesys == 'D',2:3])
  colSums(newhamp[newhamp$votesys == 'H',2:3])
  newhamp$trt <- ifelse(newhamp$votesys == 'H',1,0)
  lmodu <- lm(pObama ~ trt, newhamp)
  sumary(lmodu)
  lmodz <- lm(pObama ~ trt +  Dean , newhamp)
  sumary(lmodz)
  sumary(lm(Dean ~ trt, newhamp))

  #Diagnostics
  #############

  #Normality

  a=lm(Species~Area+Elevation+Nearest+Scruz+Adjacent,data=gala)
  summary(a)
  X=model.matrix(a)
  H=X%*%solve(t(X)%*%X)%*%t(X)
  std.res=a$res/(summary(a)$sig*sqrt(1-diag(H)))
  qqnorm(std.res)
  qqline(std.res)

  par(mfrow=c(3,3))
  for(i in 1:9) qqnorm(rnorm(50))
  for(i in 1:9) qqnorm(exp(rnorm(50)))
  for(i in 1:9) qqnorm(rcauchy(50))
  for(i in 1:9) qqnorm(runif(50))
  par(mfrow=c(1,1))

  #Bootstrap

  #First let's do some simulations


  attach(gala)

  a=lm(Species~Area+Elevation+Nearest+Scruz+Adjacent)
  summary(a)


  beta=a$coef
  sigma=summary(a)$sig
  X=model.matrix(a)
  Xinv=solve(t(X)%*%X)
  beta.sim=matrix(0,nrow=10000, ncol=6)
  for(i in 1:10000)
  {
  y=X%*%beta+rnorm(30, mean=0, sd=sigma)
  beta.sim[i,]=Xinv%*%t(X)%*%y
  }
  plot(density(beta.sim[,2]))
  std.error=sigma*sqrt(diag(Xinv))
  curve(dnorm(x,beta[2],std.error[2]),col=2,add=T)

  for(i in 1:10000)
  {
  y=X%*%beta+sigma*rt(30,4)
  beta.sim[i,]=Xinv%*%t(X)%*%y
  }
  plot(density(beta.sim[,2]))
  std.error=sigma*sqrt(diag(Xinv))
  curve(dnorm(x,beta[2],std.error[2]),col=2,add=T)

  #Now let's do the bootstrap

  beta.boot=matrix(0,nrow=10000, ncol=6)
  for(i in 1:10000)
  {
    error=sample(a$res,rep=T)
    y=X%*%beta+error
    beta.boot[i,]=Xinv%*%t(X)%*%y
  }
  std.error=sigma*sqrt(diag(Xinv))
  par(mfrow=c(3,2))
  plot(density(beta.boot[,1]))
  curve(dnorm(x,beta[1],std.error[1]),col=2,add=T)
  plot(density(beta.boot[,2]))
  curve(dnorm(x,beta[2],std.error[2]),col=2,add=T)
  plot(density(beta.boot[,3]))
  curve(dnorm(x,beta[3],std.error[3]),col=2,add=T)
  plot(density(beta.boot[,4]))
  curve(dnorm(x,beta[4],std.error[4]),col=2,add=T)
  plot(density(beta.boot[,5]))
  curve(dnorm(x,beta[5],std.error[5]),col=2,add=T)
  plot(density(beta.boot[,6]))
  curve(dnorm(x,beta[6],std.error[6]),col=2,add=T)
  par(mfrow=c(1,1))

  confint(a)
  con.boot=matrix(0,6,2)
  for(i in 1:6)
    con.boot[i,]=quantile(beta.boot[,i], prob=c(.025,.975))
  con.boot


#Unequal Variances


#Transformations

  plot(a$fit,a$res)
  a.tr=lm(log(Species)~Area+Elevation+Nearest+Scruz+Adjacent)
  summary(a.tr)
  plot(a.tr$fit,a.tr$res)

  a.tr=lm(sqrt(Species)~Area+Elevation+Nearest+Scruz+Adjacent)
  summary(a.tr)
  plot(a.tr$fit,a.tr$res)

  1-sum((Species-a.tr$fit^2)^2)/sum((Species-mean(Species))^2)

  #Weighted least squares

  data(fpe)
  attach(fpe)
  N=(A2+B2)-(A+B+C+D+E+F+G+H+J+K)
  a=lm(A2~A+B+C+D+E+F+G+H+J+K+N-1)
  summary(a)

  plot(EI,A2)

  a=lm(A2~A+B+C+D+E+F+G+H+J+K+N-1,weights=1/EI)
  summary(a)
  a=lm(A2-A-G-K~C+D+E+F+N-1,weights=1/EI)
  summary(a)

  a=lm(A2~A+B+C+D+E+F+G+H+J+K+N-1,weights=1/EI)
  X=model.matrix(a)
  library(quadprog)
  W=diag(1/EI)
  Dmat=t(X)%*%W%*%X
  dvec=t(X)%*%W%*%A2
  Amat=cbind(diag(11),-diag(11))
  b0=c(rep(0,11),rep(-1,11))
  solve.QP(Dmat,dvec,Amat,b0)



#Correlated Errors

 #Generalized least squares
  library(faraway)
  data(globwarm,package="faraway")
  lmod <- lm(nhtemp ~ wusa + jasper + westgreen + chesapeake + tornetrask + urals + mongolia + tasman, globwarm)
  sumary(lmod)
  cor(residuals(lmod)[-1],residuals(lmod)[-length(residuals(lmod))])
  require(nlme)
  glmod <- gls(nhtemp ~ wusa + jasper + westgreen + chesapeake + tornetrask + urals + mongolia + tasman,  correlation=corAR1(form=~year),  data=na.omit(globwarm))
  summary(glmod)
  intervals(glmod,which="var-cov")

#Outliers

  x=runif(30,-1,1)
  x[31]=10
  y=x+rnorm(31)
  y[31]=0
  plot(x,y, ylim=c(-2,8))
  a=lm(y~x)
  abline(a)
  X=model.matrix(a)
  ha=hat(X)
  plot(ha)
  abline(h=2*2/31,col=2)

  a$res/(summary(a)$sig*sqrt(1-ha))
  r=rstandard(a)
  halfnorm(r)

  plot(x,y)
  abline(a)
  a1=lm(y[-31]~x[-31])
  abline(a1,col=2)
  r*sqrt(28/(29-r^2))
  estar=rstudent(a)
  halfnorm(estar)


  #multiple outliers

  library(faraway)
  data(star)
  plot(light ~ temp, star)
  gs1 <- lm(light ~ temp, star)
  abline(coef(gs1))
  ga <- lm(light ~ temp, star, subset=(temp>3.6))
  abline(coef(ga),col=2)

  #influential observations

  a=lm(y~x)
  cook <- cooks.distance(a)
  plot(a)

  #Robust regression

  data(gala, package="faraway")
  lsmod <- lm(Species ~ Area + Elevation + Nearest +   Scruz + Adjacent,gala)
  sumary(lsmod)
  require(MASS)
  rlmod <- rlm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,gala)
  summary(rlmod)
  wts <- rlmod$w
  names(wts) <- row.names(gala)
  head(sort(wts),10)
  require(quantreg)
  l1mod <- rq(Species ~Area+Elevation+Nearest+Scruz+Adjacent, data=gala)
  summary(l1mod)
  set.seed(123)
  ltsmod <- ltsreg(Species ~ Area + Elevation + Nearest +  Scruz + Adjacent,gala)
  coef(ltsmod)
  ltsmod <- ltsreg(Species ~ Area + Elevation + Nearest + Scruz  + Adjacent, gala,nsamp="exact")
  coef(ltsmod)

  data(star, package="faraway")
  plot(light ~ temp, star)
  gs1 <- lm(light ~ temp, star)
  abline(coef(gs1))
  gs2 <- rlm(light ~ temp, star)
  abline(coef(gs2), lty=2,col=2)
  gs3 <- rq(light ~ temp, data=star)
  abline(coef(gs3), lty=3,col=3)
  gs4 <- ltsreg(light ~ temp, star, nsamp="exact")
  abline(coef(gs4), lty=4,col=4)

#structure of the model

fresh=read.table("fresh.txt",h=T)
a=lm(Demand~PriceDif+AdvExp,fresh)
summary(a)

par(mfrow=c(1,2))
plot(fresh$PriceDif,a$res)
plot(fresh$AdvExp,a$res)

termplot(a, partial.resid=TRUE, terms=1)
termplot(a, partial.resid=TRUE, terms=2)
par(mfrow=c(1,1))

a=lm(Demand~PriceDif+AdvExp+I(AdvExp^2),fresh)
summary(a)

a=lm(Demand~PriceDif*AdvExp+I(AdvExp^2),fresh)
summary(a)

#Transformations
require(MASS)
data(gala, package="faraway")
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz +  Adjacent,gala)
boxcox(lmod, lambda=seq(-0.25,0.75,by=0.05),plotit=T)
data(leafburn)
lmod <- lm(burntime ~ nitrogen+chlorine+potassium, leafburn)
boxcox(lmod,plotit=T)
logtrans(lmod,plotit=TRUE, alpha=seq(-min(leafburn$burntime)+0.001,0,by=0.01))

#Collinearity
library(faraway)
data(seatpos, package="faraway")
lmod <- lm(hipcenter ~ ., seatpos)
sumary(lmod)
round(cor(seatpos[,-9]),2)
library(car)
scatterplotMatrix(seatpos)
x <- model.matrix(lmod)[,-1]
e <- eigen(t(x) %*% x)
e$val
sqrt(e$val[1]/e$val)
summary(lm(x[,1] ~ x[,-1]))$r.squared
1/(1-0.49948)
vif(x)
v=numeric(8)
for(i in 1:8)
v[i]=1/(1-summary(lm(x[,i] ~ x[,-i]))$r.squared)
v

lmod <- lm(hipcenter+10*rnorm(38) ~ ., seatpos)
sumary(lmod)
round(cor(x[,3:8]),2)
lmod2 <- lm(hipcenter ~ Age + Weight + Ht, seatpos)
sumary(lmod2)

#Ridge regression

require(MASS)

a.ridge=lm.ridge(hipcenter~.,data=seatpos, lambda = seq(0, 1, len=100))
plot(a.ridge)
which.min(a.ridge$GCV)
select(a.ridge)

#Variable selection
######################

library(faraway)
data(state)
statedata <- data.frame(state.x77,row.names=state.abb)
lmod <- lm(Life.Exp ~ ., statedata)
sumary(lmod)

#All subsets regression
require(leaps)
b <- regsubsets(Life.Exp~.,data=statedata)
rs <- summary(b)
rs$which
AIC <- 50*log(rs$rss/50) + (2:8)*2
plot(AIC ~ I(1:7), ylab="AIC", xlab="Number of Predictors")
plot(2:8,rs$adjr2,xlab="No. of Parameters",ylab="Adjusted R-square")
which.max(rs$adjr2)
plot(2:8,rs$cp,xlab="No. of Parameters",ylab="Cp Statistic")
abline(0,1)

#stepwise regression
a=lm(Life.Exp ~ ., statedata)
a.step=step(a)
summary(a.step)

#Nonnegative Garrote
library(faraway)
data(state)
statedata=data.frame(state.x77,row.names=state.abb)
state.sca=as.data.frame(scale(as.matrix(statedata)))
a=lm(Life.Exp~.-1,data=state.sca)
summary(a)

B=diag(a$coef)
Z=model.matrix(a)%*%B
D=t(Z)%*%Z
y=state.sca$Life.Exp
d=t(Z)%*%y
library(quadprog)
A=cbind(-1,diag(7))
M=seq(0.01,7,length=100)
gcv=numeric(100)
for(i in 1:100)
{
  b0=c(-M[i],rep(0,7))
  coef.nng=solve.QP(D,d,A,b0)$sol
  e=y-Z%*%coef.nng
  gcv[i]=sum(e^2)/(50*(1-M[i]/50)^2)
}
plot(M,gcv,type="l")
M=M[which.min(gcv)]
b0=c(-M,rep(0,7))
coef.nng=round(solve.QP(D,d,A,b0)$sol,10)
beta.nng=B%*%coef.nng
round(cbind(a$coef,beta.nng),7)
e=y-Z%*%coef.nng
1-sum(e^2)/sum(y^2)

#Models with interactions and polynomial terms

a=lm(Life.Exp~.^2-1,data=state.sca)
summary(a)
B=diag(a$coef)
Z=model.matrix(a)%*%B
D=t(Z)%*%Z
d=t(Z)%*%y
A=cbind(-1,diag(28))
con=rbind(cbind(1,diag(6),-diag(6),matrix(0,6,15)),
          cbind(0,1,diag(5),matrix(0,5,6),-diag(5),matrix(0,5,10)),
          cbind(0,0,1,diag(4),matrix(0,4,11),-diag(4),matrix(0,4,6)),
          cbind(0,0,0,1,diag(3),matrix(0,3,15),-diag(3),matrix(0,3,3)),
          cbind(0,0,0,0,1,diag(2),matrix(0,2,18),-diag(2),matrix(0,2,1)),
          cbind(0,0,0,0,0,1,1,matrix(0,1,20),-1))
A=cbind(A,t(con))
M=seq(0.01,28,length=100)
gcv=numeric(100)
for(i in 1:100)
{
  b0=c(-M[i],rep(0,49))
  coef.nng=solve.QP(D,d,A,b0)$sol
  e=y-Z%*%coef.nng
  gcv[i]=sum(e^2)/(50*(1-M[i]/50)^2)
}
plot(M,gcv,type="l")
M=M[which.min(gcv)]
b0=c(-M,rep(0,49))
coef.nng=round(solve.QP(D,d,A,b0)$sol,10)
coef.nng
beta.nng=B%*%coef.nng
round(cbind(a$coef,beta.nng),7)
e=y-Z%*%coef.nng
1-sum(e^2)/sum(y^2)

#R code from Josh McDonald for generating A matrices
###Only input needed is p###
###weak heredity
gweak = function(p)
{
  mat = mat.or.vec(p,p*(p-1)/2)
  c = 1
  m  = p
  while(m > 1)
  {
    for (j in 1:(m-1))
    {
      mat[,c] = c(numeric(p - m),1,numeric(j-1),1,numeric(m-(j+1)))
      c = c+1
    }
    m = m-1
  }
  cbind(-1,diag(p+p*(p-1)/2),rbind(mat,-cbind(diag(p*(p-1)/2))))
}
###strong heredity
gstrong = function(p)
{
  mat = mat.or.vec(p,p*(p-1))
  c = 1
  m = p
  while(m > 1)
  {
    for (j in 1:(m-1))
    {
      mat[,c]= c(numeric(p - m),1,numeric(m-1))
      mat[,(c+p*(p-1)/2)] = c(numeric(p - m+j),1,numeric(m-(j+1)))
      c = c+1
    }
    m = m-1
  }
  cbind(-1,diag(p+p*(p-1)/2),rbind(mat,-cbind(diag(p*(p-1)/2),diag(p*(p-1)/2))))
}

#Lasso
######

require(lars)
data(state)
statedata <- data.frame(state.x77,row.names=state.abb)
lmod <- lars(as.matrix(statedata[,-4]),statedata$Life)
plot(lmod)
set.seed(123)
cvlmod <- cv.lars(as.matrix(statedata[,-4]),statedata$Life)
cvlmod$index[which.min(cvlmod$cv)]
predict(lmod,s=0.65657,type="coef",mode="fraction")$coef
coef(lm(Life.Exp ~ Population+Murder+HS.Grad+Frost, statedata))


#Nolinear Regression

#Michaelis-Menten model (Treloar 1974)
  x=c(.02,.06,.11,.22,.56,1.1)
x=rep(x,rep(2,6))
y=c(76,47,97,107,123,139,159,152,191,201,207,200)
plot(x,y)
a.tr=lm(1/y~I(1/x))
summary(a.tr)
plot(1/x,1/y)
abline(a.tr)
plot(x,y)
theta10=1/a.tr$coef[1]
theta20=a.tr$coef[2]*theta10
theta0=c(theta10,theta20)
f=function(x, theta)
  theta[1]*x/(theta[2]+x)

curve(f(x, theta0),from=0,to=1.2,add=T)

#Gauss-Newton Method

df1=function(x,theta)
  x/(theta[2]+x)
df2=function(x,theta)
  -theta[1]*x/(theta[2]+x)^2

f0=f(x,theta0)
V0=cbind(df1(x,theta0), df2(x,theta0))
theta0=theta0+lm(y-f0~V0-1)$coef
curve(f(x, theta0),from=0,to=1.2,add=T,col=3)
theta0

thetahat=theta0
fhat=f(x,thetahat)
V=cbind(df1(x,thetahat), df2(x,thetahat))
sigma2=sum((y-fhat)^2)/(12-2)
sigma2*solve(t(V)%*%V)
summary(lm(y-fhat~V-1))

#Direct minimization

sse=function(theta)
  sum((y-f(x,theta))^2)

theta0=c(theta10,theta20)
thetahat=optim(theta0,sse)$par

library(numDeriv)
hessian(sse,thetahat)
sigma2=sse(thetahat)/(12-2)
2*sigma2*solve(hessian(sse,thetahat))


#SO2 Example

so2=read.table("so2.txt",h=T)
attach(so2)

f=function(theta)
{
  beta0=theta[1]
  beta1=theta[2]
  m=theta[3]
  val=y0+((beta0+beta1*x1)/(beta0+beta1*x1+60*x2)*1000-y0)*(1-exp(-(beta0+beta1*x1+60*x2)*dt/(60*m)))
  return(val)
}

sse=function(theta)
  sum((y1-f(theta))^2)

optim(c(0,1,1000),sse)

a=nls(y1~y0+((beta0+beta1*x1)/(beta0+beta1*x1+60*x2)*1000-y0)*(1-exp(-(beta0+beta1*x1+60*x2)*dt/(60*m))),start=list(beta0=0,beta1=1,m=1000))
summary(a)
a=nls(y1~y0+((beta0+beta1*x1)/(beta0+beta1*x1+60*x2)*1000-y0)*(1-exp(-(beta0+beta1*x1+60*x2)*dt/(60*m))),start=list(beta0=0,beta1=1,m=1000), algorithm="port", lower=c(0,0,0))
summary(a)

detach(so2)
rm(list=ls())
#LAMM Example

lamm=read.table("lamm.txt",h=T)
attach(lamm)

f=function(theta)
{
  gamma=theta[1]
  b1=theta[2]
  b2=theta[3]
  b3=theta[4]
  b4=theta[5]
  val=x1^gamma*b1*exp(b2*x2-b3*x3*exp(-b4*x4))
  return(val)
}
sse=function(theta)
  sum((y-f(theta))^2)

optim(c(1,1,0,0,0),sse)

a=nls(y~x1^gamma*b1*exp(b2*x2-b3*x3*exp(-b4*x4)),start=c(gamma=1,b1=1,b2=0,b3=.01,b4=0))
summary(a)

confint(a)

library(nlstools)
a=nls(y~x1^gamma*b1*exp(b2*x2-b3*x3*exp(-b4*x4)),start=c(gamma=1,b1=1,b2=0,b3=.01,b4=0),data=lamm)
a.boot=nlsBoot(a)
summary(a.boot)
par(mfrow=c(2,3))
plot(density(a.boot$coef[,1]))
plot(density(a.boot$coef[,2]))
plot(density(a.boot$coef[,3]))
plot(density(a.boot$coef[,4]))
plot(density(a.boot$coef[,5]))
par(mfrow=c(1,1))

plot(fitted(a),residuals(a))
par(mfrow=c(2,2))
plot(x1,residuals(a))
plot(x2,residuals(a))
plot(x3,residuals(a))
plot(x4,residuals(a))
par(mfrow=c(1,1))

par(mfrow=c(2,3))
interaction.plot(x1,x2,residuals(a))
interaction.plot(x1,x3,residuals(a))
interaction.plot(x1,x4,residuals(a))
interaction.plot(x2,x3,residuals(a))
interaction.plot(x2,x4,residuals(a))
interaction.plot(x3,x4,residuals(a))
par(mfrow=c(1,1))

a=nls(y~x1^gamma*b1*exp(b2*x2-(b3*x3+b5*x2*x3)*exp(-b4*x4)),start=c(gamma=1,b1=1,b2=0,b3=.01,b4=0,b5=0))
summary(a)

#Conditionally Linear Parameters

a=nls(y~x1^gamma*exp(b2*x2-b3*x3*exp(-b4*x4)),start=c(gamma=1,b2=0,b3=.01,b4=0),algorithm="plinear")
summary(a)


a=nls(log(y)~gamma*log(x1)+log(b1)+b2*x2-b3*x3*exp(-b4*x4),start=c(gamma=1,b1=1,b2=0,b3=.01,b4=0))
summary(a)
X=model.matrix(lm(y~I(log(x1))+x2))
a=nls(log(y)~cbind(X,-x3*exp(-b4*x4)),start=c(b4=0),algorithm="plinear")

summary(a)
