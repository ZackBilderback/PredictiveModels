
##################################################
# Plot balance vs. default for the Default data.
##################################################

#--------------------------------------------------
#get Default data
library(ISLR)
data(Default)
attach(Default)
#--------------------------------------------------
#plot default (binary y) vs balance (numeric x)

plot(balance~default,data=Default,col=c('lightblue','orange'),cex.axis=1.5,cex.lab=1.5)


##################################################
#plot logistic function
##################################################

z = seq(from=-5,to=5,length.out=1000)
Fz = exp(z)/(1+exp(z))
plot(z,Fz,type='l',col='blue',lwd=2.5,xlab=expression(eta),ylab='F',cex.lab=1.3)

##################################################
# Do the logit fit for default ~ balance, show the two steps.
##################################################
#--------------------------------------------------
#get Default data
data(Default)
attach(Default)
#--------------------------------------------------
#fit logit for default~balance

F = function(x) {exp(x)/(1+exp(x))}

glm.fit = glm(default~balance,data=Default,family=binomial)
eta = predict(glm.fit) #get eta
pyx = predict(glm.fit,type='response') #get phat = F(eta)

par(mfrow=c(3,1))
n=nrow(Default)
ii = sample(1:n,50)

plot(balance,eta)
z = seq(from=-11,to=8,length.out=1000)
points(balance[ii],eta[ii],pch=16,col='red')
title(main='eta = -10.65 + .0055 balance',cex.main=1.5)

plot(z,F(z),col='blue',type='l',lwd=3,xlab='eta',ylab='F(eta)')
points(eta[ii],F(eta)[ii],pch=16,col='red')
title(main='eta vs. F(eta)',cex.main=1.5)

oo = order(balance)
plot(balance[oo],pyx[oo],type='l',col='red',xlab='x=balance',ylab='P(Y=Yes | x=balance)',
           lwd=2)
title(main='x=balance vs. P(Y=Yes | balance)',cex.main=1.5)


par(mfrow=c(1,1))
plot(balance[oo],pyx[oo],type='l',col='red',xlab='x=balance',ylab='P(Y=Yes | x=balance)',
           lwd=2,cex.lab=1.3)
title(main='x=balance vs. P(Y=Yes | balance)',cex.main=1.3)


print(summary(glm.fit))

##################################################
# Fit logit with various choices for x from Default data frame
##################################################
#fit logit for default using various number of x
glm.fit = glm(default~balance+student+income,data=Default,family=binomial)
phat = predict(glm.fit,type='response')
glm.fit.1 = glm(default~balance,data=Default,family=binomial)
phat.1 = predict(glm.fit.1,type='response')
glm.fit.2 = glm(default~student,data=Default,family=binomial)
phat.2 = predict(glm.fit.2,type='response')
glm.fit.3 = glm(default~balance+student,data=Default,family=binomial)
phat.3 = predict(glm.fit.3,type='response')

#--------------------
#plot balance vs. phat for students and non-students using default~balance+student fit.
plot(range(balance),range(c(phat.3,phat.1,phat.2)),xlab='balance',ylab='P(Y=1|x)',type='n',cex.lab=1.3)
ii=student=="Yes"
points(balance[ii],phat.3[ii],col='orange')
ii=student=="No"
points(balance[ii],phat.3[ii],col='lightblue')
legend('topleft',legend=c('student','not-student'),col=c('orange','lightblue'),pch=1)

#--------------------
#write out regression output for default~balance+student+income

print(summary(glm.fit))


#--------------------
#plot phats with and without income
plot(phat,phat.3,xlab='phat all Xs',ylab='phat without income',cex.lab=1.3)
abline(0,1,col='red')

#--------------------
#write out regression output for default~balance+student+income

print(summary(glm.fit.3))


#--------------------
#write out regression output for default~student

print(summary(glm.fit.2))


#--------------------
#plot student vs. balance
plot(balance~student,data=Default,col=c('lightblue','orange'),cex.lab=1.3)


###################################################
# Discretize balance and then make table relating balance to default.
###################################################
#--------------------------------------------------
#--------------------------------------------------
bks = c(-Inf,c(1473,1857),Inf)
bal = cut(balance,breaks=bks,labels=c('1','2','3'))

def=default; levels(def) = c('0','1') #1 means you default, 0 means you don't.
tbc = table(bal,def) #table of counts
tbl = round(tbc/nrow(Default),3) #table of percentages

print(tbl)
print(tbc)

tbcond = tbl
for(i in 1:nrow(tbcond)) {
   tbcond[i,] = tbcond[i,]/sum(tbcond[i,])
}
tbcond = round(tbcond,3)
print(tbcond)

def=default; levels(def) = c('1','2') #2 means you default, 1 means you don't.
tbc = table(bal,def) #table of counts
tbl = round(tbc/nrow(Default),3) #table of percentages
tbpxy = tbl
for(i in 1:ncol(tbpxy)) {
   tbpxy[,i]=tbpxy[,i]/sum(tbpxy[,i])
}
tbpxy = round(tbpxy,3)
print(tbpxy)


##################################################
#Do naive bayes, make tables needed for balance vs. default and student vs. default.
##################################################
#--------------------------------------------------
bks = c(-Inf,c(1473,1857),Inf)
bal = cut(balance,breaks=bks,labels=c('1','2','3'))

def=default; levels(def) = c('1','2') #2 means you default, 1 means you don't.
tbc = table(bal,def) #table of counts

tbl = round(tbc/nrow(Default),3) #table of percentages, def and bal
tbpx1y = tbl
for(i in 1:ncol(tbpx1y)) {
   tbpx1y[,i]=tbpx1y[,i]/sum(tbpx1y[,i])
}
tbpx1y = round(tbpx1y,3)

tbl = round(table(student,def)/nrow(Default),3) #table of percentages, def and stu
tbpx2y = tbl
for(i in 1:ncol(tbpx2y)) {
   tbpx2y[,i]=tbpx2y[,i]/sum(tbpx2y[,i])
}
tbpx2y = round(tbpx2y,3)

#write tables
print(tbl)

print(tbpx2y)

##################################################
# Plot deviance loss function.
##################################################
pv = seq(from=.0001,to=.999,length.out=1000)
plot(pv,-2*log(pv),xlab=expression(p[y]),ylab='dev-loss',type='l',col='blue',lwd=1.5,cex.lab=1.5)

##################################################
#Get out of sample deviance loss for various logit fits.
# Do repeated train/test splits.
##################################################
source('Sec3Functions.txt')
#--------------------------------------------------
#--------------------------------------------------
#train/test
set.seed(99)
n=nrow(Default)
ntrain=floor(.75*n)
iitrain = sample(1:n,ntrain)
Deftrain = Default[iitrain,]
Deftest = Default[-iitrain,]
#--------------------------------------------------
#train/test
#s:student, b:balance, i:income
glm.i = glm(default~income,data=Deftrain,family=binomial)
phat.i = predict(glm.i,newdata=Deftest,type='response')
l.i = loss(Deftest$default,phat.i)

glm.b = glm(default~balance,data=Deftrain,family=binomial)
phat.b = predict(glm.b,newdata=Deftest,type='response')
l.b  = loss(Deftest$default,phat.b)

glm.bs = glm(default~balance+student,data=Deftrain,family=binomial)
phat.bs = predict(glm.bs,newdata=Deftest,type='response')
l.bs  = loss(Deftest$default,phat.bs)

glm.bsi = glm(default~balance+student+income,data=Deftrain,family=binomial)
phat.bsi = predict(glm.bsi,newdata=Deftest,type='response')
l.bsi  = loss(Deftest$default,phat.bsi)

par(mfrow=c(2,2))
plot(phat.i~Deftest$default,col=c('lightblue','orange'),cex.axis=2,cex.lab=2)
title(main=paste('oos deviance loss: ',round(l.i,3),sep=''))
plot(phat.b~Deftest$default,col=c('lightblue','orange'),cex.axis=2,cex.lab=2)
title(main=paste('oos deviance loss: ',round(l.b,3),sep=''))
plot(phat.bs~Deftest$default,col=c('lightblue','orange'),cex.axis=2,cex.lab=2)
title(main=paste('oos deviance loss: ',round(l.bs,3),sep=''))
plot(phat.bsi~Deftest$default,col=c('lightblue','orange'),cex.axis=2,cex.lab=2)
title(main=paste('oos deviance loss: ',round(l.bsi,3),sep=''))
#--------------------------------------------------
#loop over train/test splits
nd=100
dl.i = rep(nd,0)
dl.b = rep(nd,0)
dl.bs = rep(nd,0)
dl.bsi = rep(nd,0)

n=nrow(Default)
ntrain=floor(.75*n)

set.seed(99)
for(i in 1:nd) {
   #draw train and test
   iitrain = sample(1:n,ntrain)
   Deftrain = Default[iitrain,]
   Deftest = Default[-iitrain,]

   #income
   glm.i = glm(default~income,data=Deftrain,family=binomial)
   phat.i = predict(glm.i,newdata=Deftest,type='response')
   dl.i[i] = loss(Deftest$default,phat.i)

   #balance
   glm.b = glm(default~balance,data=Deftrain,family=binomial)
   phat.b = predict(glm.b,newdata=Deftest,type='response')
   dl.b[i] = loss(Deftest$default,phat.b)

   #balance, student
   glm.bs = glm(default~balance+student,data=Deftrain,family=binomial)
   phat.bs = predict(glm.bs,newdata=Deftest,type='response')
   dl.bs[i] = loss(Deftest$default,phat.bs)

   #balance, student, income
   glm.bsi = glm(default~balance+student+income,data=Deftrain,family=binomial)
   phat.bsi = predict(glm.bsi,newdata=Deftest,type='response')
   dl.bsi[i] = loss(Deftest$default,phat.bsi)
}
par(mfrow=c(1,1))
boxplot(dl.i,dl.b,dl.bs,dl.bsi,names=c('inc','bal','bal+stu','bal+stu+inc'),col=2:5)
legend("topright",legend=c('income','balance','balance+student','balance+student+income'),
col=2:5,lwd=4)
title(main='out of sample deviance loss, repeated train/test draws')

##################################################
# Compute lift and ROC 
##################################################
source('Sec3Functions.txt')
#--------------------------------------------------
#fit simple logit 
glm.b = glm(default~balance,data=Default,family=binomial) 
phat.b = predict(glm.b,type='response') 
#--------------------------------------------------
#do for one s
s=.5
yhat = ifelse(phat.b<s,0,1)
y = as.numeric(default)-1 #all
tbl = table(yhat,y)

print(tbl)

#--------------------------------------------------
#compute lift and roc
ns=1000
sv = seq(from=.0,to=.99,length.out=ns)
FP=rep(0,ns)
TP=rep(0,ns)
N=rep(0,ns)
n0=sum(y==0)
for(i in 1:ns) {
   N[i] = sum(phat.b>sv[i])/length(y)
   TP[i] = sum((phat.b>sv[i]) & (y==1))/sum(y==1)
   FP[i] = sum((phat.b>sv[i]) & (y==0))/sum(y==0)
}

par(mfrow=c(1,3))
par(mai=c(0.9,0.9,.4,.4))
plot(sv,N,xlab='s',type='l',col='blue',cex.lab=2.0)
title(main='s vs. N',cex.main=2)
plot(sv,TP,xlab='s',type='l',col='blue',cex.lab=2.0)
title(main='s vs. TP',cex.main=2)
plot(sv,FP,xlab='s',type='l',col='blue',cex.lab=2.0)
title(main='s vs. FP',cex.main=2)

par(mai=c(0.9,0.9,.4,.4))
par(mfrow=c(1,2))
plot(FP,TP,type='l',col='blue',cex.lab=2.0)
abline(0,1,lty=2)
title(main='ROC',cex.main=2)
plot(N,TP,type='l',col='blue',cex.lab=2.0)
abline(0,1,lty=2)
title(main='Lift',cex.main=2)
temp = liftf(y,phat.b,dopl=FALSE)
lines((1:length(y))/length(y),temp,col='red',lty=3)


temp = liftf(y,phat.b)


##################################################
# Get out of sample lift and compare them.
##################################################
source('Sec3Functions.txt')
Default$balsq = Default$balance^2
#--------------------------------------------------
#train/test
set.seed(99)
n=nrow(Default)
ntrain=floor(.75*n)
iitrain = sample(1:n,ntrain)
Deftrain = Default[iitrain,]
Deftest = Default[-iitrain,]
#--------------------------------------------------
#more on lift
glm.b = glm(default~balance,Deftrain,family=binomial)
phat.b = predict(glm.b,newdata=Deftest,type='response')

glm.i = glm(default~income,Deftrain,family=binomial)
phat.i = predict(glm.i,newdata=Deftest,type='response')

glm.b2 = glm(default~balance+balsq,Deftrain,family=binomial)
phat.b2 = predict(glm.b2,newdata=Deftest,type='response')

ytest = as.numeric(Deftest$default)-1

l.b = liftf(ytest,phat.b,dopl=FALSE)
l.i = liftf(ytest,phat.i,dopl=FALSE)
l.b2 = liftf(ytest,phat.b2,dopl=FALSE)

lmat = cbind(l.i,l.b,l.b2)
rgy = range(lmat)
ii = (1:nrow(lmat))/nrow(lmat)
rgx = range(ii)

plot(rgx,rgy,xlab='N',ylab='lift',type='n')
for(i in 1:ncol(lmat)) 
   lines(ii,lmat[,i],col=i+1)
abline(0,1,lty=2)
legend('bottomright',legend=c('income','balance','balance+square'),col=c(2,3,4),lwd=3)
