
library(randomForest)
cars <- read.csv("Cars.csv")
attach(cars)
cars = cars[,-c(1)]
carsX <- read.csv("cars_X_out.csv")
carsx = carsX[,-c(1)]

#train, val, test
set.seed(99)
n=nrow(cars)
n1=floor(n/2)
n2=floor(n/4)
n3=n-n1-n2
ii = sample(1:n,n)
cartrain=cars[ii[1:n1],]
carval = cars[ii[n1+1:n2],]
cartest = cars[ii[n1+n2+1:n3],]



#--------------------------------------------------
set.seed(1)
p=ncol(cartrain)-1
mtryv = c(4)
ntreev = c(200)
parmrf = expand.grid(mtryv,ntreev)
colnames(parmrf)=c('mtry','ntree')
nset = nrow(parmrf)
olrf = rep(0,nset)
ilrf = rep(0,nset)
rffitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing rf ',i,' out of ',nset,'\n')
  temprf = randomForest(price~.,data=cartrain,mtry=parmrf[i,1],ntree=parmrf[i,2])
  ifit = predict(temprf)
  ofit=predict(temprf,newdata=carval)
  olrf[i] = sum((carval$price-ofit)^2)
  ilrf[i] = sum((cartrain$price-ifit)^2)
  rffitv[[i]]=temprf
}
ilrf = round(sqrt(ilrf/nrow(cartrain)),3); olrf = round(sqrt(olrf/nrow(carval)),3)
#----------------------------------------
#print losses

print(cbind(parmrf,olrf,ilrf))

iirf=which.min(olrf)
therf = rffitv[[iirf]]
therfpred=predict(therf,newdata=carval)



#--------------------------------------------------------------#
# Run random forest with best parameters found from above code #
#--------------------------------------------------------------#
set.seed(1)
cartrainval = rbind(cartrain,carval)
finrf = randomForest(price~.,data=cars,mtry=4,ntree=200)
finrfpred=predict(finrf,newdata=carsX)
#--------------------------------------------------
#plot y vs yhat for test data

finrfrmse = sqrt(sum((cartest$price-finrfpred)^2)/nrow(cartest))
cat('finrfrmse: ',finrfrmse,'\n')
plot(cartest$price,finrfpred,xlab='test price',ylab='rf pred', main="RF Actual vs. Fitted")
abline(0,1,col='red',lwd=2)

#--------------------------------------------------
#plot variable importance

varImpPlot(finrf)
#--------------------------------------------------
rm(list=ls())




#--------------------------------------------------
#get rf fits for different number of trees
#note: to get this to work I had to use maxnodes parameter of randomForest!!!

ntreev = c(10,50,100,150)
nset = length(ntreev)
fmat = matrix(0,n,nset)
for(i in 1:nset) {
  cat('doing Cars rf: ',i,'\n')
  rffit = randomForest(price~mileage,data=cars,ntree=ntreev[i],maxnodes=15)
  fmat[,i] = predict(rffit)
}
#--------------------------------------------------
#plot oob error using last fitted rffit which has the largest ntree.


par(mfrow=c(1,1))
plot(rffit)

#--------------------------------------------------
#plot fits

par(mfrow=c(1,3))
oo = order(cars$mileage)
for(i in 1:nset) {
  plot(cars$mileage,cars$price,xlab='mileage',ylab='price')
  lines(cars$mileage[oo],fmat[oo,i],col=i+1,lwd=3)
  title(main=paste('bagging ntrees = ',ntreev[i]))
}



#--------------------------------------------------------------------#
# Final lines of code to make model with cars data and predict carsX #
#--------------------------------------------------------------------#
cars <- read.csv("Cars.csv")
attach(cars)
cars = cars[,-c(1)]
carsX <- read.csv("Cars_X_out.csv")
carsx = carsX[,-c(1)]
newcars <- read.table("Cars_Price2_out.txt", header = FALSE)
# levels(carsx$soundSystem) = levels(cars$soundSystem)
# levels(carsx$trim) = levels(cars$trim)
# levels(carsx$color) = levels(cars$color)
# levels(carsx$wheelSize) = levels(cars$wheelSize)
# levels(carsx$displacement) = levels(cars$displacement)

for(attr in colnames(cars))
{
  if (is.factor(cars[[attr]]))
  {
    new.levels <- setdiff(levels(cars[[attr]]), levels(carsx[[attr]]))
    if ( length(new.levels) == 0 )
    { print(paste(attr, '- no new levels')) }
    else
    {
      print(c(paste(attr, length(new.levels), 'of new levels, e.g.'), head(new.levels, 2)))
      levels(carsx[[attr]]) <- union(levels(carsx[[attr]]), levels(cars[[attr]]))
      levels(cars[[attr]]) <- union(levels(carsx[[attr]]), levels(cars[[attr]]))
    }
  }
}

finrf = randomForest(price~.,data=cars,mtry=4,ntree=200)
finrfpred = predict(finrf,newdata=carsx)

finrfrmse = sqrt(sum((carsx$Price-finrfpred)^2)/nrow(carsx)) #change cars to new file name

finrfpred_is = predict(finrf,newdata=cars)

finrfrmse_is = sqrt(sum((cars$price-finrfpred_is)^2)/nrow(cars))

plot(carsx$Price,finrfpred,xlab='test price',ylab='rf pred', main="RF Actual vs. Fitted")
abline(0,1,col='red',lwd=2)