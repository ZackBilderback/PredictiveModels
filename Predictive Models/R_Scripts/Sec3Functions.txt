### Functions...
################

loss <- 
function(y, pred, eps=.0001)
{
#Loss is a function to compute the appropriate loss,
#based on binary, categorical, or numeric y vector,
#a prediction vector pred, and a minimum probability tolerance eps.
#GSB 41201, U. of Chicago.

val <- 0
#Factors first.
if(is.factor(y))
{
  #Trim away a tree result, using the second or higher value column.
  if((length(levels(y))==2) & is.matrix(pred)) pred <- pred[,2]

  if(!is.matrix(pred))
	{
 	#assume y is 0 or 1 and pred is prob of a 1
	ny <- as.numeric(y)-1
 	pred[pred < eps] <- eps
	pred[pred > (1-eps)] <- 1-eps
	temp <- -2*(ny*log(pred) + (1-ny)*log(1-pred))
 	val <- sum(temp)
	}

  else #Categorical y, loop is quite slow.
	{
	for(i in 1:length(y))
	  {
	  temp <- pred[i,as.character(y[i])]
	  ltemp <- -2*log(max(temp,eps))
	  val<- val+ltemp
	  }
	}
  }

else #Numeric Y
  {val <- sum((y-pred)^2)}

return(val)
}



##################################################
# lift function.
##################################################
#--------------------------------------------------
#plots lift function
liftf = function(yl,phatl,dopl=TRUE) {
oo = order(-phatl)
sy = cumsum(yl[oo])/sum(yl==1)
if(dopl) {
   ii = (1:length(sy))/length(sy)
   plot(ii,sy,type='l',lwd=2,col='blue',xlab='% tried',ylab='% of successes',cex.lab=1.5)
   abline(0,1,lty=2)
}
return(sy)
}


