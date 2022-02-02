#qchisq(confidence value,n)-gives the statistical table value of chi^2 at (100-confidence value) level of significance for n degrees of freedom 
x <- c(0,1,2,3,4,5,6,7)
f <- c(7,6,19,35,30,23,7,1)
fx <- f*x
len <- length(x)-1
mean <- sum(fx)/sum(f)
p <- mean/len
prob <- dbinom(x,len,p)
fe <- sum(f)*prob
mydata <- data.frame(x,f,fx,prob,fe)
sum <- c(NA,sum(f),sum(fx),NA,sum(fe))
mydata <- rbind(mydata,sum)
View(mydata)
val <- chisq.test(f,fe)
if(val$p.value>0.05)
{
  print("given data fits into binomial distribution")
}else{
  print("given data cannot be fitted into binomial distribution")
}
