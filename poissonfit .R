#qchisq(confidence value,n)-gives the statistical table value of chi^2 at (100-confidence value) level of significance for n degrees of freedom 
x <- c(0:4)
f <- c(211,90,19,5,0)
fx <- f*x
mean <- sum(fx)/sum(f)
lambda <- mean
prob <- dpois(x,lambda)
fe <- sum(f)*prob
mydata <- data.frame(x,f,fx,prob,fe)
total <- c(NA,sum(f),sum(fx),NA,sum(fe))
mydata <- rbind(mydata,total)
View(mydata)
val <- chisq.test(f,fe)
if(val$p.value>0.05)
{
  print("given data fits into poissons distribution")
}else{
  print("given data cannot be fitted into poissonsl distribution")
}
