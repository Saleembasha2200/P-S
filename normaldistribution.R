print("enter lower limits of the class interval:")
lower <- scan()
print("enter upper limits of the class interval:")
upper <- scan()
print("enter frequencies :")
fo <- scan()
print("enter level of significance:")
los <- scan()
x <- (lower+upper)/2
fx <- fo*x
fxsquare <- fo*x^2
n <- sum(fo)
muew <- sum(fx)/n
sigma <-sqrt((sum(fxsquare)/n)-(muew)^2) 
z1 <- (lower-muew)/sigma
z2 <- (upper-muew)/sigma
fz1 <- pnorm(lower,muew,sigma)
fz2 <- pnorm(upper,muew,sigma)
p <- round((fz2-fz1),digits=5)
fe <- round((n*p),digits=0)
mydata <- data.frame(lower,upper,fo,x,fx,fxsquare,z1,z2,p,fe)
bind <-c(NA,NA,sum(fo),NA,sum(fx),sum(fxsquare),NA,NA,NA,NA)
mydata <- rbind(mydata,bind)
View(mydata)
result <- chisq.test(fo,p=p,rescale.p = TRUE)
chisquarecalculated=result[1]
print("chisquare calculated value is: ")
print(chisquarecalculated)
chisquaretable <- qchisq(1-los,result$parameter)
cat("chisquare table value is:",chisquaretable,"\n")
if (chisquaretable>chisquarecalculated){
  print("data fits into normal distribution")
}else{
  print("data fits does not into normal distribution")
}
