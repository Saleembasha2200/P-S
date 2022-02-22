#t-test for two means SST (n1!=n2)
x <- c(8260,8130,8350,8070,8340)
n1 <- length(x)
y <- c(7950,7890,7900,8140,7920,7840)
n2 <- length(y)
los <- 0.01
meanx <- mean(x)
print(meanx)
meany <- mean(y)
print(meany)
s <- sqrt((sum((x-meanx)**2)+sum((y-meany)**2))/(n1+n2-2))
print(s)
tcal <- (meanx-meany)/(s*sqrt((1/n1)+(1/n2)))
tc <- t.test(x,y)#built in method to calculate tcal in t test
print(tc)
print(tcal)
ttable <- qt(1-(los/2),df=n1+n2-2)
print(ttable)
if(ttable>tcal)
{
  print("difference between the means of these samples are not significant")
}else{
  print("difference between the means of these samples are significant")
}
#t-test for two means LST for both (n1!=n2) and (n1==n2)
n1 <- 40
n2 <- 40
mean1 <- 647
mean2 <- 638
sd1 <- 27
sd2 <- 31
los <- 0.05
zcal <- (mean1-mean2)/sqrt((sd1**2/n1)+(sd2**2/n2))
print(zcal)
zcritical <- qnorm(1-los) #for left tailed test write qnorm(confindence limits,lower.tail=true)-to get zcritical value
print(zcritical)          #for both two and right tailed test write qnorm(confindence limits)-to get zcritical value
if(zcritical>zcal)
{
  print("first sample is equal to second sample")
}else{
  print("first sample is suferior to second sample")
}
