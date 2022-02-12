#F-test for SST
x <- c(9,11,13,11,15,9,12,14)
n1 <- length(x)
y <- c(10,12,10,14,9,8,10)
n2 <- length(y)
los <- 0.01
fbuiltin <- var.test(x,y)
meanx <- sum(x)/n1
meany <- sum(y)/n2
sxsquare <-(sum((x-meanx)**2))/(n1-1)
print(sxsquare)
sysquare <- (sum((y-meany)**2))/(n2-1)
print(sysquare)
fcal <- sxsquare/sysquare
if (fcal<1)
{
  fcal <- 1/f
  print(fcal)
  ftable <- qf(1-los,n2-1,n1-1)
  print(ftable)
}else{
  print(fcal)
  ftable <- qf(1-los,n1-1,n2-1)
  print(ftable)
}
if(ftable<fcal)
{
  print("null hypothesis rejected and alternative hypothes is accepted")
}else{
  print("null hypothesis accepted")
}
#F-test for LST
n1 <- 40
sd1 <- 34
n2 <- 60
sd2 <- 28
los <- 0.05
zcal <- (sd1-sd2)/(sqrt(((sd1^2)/(2*n1))+((sd2^2)/(2*n2))))
print(zcal)
zcritical <- qnorm(1-(los/2))
print(zcritical)
if(zcritical<zcal)
{
  print("null hypothesis rejected and alternative hypothes is accepted")
}else{
  print("null hypothesis accepted")
}
