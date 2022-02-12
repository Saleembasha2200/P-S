#t-test for one-mean SST
height <- c(63,63,66,67,68,69,70,70,71,71)
n <- length(height)
los <- 0.05
muew <- 66
mean <- sum(height)/length(height)
sd <- sqrt(sum((height-mean)**2)/(n-1))
print(mean)
print(sd)
tcal <- (mean-muew)/(sd/sqrt(n))
print(tcal)
ttable <- qt(1-(los/2),df=9)
print(ttable)
if (ttable>tcal)
{
  print("the sample comes from a normal population whose mean height is 66")
}else{
  print("the sample doesnot comes from a normal population whose mean height is 66")
}
#t-test for one-mean LST
n <- 400
muew <- 67.39
sigma <- 1.3
mean <- 67.47
los <- 0.01
zcal <- (mean-muew)/(sigma/sqrt(n))
print(zcal)
ztable <- qnorm(1-los/2)
print(ztable)
if(abs(zcal)<ztable)
{
  print("sample has been taken from large population with mean height 67.39 inches and with sd 1.3 inches")
}else{
  print("sample has not been taken from large population with mean height 67.39 inches and with sd 1.3 inches") 
}
