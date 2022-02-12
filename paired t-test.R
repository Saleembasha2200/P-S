#paired t-test for two means SST (n1==n2)
x <- c(45,73,46,124,33,57,83,34,26,17)
y <- c(36,60,44,119,35,51,77,29,24,11)
los <- 0.05
n <- length(x)
di <- x-y
dbar <- sum(di)/n
print(dbar)
sd <- sqrt((sum((di-dbar)**2))/(n-1))
print(sd)
tcal <- dbar/(sd/sqrt(n))
print(tcal)
tcalbuiltin <- t.test(x,y,paired=TRUE)
print(tcalbuiltin)
ttable <- qt(1-los,df=n-1)
print(ttable)
if(ttable<tcal)
{
  print("null hypothesis rejected and alternative hypothes is accepted")
}else{
  print("null hypothesis accepted")
}
