x<-c(0.63, 0.49, 0.24, 0.57, 0.76, 0.89)
t<-ks.test(x,"punif")
print(t)