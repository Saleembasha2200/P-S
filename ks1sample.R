x<-c(0.63, 0.49, 0.24, 0.57, 0.76, 0.89)
t<-ks.test(x,"punif")
los <- 0.05
if (t$p.value>los){
  print("accept Ho")
}else{
  print("reject Ho") 
}
