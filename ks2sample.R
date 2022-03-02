x<-c(40,30,40,45,55,30)
y<-c(50,55,45,55,60,40)
t<-ks.test(x,y,paired=T)
los <- 0.05
if (t$p.value>los){
  print("accept Ho")
}else{
  print("reject Ho") 
}
