print("wilcoxon signed rank test for one sample :")
print("enter observations:")
x <- scan()
print("enter mean or median value:")
muew <- scan()
print("enter level of significance:")
los <- scan()
t <- wilcox.test(x,mu=muew,alternative="two.sided")
if (t$p.value>los){
  print("accept Ho")
}else{
  print("reject Ho") 
}
