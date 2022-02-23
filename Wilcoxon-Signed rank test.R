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
print("wilcoxon signed rank test for (paired sample test ) n1==n2:")
print("enter first observations:")
x <- scan()
print("enter second observations:")
y <- scan()
print("enter level of significance:")
los <- scan()
t <- wilcox.test(x,y,paired=TRUE,alternative="two.sided")
if (t$p.value>los){
  print("accept Ho")
}else{
  print("reject Ho") 
}
