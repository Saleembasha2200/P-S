print("enter level of significance:")
los <- scan()
#colname <- c('labour','clerks','technicians','executives')
print("enter row size:")
rowsize <- scan()
rowname <-c()
print("enter row elements:")
for (i in 1:rowsize){
  rowname <- append(rowname,readline())
}
print("enter column size:")
colsize <- scan()
colname <- c()
print("enter column elements:")
for (i in 1:colsize){
  colname <- append(colname,readline())
}
print("enter  elements:")
values <- scan()
#rowname <- c('type1','type2','type3')
#values <- c(190,243,197,82,44,44,23,78,34,5,12,8)
table <- matrix(values,byrow=TRUE,nrow=rowsize,dimnames = list(rowname,colname))
colsum <- c()
for (i in 1:colsize)
{
  colsum <- append(colsum,sum(table[,i]))
}
table <- rbind(table,colsum)
rowsum <- c()
for (i in 1:rowsize){
  rowsum <- append(rowsum,sum(table[i,]))
}
table <- cbind(table,rowsum)
table[rowsize+1,colsize+1]=sum(rowsum)
sample_size <- table[rowsize+1,colsize+1]
View(table)
data1<- c()
for (i in 1:rowsize){
  for (j in 1:colsize){
    data1 <- append(data1,(rowsum[i]*colsum[j])/sample_size)
  }
}
expectedfrequenciestable <- matrix(data1,byrow=TRUE,nrow=rowsize,dimnames = list(rowname,colname))
View(expectedfrequenciestable)
chisquarecal <- sum((values-data1)**2/data1)
cat("chisquare calculated value:",chisquarecal,"\n")
dof <- (length(rowname)-1)*(length(colname)-1)
chisquaretable <- qchisq((1-los),dof)
cat("chisquare table value:",chisquaretable,"\n")

if (chisquaretable>chisquarecal)
{
  print("the factors in the contingency table are independent")
}else{
  print("the factors in the contingency table are dependent")
}
