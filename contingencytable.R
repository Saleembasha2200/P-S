print("enter level of significance:")
los <- scan()
colname <- c('labour','clerks','technicians','executives')
rowname <- c('type1','type2','type3')
values <- c(190,243,197,82,44,44,23,78,34,5,12,8)
table <- matrix(values,byrow=TRUE,nrow=4,dimnames = list(colname,rowname))
colsum <- c(sum(table[,1]),sum(table[,2]),sum(table[,3]))
table <- rbind(table,colsum)
rowsum <- c(sum(table[1,]),sum(table[2,]),sum(table[3,]),sum(table[4,]))
table <- cbind(table,rowsum)
table[5,4]=sum(rowsum)
sample_size <- table[5,4]
View(table)
data1<- c()
for (i in 1:4){
  for (j in 1:3){
    data1 <- append(data1,(rowsum[i]*colsum[j])/sample_size)
  }
}
expectedfrequenciestable <- matrix(data1,byrow=TRUE,nrow=4,dimnames = list(colname,rowname))
View(expectedfrequenciestable)
chisquarecal <- sum((values-data1)**2/data1)
print(chisquarecal)
dof <- (length(rowname)-1)*(length(colname)-1)
chisquaretable <- qchisq((1-los),dof)
print(chisquaretable)

if (chisquaretable>chisquarecal)
{
  print("the factors in the contingency table are independent")
}else{
  print("the factors in the contingency table are dependent")
}
