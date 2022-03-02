# curve fitting
#Straight line fit
x<-c(1,2,3,4,6,8)
y<-c(2.4,3,3.6,4,5,6)
n<-length(x)
xy<-x*y
xx<-x*x
mydata<- data.frame(x,y,xy,xx)
print(mydata)
sums<-list(sum(x),sum(y),sum(x*y),sum(x*x))
mydata<-rbind(mydata,sums)
print(mydata,row.names=FALSE)
stline<-lm(y~x)
print(stline)
summary(stline)
plot(x,y)
abline(stline,col="BLUE")



#Parabola fit
x<-c(0,1,2,3,4)
y<-c(1,1.8,1.3,2.5,6.3)
n<-length(x)
xy<-x*y
xx<-x*x
xxx<-x^3
xxxx<-x^4
xxy<-x^2*y
mydata<- data.frame(x,y,xy,xx,xxx,xxxx,xxy)
print(mydata)
sums<-list(sum(x),sum(y),sum(x*y),sum(x*x),sum(x^3),sum(x^4),sum(x^2*y))
mydata<-rbind(mydata,sums)
print(mydata,row.names=FALSE)
parabola <- lm(y ~ x+I(x^2))
print(parabola)
f<-coef(parabola)[1]+((coef(parabola)[2])*x)+((coef(parabola)[3])*x*x)
print(f)
plot(x,y)
curve((coef(parabola)[1]+(coef(parabola)[2]*x)+(coef(parabola)[3]*x*x)),from=x[1],n=x[n],add=T)
curve(predict(parabola,newdata=data.frame(x)),add=T)
