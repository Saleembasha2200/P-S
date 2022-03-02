
# Clear R workspace
rm(list = ls() ) 

# Declare a variable to read and store moviesData  
movies <- read.csv("moviesData.csv")

# View the stored data frame 
View(movies)

# View the dimension of the data frame 
dim(movies)

hist(movies$runtime)

hist(movies$runtime,
     main="distribution of movies length",
     xlab="runtime of movies",
     xlim = c(0,300),
     col="red")

hist(movies$runtime,
     main="distribution of movies length",
     xlab="runtime of movies",
     xlim = c(0,300),
     col="red",
     breaks=4)

# PIE CHART
genere <- table(movies$genre)
genere
View(genere)
pie(genere)

pie(genere,
    main="proportion of movie generes",
    border="red",
    col="blue")


#BAR CHART
m <- movies[1:20,]
View(m)
m$imdb_rating
barplot(m$imdb_rating)

barplot(m$imdb_rating,
        xlab="movie names",
        ylab="ratings",
        main="MOVIES",
        col="red")

barplot(m$imdb_rating,
        xlab="movie names",
        ylab="ratings",
        main="MOVIES",
        col="red",
        ylim = c(0,10))

barplot(m$imdb_rating,
        xlab="movie names",
        ylab="ratings",
        main="MOVIES",
        col="red",
        ylim = c(0,10),
        names.arg = m$title)

barplot(m$imdb_rating,
        xlab="movie names",
        ylab="ratings",
        main="MOVIES",
        col="red",
        ylim = c(0,10),
        names.arg = m$title,
        las=2)


#SCATTER PLOT
plot(x=movies$imdb_rating,
     y=movies$audience_score)

plot(x=movies$imdb_rating,
     y=movies$audience_score,
     main="Review Analysis",
     xlab="IMDB Rating",
     ylab="Audiance score",
     xlim = c(0,10),
     ylim = c(0,100),
     col="blue")

cor(movies$imdb_rating,movies$audience_score)

#ggplot2 (Grammer of graphics)

x <- seq(-pi,pi,0.1)
y <- sin(x)
plot(x,y)

plot(x,
     y,
     main="Plotting",
     ylab="sin(x)",
     type="l",
     col="blue")

plot(x,
     y,
     main="Plotting",
     ylab="sin(x)",
     type="l",
     col="blue")
lines(x,
      cos(x),
      col="red")
legend("topleft",
       c("sin(x)","cos(x)"),
       fill=c("blue","red"))

install.packages("ggplot2")
library(ggplot2)


#Polygon
x<-1:10
y<-sample(2:10,10,replace=TRUE)

plot(x,y,type="l") polygon(c(1,x,10),c(0,y,0),col="red")

mtcars
View(mtcars)
c <- table(mtcars$gear)
c
str(c)
barplot(c,main='car',xlab = 'distribution',horiz = T,
        legend=rownames(c),col=c("red", "yellow", "green"))



mtcars$gear
mtcars$vs
counts <-table(mtcars$vs, mtcars$gear)
counts

barplot(counts, 
        main="Car Distribution by Gears and VS",
        xlab="Number of Gears", 
        col=c("grey","cornflowerblue"),
        legend = rownames(counts))


slices <-c(10, 12,4, 16, 8)
lbls<-c("US", "UK", "Australia", "Germany", "France")
pie( slices, labels = lbls, main="Simple Pie Chart")

#Colors
slices <-c(10, 12,4, 16, 8)
pct<-round(slices/sum(slices)*100)
lbls<-paste(c("US", "UK", "Australia", 
              "Germany", "France"), " ", pct, "%", sep="")
pie(slices, labels=lbls, 
    col=rainbow(5),main="Pie Chart with Percentages")

v <- c(1,2,3,4,3,2,1,2,32,32,32,2)
barplot(table(v))
x <- table(v)
x        


hist(x)

mtcars$mpg#miles per gallon data 
hist(mtcars$mpg)

# Colored Histogram with Different Number of Bins
hist(mtcars$mpg, breaks=20, col="lightgreen")



plot(density(x))
# kernel Density Plot
density_data<-density(mtcars$mpg) 
plot(density_data) 
# Filling density Plot with colour 
density_data<-density(mtcars$mpg) 
plot(density_data, main="Kernel Density of Miles Per Gallon")
polygon(density_data, col="skyblue", border="black")


lines(x,y,type=)
weight <-c(2.5, 2.8, 3.2, 4.8, 5.1,5.9, 6.8, 7.1, 7.8,8.1)
months <-c(0,1,2,3,4,5,6,7,8,9)

plot(months, 
     weight, type = "b", 
     main="Baby weight chart")


vec<-c(3, 2, 5, 6, 4, 8, 1, 2, 3, 2, 4)
summary(vec)
boxplot(vec, varwidth= TRUE)
#varwidth=TRUE to make box plot proportionate to width



x=1.23456
x
options(digits = 2)




