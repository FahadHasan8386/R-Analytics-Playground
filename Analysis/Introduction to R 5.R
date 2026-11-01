

df_iris<-read.csv("Lab5/iris.csv",sep=",")
colSums(is.na(df_iris))

x<-df_iris$sepal_length
mean(x)
df_iris$sepal_length<-(x-mean(x))/var(x)


df_iris$sepal_length[is.na(df_iris$sepal_length)]<-mean(df_iris$sepal_length
                                                        ,na.rm = TRUE)
colSums(is.na(df_iris))


df_iris$species[is.na(df_iris$species)]<-
  names(which.max(table(df_iris$species)))

colSums(is.na(df_iris))

table(df_iris$species)

barplot(df_iris$sepal_length,main="sepal length",ylab = "Frequency")


barplot(table(df_iris$species))

pie(table(df_iris$species))


par(mfrow=c(2,2))
hist(df_iris$sepal_length)
hist(df_iris$sepal_width)
hist(df_iris$petal_length)
hist(df_iris$petal_width)

hist(df_iris$sepal_length,col = "red",breaks = 12)


x <- df_iris$sepal_length
h<-hist(x,
        breaks=12,
        col="red",
        xlab="sepal_length",
        main="Histogram with normal curve and box")
xfit<-seq(min(x), max(x), length=40)
yfit<-dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
box()

d <- density(df_iris$sepal_length)
plot(d)



d <- density(df_iris$sepal_length)
plot(d, main=" Density of sepal_length")
polygon(d, col="red", border="blue")
rug(df_iris$sepal_length, col="brown")



boxplot(df_iris$sepal_length)



plot(df_iris$sepal_length,df_iris$sepal_width)


colSums(is.na(df_iris))
head(df_iris$sepal_length)

#min max normalization
x<-df_iris$sepal_length

df_iris$sepal_length<- (x-min(x))/(max(x)-min(x))

