#doc file xo1_01_2021
View(X01_01_2021)
head(X01_01_2021)
attach(X01_01_2021)
str(dt)
dt = X01_01_2021
dt
dt <- dt[, c("Country_Region", "Deaths" ,"Recovered")]
#g???p d??? li???u
dt <- ddply(dt, .(Deaths, Country_Region), numcolwise(sum))
dt
hist(Confirmed,xlab = "S??? Ngu???i ch???t", ylab = "Probability" , col = "Blue", border = "white", main = "Deaths" )
lines(density(na.omit(Deaths), col="red", lwd =3)
table(X01_01_2021$Deaths)
#cái thu 1 bieu do the hien nguoi chet và tu vong vào ngày 1/1/2021
boxplot(Recovered ~ Deaths, col = "Blue", xlab = "So nguoi hoi phuc", ylab = 'So nguoi chet' ,notch = T, main = "Bieu do boxplot the hien nguoi chet và bình phuc",)

Bd <- dt$Country_Region
barplot (Bd, las =1, cex =0.5, cex.axis=1, cex.names=0.5, main = 'Các nuoc bi nhiem Covid (tính den 1/1/2021)')
plot (Bd)
boxplot(Recovered)
# cái thu 2 bieu do Barplot the hien so nguoi nhiem
library(ggplot2)
view(dt)
ggplot(data = dt) + geom_point(mapping = aes( x= Deaths, y = Recovered))
pie(dt[, 1:10])
