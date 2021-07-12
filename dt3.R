Data1 = X12_31_2020
nrow(Data1)
ncol(Data1)
head(Data1, 10)
tail(Data1, 10)
str(Data1)
summary(Data1)
#ch???n m???t c???t
Data1$Country_Region
#l???y nhi???u c???t
Data3 <- Data1[, c("Country_Region", "Deaths" ,"Recovered", "Confirmed", "Active")]
Data2 <- Data1[1:3,4:10]

#Data3[groupaby(Country_Region)]
Data3 <- ddply(Data3, .(Deaths, Country_Region), numcolwise(sum))
ggplot(Data3 , aes(Recovered, fill = Country_Region) +
  geom_density(position = "stack")
Summary(Data3) 
str(Data3)
Summary.data.frame(Data3)
#cai thu 11 ve bieu do plot
plot(Data3$Recovered, Data3$Deaths, col = "Orange", xlab = "Só nguoi hoi phuc", ylab= "So nguoi chet")
abline( lm(Data3$Recovered, Data3$Deaths), col ="red", lwd= 2)
# cái thu 12 Bieu do Scatterpot
library(car)

scatterplot(Data3$Recovered, Data3$Deaths, xlab = "So nguoi chet",
          ylab = "So Nguoi hoi phuc", main= "Bieu do Data12")
# cái th 13 Bieu dò plot
plot(Data3$Confirmed, Data3$Deaths,col ="Red"
     ,xlab =  "S??? ngu???i nhi???m",ylab = "S??? ngu???i ch???t", pch = 21, main = "Bi???u d??? phân tán")
# Bi???u d??? histogram
hist(Data3$Deaths, beak = 100, freq = FALSE)
#cái thu 14 ve ggplot
p = ggplot(Data3, aes (x = Confirmed, y= Deaths))
p+ geom_point(size =2, color= 'red') + geom_smooth() +theme_bw() + xlab("Xác nh???n") + ylab(" T??? vong")+ ggtitle("S??? ca nhi???m và thi???t m???ng")
# cái thu 15 ve ggplot dùng bang duong
p+ geom_line() + geom_smooth() +theme_bw()
# v??? gglot th??? hi???n ngu???i kh???i và ngu???i ch???t


