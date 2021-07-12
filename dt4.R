library(ggplot2)
library(GGally)
library(lubridate)
getwd()
Data <- covid_19_data_cleaned
str(Data)
# Dùng thu vi???n này d??? group l???i các thành ph???n 
library('plyr')
Data <- ddply(Data, .(Date, Country), numcolwise(sum))
str(Data)
Data <- within(Data, rm(Lat, Long))
str(Data)
US <- subset(Data, Country == 'US')
latest <- subset(Data, Date==max(Date))
latest  
latest <- latest[order(latest$Confirmed, decreasing = TRUE),]
top10 <- latest[1:10,]
rownames(top10) <- 1:10
top10
# b???t d???u v??? bi???u d???
# cái thu 16 bieu do Bar Charts
ggp <- ggplot(top10, aes(x= Country, y= Confirmed))
ggp + geom_col(color='blue', fill= 'yellow')

#sap xep lai theo thu quoc gia
ggp <- ggplot(top10, aes(x= reorder(Country, -Confirmed), y= Confirmed))
ggp + geom_col(color='blue', fill= 'yellow')
# Thêm phu de vào
ggp <- ggplot(top10, aes(x= reorder(Country, -Confirmed), y= Confirmed))
ggp + geom_col(color='blue', fill= 'yellow') + labs(title = 'Top10 nuoc bi nhiem Covid-19(Tính tai 7/8/2021)',
                                                    subtitle = 'Covid -19 dataset Worldwide',
                                                    caption = 'Data Source: Johns Hopkins Univ,') +
  theme(plot.title =  element_text(color = '#731313', size =24, face = 'bold'),
        plot.subtitle =  element_text(color = '#254721', size =15, face = 'italic'),
        plot.caption=  element_text(color = '#142751', size =8, face = 'italic'))
#cái thu 17 Ve bieu do 20 nuoc 
ggp1 <- ggplot(latest[1:20,], aes(x= Country, y= Confirmed))
ggp1 <- ggplot(latest[1:20,], aes(x= reorder(Country, -Confirmed), y= Confirmed))
ggp1 + geom_col(color='red', fill= 'green')
ggp1
#cái thu 18 ve bieu do scatter Plot
ggp2 <- ggplot(US, aes(x= Date, y= Confirmed))
ggp2 + geom_point(size =0.5, color ='blue') + labs( title = 'Tong so nguoi nhiem nuoc My(tính toi tháng 1/2020 den tháng 7/2021',
                                                    subtitle = 'Covid-19 Cases',
                                                    caption ='Data Source: Johns Hopkins Univ,' )
ggp2
#cái thu 19  Ve bieu do line plot
gp2 <- ggplot()
ggp3 <-ggp2 + geom_line(data = US, aes(x= Date, y= Confirmed),size =2, color ='yellow') +
              geom_line(data = US, aes(x= Date, y= Recovered),size =2, color ='#15EC12')+
              labs( title = 'Tong so nguoi nhiem và hoi phuc cua nuoc My(tính ten tháng 1/2020 den tháng 7/2021',
                                                    subtitle = 'Covid-19 Cases',
                                                    caption ='Data Source: Johns Hopkins Univ,' )
ggp3 

