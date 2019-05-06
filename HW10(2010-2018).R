rm(list=ls())

library(data.table)


d.price = read.table("C:/Users/Sarah/Desktop/FinDB_2019_Sales_Analysis/Price_2010-2018_day.txt")
d.price = d.price[,-2]
colnames(d.price) = c("id", "", "", "date","close")
head(d.price)
d.price

dprice.reorder = dcast(d.price,date~id)
dprice.reorder
dim(dprice.reorder)
head(dprice.reorder)


write_rds(dprice.reorder,"d_price.rds")



