rm(list=ls())

library(data.table)


d.price = read.table("D/FinDB_2019_Sales_Analysis/Price_2010-2018_day.txt",fileEncoding = "UTF8")
d.price = d.price[,-2]
colnames(d.price) = c("id","date","close")

dprice.reorder = dcast(d.price,date~id)
dim(dprice.reorder)

write_rds(dprice.reorder,"d_price.rds")
head(dprice.reorder)
