library(tidyquant)
library(timetk)

rm(list=ls())

stock_day <- read_tsv("D:/FinDB2019_SalesAnalysis/FinDB_2019_Sales_Analysis/final/tej_day_2018-2019.txt")
glimpse(stock_day)

price_day <- stock_day %>% 
  rename(id    = 證券代碼, 
         name  = 簡稱, 
         date  = 年月日, 
         price = `收盤價(元)`,
         cap   = `市值(百萬元)`
  ) %>% 
  mutate(id = as.character(id)) %>%
  mutate(date = as.Date(as.character(date), '%Y%m%d')) %>%
  select(id, date, price) %>% 
  spread(key = id, value = price) 

dim(price_day)


price_day_na <- price_day %>% 
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value!=0)
price_day_na


price_day_na.1 <- price_day %>% 
  # last observation carried forward
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value!=0)
price_day_na.1


price_day_clear <-  price_day %>% 
  na.locf(fromLast = TRUE, na.rm=FALSE) %>%
  select(-c("2025", "6131"))
dim(price_day_clear)


ret_day <- price_day_clear %>% 
  tk_xts(select = -date, date_var = date) %>% 
  Return.calculate(method = "log")
dim(ret_day)


price_day.xts <- price_day_clear %>%
  tk_xts(select = -date, date_var = date)  

ret_mon.xts <- price_day.xts %>% 
  to.period(period = "months", 
            indexAt = "lastof", 
            OHLC= FALSE) %>% 
  Return.calculate(method = "log")


###
ret_day_2stock = ret_day[-1, c("1101", "2330")] %>%
                 tk_tbl(., rename_index = `date`) %>%
                 gather(key = id, value = return, time = -data)


ret_day_2stock %>% ggplot(ase(x= return, fill= id))+
                   geom_density(color = "blue")+
                   facet_wrap(~id)



