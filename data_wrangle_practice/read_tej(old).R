library(tidyquant)
library(timetk)

rm(list=ls())
stock_day_2_year<-read_tsv("D:/FinDB2019_SalesAnalysis/FinDB_2019_Sales_Analysis/data_wrangle_practice/tej_day_price_2017_2018.txt")
glimpse(stock_day_2_year)
# data wrangling
price_day_2_year <- stock_day_2_year %>% 
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
dim(price_day_2_year)
# compute number of na for each stock
price_day_2_year_na <- price_day_2_year %>% 
                       map_df(~sum(is.na(.))) %>% 
                       gather() %>% 
                       filter(value!=0)
price_day_2_year_na

# fill na with the most recent price and check data
price_day_2_year_na.1 <- price_day_2_year %>% 
                         na.locf(fromLast = TRUE, na.rm=FALSE) %>% 
                         map_df(~sum(is.na(.))) %>% 
                         gather() %>% 
                         filter(value!=0)
price_day_2_year_na.1
# remove stocks with na values
price_day_2_year_clear <-  price_day_2_year %>% 
                           na.locf(fromLast = TRUE, na.rm=FALSE) %>%
                           select(-c("2025", "6131"))
dim(price_day_2_year_clear)
#                          
ret_day_2_year <- price_day_2_year %>% 
                  na.locf(fromLast = TRUE) %>% 
                  tk_xts(select = -date, date_var = date) %>% 
                  Return.calculate(method = "log")
dim(ret_day_2_year)

#convert to daily price
ret_day_2_year <- price_day_2_year %>%
                  # convert from tibble into xts
                  tk_xts(select = -date, date_var = date) %>% 
                  na.locf(fromLast = TRUE) %>% 
                  Return.calculate(method = "log")
#convert to monthly return
price_mon_2_year <- price_day_2_year %>%
                    na.locf(fromLast = TRUE, na.rm = FALSE) %>% 
                    mutate(date = as.yearmon(date) %>% as.Date(frac = 1)) %>% 
                    distinct(date, .keep_all = TRUE)
#
temp <- sum(is.na(price_day_2_year[1, ]))
temp
#
dim(price_mon_2_year)                  
tail(price_mon_2_year[,1])
#

price_wk_2_year <- price_wk_2_year[-1,]
price_wk_2_year[1:20, 1:5]
tail(price_wk_2_year,1)
price_day_2_year[,1]

# convert into daily returns
ret_mon_2_year <- price_day_2_year %>%
                  tk_xts(select = -date, date_var = date) %>% 
                  na.locf(fromLast = TRUE, na.rm = FALSE) %>% 
                  to.monthly(indexAt = "last", OHLC = FALSE) %>% 
                  Return.calculate(method = "log")
dim(ret_mon_2_year)
ret_mon_2_year[, 1:5]
tail(ret_mon_2_year, 1)
#
                  
                  





