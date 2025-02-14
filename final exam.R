rm(list=ls())

library(tidyquant)
library(timetk)

#1. 使用套件tidyquant, timetk，並讀入資料  https://github.com/swtzang/FinDB_2019/tree/master/data_wrangle_practice/tej_day_price_2017_2018.txt
stock_day <- read_tsv("C:/Users/Sarah/Desktop/FinDB2019_SalesAnalysis/FinDB_2019_Sales_Analysis/data_wrangle_practice/tej_day_price_2017_2018.txt")
glimpse(stock_day)

#2. 選取欄位“證券代碼”, “簡稱”, “年月日”, “收盤價(元)”, “市值(百萬元)”, 並將名稱改為“id”, “name”, “date”, “price”, “cap”。
price_day <- stock_day %>% 
  rename(id    = 證券代碼, 
         name  = 簡稱, 
         date  = 年月日, 
         price = `收盤價(元)`,
         cap   = `市值(百萬元)`
  )

dim(price_day)
glimpse(price_day)
price_day

#3. 選取id, date, price, 並將id改為文字格式，date改為日期格式，並將資料格式改為寬資料。提示：使用spread()。
price_day1 <- stock_day %>% 
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

dim(price_day1)
glimpse(price_day1)
price_day1

#4. 檢查含有NA的股票代碼及其NA的個數。
price_day1_na <- price_day1 %>% 
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value!=0)
price_day1_na

price_day1_na1 <- price_day1 %>% 
  # last observation carried forward
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value!=0)

price_day1_na1

#5. 將NA值以最近的股價取代。提示：使用na.locf()
price_day_clear <-  price_day1 %>% 
  na.locf(fromLast = TRUE, na.rm=FALSE) %>%
  select(-c("2025", "6131"))

price_day_clear

#6. 刪除上題中仍含有NA值的股票, 並確認股票數量及筆數。
dim(price_day_clear)

#7. 將資料轉為xts(提示：可用tk_xts()), 計算日報酬率(以log計算)(提示：可用Return.calculate()), 並刪除第一筆沒有報酬率的資料。請顯示前五檔股票第1-5天的報酬率。
ret_day <- price_day_clear %>% 
  select(1:6)  %>% 
  tk_xts(select = -date, date_var = date) %>% 
  Return.calculate(method = "log")  %>%
  na.omit()

dim(ret_day)
ret_day

head(ret_day,5)

#8. 計算月報酬率(以log計算)(提示：可用Return.calculate()), 並刪除第一筆沒有報酬率的資料。請顯示前五檔股票第1-5天的報酬率。
price_day.xts <- price_day_clear %>%
                 select(1:6)  %>%
  tk_xts(select = -date, date_var = date)  

ret_mon.xts <- price_day.xts %>% 
  to.period(period = "months", 
            indexAt = "lastof", 
            OHLC= FALSE) %>% 
  Return.calculate(method = "log") %>%
  na.omit()
  
dim(ret_mon.xts)
ret_mon.xts

head(ret_mon.xts,5)

#9. 找出2017及2018年年底市值最大的前20家公司代碼, 簡稱, 並修改資本額格式，計算每家公司市值佔20家總市值的百分比。提示：使用filter(), arrange(), slice(), sum()。
tej20 <- read_tsv("C:/Users/Sarah/Desktop/FinDB2019_SalesAnalysis/FinDB_2019_Sales_Analysis/data_wrangle_practice/tej_day_price_2017_2018.txt", col_names = TRUE)
glimpse(tej20)

tej1<-tej20 %>% select('證券代碼', '簡稱', '年月日', '市值(百萬元)') %>% 
  rename(id = '證券代碼', name = '簡稱', date = '年月日', cap = '市值(百萬元)') %>%      
  mutate(date = date %>% as.character %>% as.Date('%Y%m%d')) %>% 
  mutate(id = id %>% as.character) %>% 
  arrange(desc(date), desc(cap)) %>%  
  select(3,4,1,2) %>%
  slice(1:20, 224877:224896) 


tej1
glimpse(tej1)

#10. 將2017年前20大公司市值以圖形表示如下。注意：市值由大小排列順序。
tej2<-tej20 %>% select('證券代碼', '簡稱', '年月日', '市值(百萬元)') %>% 
  rename(id = '證券代碼', name = '簡稱', date = '年月日', cap = '市值(百萬元)') %>%      
  mutate(date = date %>% as.character %>% as.Date('%Y%m%d')) %>% 
  mutate(id = id %>% as.character) %>% 
  arrange(desc(date), desc(cap)) %>%  
  select(3,4,1,2) %>%
  slice(224877:224896)

tej2

#11. 將題7的日報酬格式由寬格式改為長格式(如下),並只選取2018年的資料。提示：可用tk_tbl()將資料xts轉為tibble格式。並用gather()將寬資料轉為長資料。
tej_day_price_2017_2018.tbl = ret_day %>% 
  tk_tbl(select = -date, date_var = date) %>%
  select(2:6) %>%
  gather(key = id, value = ret)

tej_day_price_2017_2018.tbl

#12. 利用題9的20檔股票代碼，找出相對應20檔股票在2018年的日報酬率。提示：利用filter()。


#13. 依前題，計算20檔股票每月報酬率。提示：將每月中的每天報酬率加總，即可以得每月報酬率。利用as.yearmon()將日期轉為年月，並利用group_by(), summarize()計算分組報酬率總和。






