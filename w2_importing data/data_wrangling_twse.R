# Reference
# http://cm.nsysu.edu.tw/~msrc/wp/rmarkdown/tidyquan.html
rm(list=ls())
#
library(tidyquant)
# read.table() is too slow to import large data 
# tej <- read.table("tej_day_price_2017_2018_UTF8.txt", header = T, fileEncoding = "UTF-8-BOM")
# We use read_tsv
tej <- read_tsv("../sales_analysis/w2_importing data/tej_day_price_2017_2018_UTF8.txt", col_names = TRUE)
saveRDS(tej, "../sales_analysis/w2_importing data/tej_d_2017_18.rds")
tej <- readRDS("../sales_analysis/w2_importing data/tej_d_2017_18.rds")
# we can also save file in Rdata format
# but the size seems too large here!!!
#save(tej, file = "../sales_analysis/w2_importing data/tej_d_2017_18.Rdata")
# If import .txt, then chinese will be garbled text.
#tej1 <- read_tsv("tej_day_price_2017_2018.txt", col_names = TRUE)
# You can use the locale to setup to import .txt data;
#tej2 <- read_tsv("tej_day_price_2017_2018.txt", locale = locale(encoding='big5'))
#?read_tsv
glimpse(tej)

tej1<-tej %>% select('證券代碼', '年月日', '收盤價(元)') %>% 
  rename(ID = '證券代碼', date = '年月日', close = '收盤價(元)') %>%      
  mutate(date = date %>% as.character %>% as.Date('%Y%m%d')) %>% 
  mutate(ID = ID %>% as.character) %>% 
  arrange(ID)  
tej1

# select 3 stocks from tej1: 1101, 2317, 2330;
tej.3 <- tej1 %>%
  arrange(ID) %>% 
  filter(ID %in% c("1101", "2317", "2330"))
  #filter(ID %>% str_detect("1101"))
tej.3
tail(tej.3)

#
tej.3 <- tej.3 %>%
  group_by(ID) %>% 
  tq_mutate(select = c(close),    # 選擇收盤價
            mutate_fun = SMA,           # 選擇簡單移動平均線
            n = 5) %>%                  # 5日簡單移動平均線參數
  rename(ma5 = SMA)  %>% 
# 計算10日簡單移動平均線參數
  tq_mutate(select = c(close),
            mutate_fun = SMA,
            n = 10) %>%
  rename(ma10 = SMA) %>%
  # 計算20日簡單移動平均線參數
  tq_mutate(select = c(close),
            mutate_fun = SMA,
            n = 20) %>%
  rename(ma20 = SMA) %>% 
  ungroup()
tej.3        
# spread() long to wide;
tej.3 %>% select(-starts_with("ma")) %>% 
  spread(key = ID, value = close)


# import data of TWSE index
twse <- read_tsv("w2_importing data/twse_2017_2018.txt", locale = locale(encoding='big5'))
twse <- twse %>% 
  select(c(3,4)) %>% 
  rename(date = "年月日", close = '收盤價(元)') %>% 
  mutate(date = date %>% as.character %>% as.Date('%Y%m%d'))
glimpse(twse)
#
mktret <- twse %>% 
  mutate(ret = close/lag(close)-1) %>% 
  na.omit() %>% 
  rename(mktret = "ret")
mktret
#
stockret <- tej.3 %>% group_by(ID) %>%  
  mutate(ret = close/lag(close)-1) %>% 
  select(-'close') %>% 
  na.omit() %>% 
  ungroup()
stockret
# 整理各資產報酬率並且併入各交易日對應的市場報酬率
stockret <- stockret %>%
  left_join(mktret, by = c("date" = "date"))
stockret
#
stockret %>% group_by(ID) %>% 
  summarize(
    count = n(),
    avg = mean(ret), 
    med = median(ret),
    sd = sd(ret),
    min = min(ret),
    max = max(ret)
  ) %>% 
  ungroup()
# compute sharpeRatio
sharpeRatio <- stockret %>%
  group_by(ID) %>%
  tq_performance(Ra = ret,
                 Rb = NULL,
                 performance_fun = SharpeRatio,
                 Rf = 0.01/252)
sharpeRatio




#
capmTable <- stockret %>%
  group_by(ID) %>%
  tq_performance(Ra = ret, 
                 Rb = mktret, 
                 performance_fun = table.CAPM,
                 Rf = 0.01/252)
tail(capmTable)
