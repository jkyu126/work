
# Loads tidyquant
library(tidyverse)
library(tidyquant)

## Data Download
from = "1998-01-01"
sp500 <- tq_get("^GSPC", get = "stock.prices", from=from) 
djia <- tq_get("^DJI", get = "stock.prices", from=from)
vix <- tq_get("^VIX", get = "stock.prices", from=from) # FROM 1990
kospi <- tq_get("^KS11", get = "stock.prices", from=from) # FROM 1997
bond_20yr <- tq_get("TLT", get="stock.prices", from=from)
bond_1yr <- tq_get("SHY", get="stock.prices", from=from)

oil <- tq_get("DCOILWTICO", get = "economic.data", from=from)
gold <- tq_get("GOLDAMGBD228NLBM", get = "economic.data", from=from)
usd <- tq_get("EXKOUS", get="economic.data", from=from) #FROM 1981-04
home <-tq_get("USSTHPI", get="economic.data", from=from)


# Process data
sp500 = sp500 %>% mutate(price=adjusted, symbol = "sp500") %>% select(symbol, date, price)
djia = djia %>% mutate(price=adjusted, symbol = "djia") %>% select(symbol, date, price)
vix = vix %>% mutate(price=adjusted, symbol = "vix") %>% select(symbol, date, price)
kospi = kospi %>% mutate(price=adjusted, symbol="kospi") %>% select(symbol, date, price)
bond_20yr = bond_20yr %>% mutate(price=adjusted, symbol="bond_20yr") %>% select(symbol, date, price)
bond_1yr = bond_1yr %>% mutate(price=adjusted, symbol="bond_1yr") %>% select(symbol, date, price)

home = home %>% mutate(symbol = "us_home")
oil = oil %>% mutate(symbol = "oil")
gold = gold %>% mutate(symbol = "gold")
usd = usd %>% mutate(symbol = "usd")

DF = rbind(sp500, djia, kospi, vix, oil, gold, usd, us_home, bond_20yr, bond_1yr)
DF2 = DF %>% 
  mutate(time = paste0(year(date), 
                       ifelse(nchar(month(date))==1, 
                              paste0("0",month(date)), month(date)))) %>%
  group_by(symbol, time) %>%
  summarise(price = mean(price, na.rm=T)) 

DF3 = DF2 %>% 
  mutate(time = paste0(time, "01")) %>%
  mutate(time2 = as.Date(as.character(time), format = "%Y%m%d")) %>%
  group_by(symbol) %>%
  mutate(adjusted = price / price[1]) %>%
  ungroup() %>%
  select(symbol, time2, adjusted, price)

DF4 = DF3 %>%
  mutate(year = year(time2), period = NULL) %>%
  mutate(period = ifelse(1998<=year & year<=2002, "Phase0", 
                         ifelse(2004<=year & year<=2008, "Phase1",
                                ifelse(2010<=year & year<=2014, "Phase2",
                                       ifelse(2015<=year & year<=2019, "Phase3", "NOT_USED"))))) 

DF5 = DF4 %>%
  filter(period %in% c("Phase0", "Phase1", "Phase2", "Phase3")) %>%
  mutate(plusyear = ifelse(period=="Phase0", 18, 
                           ifelse(period=="Phase1", 17, 16))) %>%
  mutate(time3 = paste0(year(time2)+plusyear, 
                        ifelse(nchar(month(time2))==1, 
                               paste0("0", month(time2)), month(time2)), "01")) %>%
  mutate(time4 = as.Date(time3, format = "%Y%m%d"))

names = c("djia", "gold", "kospi", "oil", "sp500", "bond_1yr",
          "us_home", "bond_20yr", "usd", "vix")

mydat = DF5 %>% mutate(time = time4, price = adjusted) %>%
  select(symbol, time, price, period) %>% 
  arrange(match(symbol, names), time)


us_home = data.frame(year = rep(2016:2035, each=12), month = rep(1:12, 20)) %>%
  mutate(time = paste(year, ifelse(nchar(month)==1, paste0("0", month), month), "01", sep="-")) %>% 
  mutate(time = as.Date(time, format = "%Y-%m-%d")) %>%
  select(time, year) %>%
  left_join(mydat %>% filter(symbol=="us_home"), by="time") %>%
  mutate(symbol = "us_home") 

us_home$price[-c(239:240)] = na.approx(us_home$price)
us_home$price[239:240] = us_home$price[238]

us_home = us_home %>% mutate(period = ifelse(
  year<=2020, "Phase0", 
  ifelse(year>=2021 & year<=2025, "Phase1", 
         ifelse(year>=2026 & year<=2030, "Phase2", "Phase3"))
))
mydat2 = mydat %>% filter(symbol != "us_home")
us_home = us_home %>% select(symbol, time, price, period)
mydat = rbind(mydat2, us_home) %>%
  arrange(match(symbol, names), time)

phase0 = mydat %>% filter(period == "Phase0")
phase1 = mydat %>% filter(period == "Phase1")
phase2 = mydat %>% filter(period == "Phase2")
phase3 = mydat %>% filter(period == "Phase3")
    
write.csv(DF5, "data/entire_dataset.csv", row.names = F)
write.csv(mydat, "data/sample_dataset.csv", row.names = F)
write.csv(phase0, "data/sample_phase0.csv", row.names = F)
write.csv(phase1, "data/sample_phase1.csv", row.names = F)
write.csv(phase2, "data/sample_phase2.csv", row.names = F)
write.csv(phase3, "data/sample_phase3.csv", row.names = F)






