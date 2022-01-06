library(tidyverse)
library(tsibble)

### Data Loading Section. 
###
### Currently, looking at data sources before
### outputting to a better currated .csv file

### OECD price data - seasonally adjusted and deflated
prices <- read_csv("Data\\DP_LIVE_03012022182027984.csv",
                   col_types = "ccccccd?") %>% 
  dplyr::select(-`Flag Codes`, -INDICATOR, -MEASURE) %>% 
  rename(houseprice = Value)

# quarterly price data
prices_Q_all <- prices %>% dplyr::filter(FREQUENCY == "Q") %>% 
  mutate(year = as.numeric(str_sub(TIME, 0, 4)),
         quarter = as.numeric(str_sub(TIME, 7, 7)))
prices_Q_real <- prices_Q_all %>% dplyr::filter(SUBJECT == "REAL") %>% 
  dplyr::select(-SUBJECT, -FREQUENCY, -TIME)

# annual price data
prices_A_all <- prices %>% dplyr::filter(FREQUENCY == "A") %>% 
  mutate(year = as.numeric(TIME))
prices_A_real <- prices_A_all %>% dplyr::filter(SUBJECT == "REAL") %>% 
  dplyr::select(-SUBJECT, -FREQUENCY, -TIME)
rm(prices)

### OECD Long-Term Interest
interest <- read_csv("Data\\DP_LIVE_03012022190210320.csv",
                col_types = "ccccccd?") %>%
  dplyr::select(-`Flag Codes`, -INDICATOR, -SUBJECT, -MEASURE) %>%
  rename(interest = Value)
# interest quarterly data
interest_Q_all <- interest %>% dplyr::filter(FREQUENCY == "Q") %>% 
  mutate(year = as.numeric(str_sub(TIME, 0, 4)),
         quarter = as.numeric(str_sub(TIME, 7, 7))) %>% 
  dplyr::select(-TIME, -FREQUENCY)
# interest annual data
interest_A_all <- interest %>% dplyr::filter(FREQUENCY == "A") %>% 
  mutate(year = as.numeric(TIME)) %>% 
  dplyr::select(-TIME, -FREQUENCY)
rm(interest)

### GDP data OECD
gdp <- read_csv("Data\\DP_LIVE_04012022213751140.csv",
            col_types = "ccccccd?") %>%
  dplyr::select(-`Flag Codes`, -INDICATOR) %>%
  rename(gdp = Value)
# GDP annual data
gdp_A <- gdp %>% 
  dplyr::filter(FREQUENCY == "A", SUBJECT == "VOLIDX", MEASURE == "IDX") %>% 
  mutate(year = as.numeric(TIME)) %>% 
  dplyr::select(-TIME, -FREQUENCY, -SUBJECT, -MEASURE)
# GDP quarterly data
gdp_Q <- gdp %>% 
  dplyr::filter(FREQUENCY == "Q", SUBJECT == "VOLIDX", MEASURE == "IDX") %>% 
  mutate(year = as.numeric(str_sub(TIME, 0, 4)),
         quarter = as.numeric(str_sub(TIME, 7, 7))) %>% 
  dplyr::select(-TIME, -FREQUENCY, -SUBJECT, -MEASURE)
  
### Data join annual data
data_A <- left_join(prices_A_real, interest_A_all) %>% 
  left_join(gdp_A) %>% 
  group_by(LOCATION) %>% 
  mutate(lag_houseprice = lag(houseprice, order_by = LOCATION)) %>% 
  ungroup()
data_Q <- left_join(prices_Q_real, interest_Q_all) %>% 
  left_join(gdp_Q)

### Starting model
### prices ~ stock + inflation + gdp + population

lm1 <- lm(houseprice ~ interest + gdp + lag_houseprice + as.factor(LOCATION),
           data = data_A)
lm2 <- lm(houseprice ~ interest + gdp + lag_houseprice + as.factor(LOCATION),
          data = data_A)
anova(lm1, lm2)

### Time-series data structure tsibble package
###

annual_ts <- data_A %>% as_tsibble(key = LOCATION, index = year)
quarterly_ts <- data_Q %>% 
  mutate(year_q = yearquarter(paste0(year, " Q", quarter))) %>% 
  as_tsibble(key = LOCATION, index = year_q)

### Write datasets
write_csv(data_A, "Data\\Annual_Data_timeseries.csv")
write_csv(data_Q, "Data\\Quarterly_Data_timeseries.csv")

### References for datasets

### House Prices - OECD (2021), Housing prices (indicator). doi: 10.1787/63008438-en (Accessed on 03 January 2022)
### Long-term Interest - OECD (2022), Long-term interest rates (indicator). doi: 10.1787/662d712c-en (Accessed on 05 January 2022)
### GDP - OECD (2022), Quarterly GDP (indicator). doi: 10.1787/b86d1fc8-en (Accessed on 05 January 2022)