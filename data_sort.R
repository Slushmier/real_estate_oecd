library(tidyverse)
library(tsibble)
library(lme4)

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

### Housing stock data (may offload to separate script)
### This data is not a strict time-series, so I'm filling in blank
### values by continuing trends between years

# Import data, leave out 3 countries with only one observation
housing_builds <- read_csv("Data\\Housing-stock-and-construction.csv",
                           skip = 1) %>% 
  dplyr::filter(!Country %in% c("Korea", "Russian Federation", "South Africa"))

housing_builds <- housing_builds %>% 
  dplyr::filter(!is.na(Country)) %>% 
  mutate(rate1 = (Dwellings2 / Dwellings1) ^ (1 / (Year2 - Year1)) - 1,
         rate2 = (Dwellings3 / Dwellings2) ^ (1 / (Year3 - Year2)) - 1,
         ratepc1 = (PCDwellings2 / PCDwellings1) ^ (1 / (Year2 - Year1)) - 1,
         ratepc2 = (PCDwellings3 / PCDwellings2) ^ (1 / (Year3 - Year2)) - 1) %>% 
  mutate(rate2 = ifelse(is.na(rate2), rate1, rate2),
         ratepc2 = ifelse(is.na(ratepc2), ratepc1, ratepc2))

builds_df <- data.frame(country = unique(housing_builds$Country),
                        year = NA) %>% 
  complete(country, year = min(housing_builds$Year1, na.rm = T):
             max(housing_builds$Year3, na.rm = T)) %>% 
  mutate(Dwellings = 0.00, PCDwellings = 0.00) %>% 
  dplyr::filter(!is.na(year))

min_year <- min(housing_builds$Year1, na.rm = T)
max_year <- max(housing_builds$Year3, na.rm = T)

# My least favorite for loop ever
for (country in builds_df$country){
  filter_df <- housing_builds %>% dplyr::filter(Country == country)
  for (year in min_year:max_year){
    if (year < filter_df$Year1) {
      builds_df[builds_df$country == country & builds_df$year == year,
                c("Dwellings", "PCDwellings")] <- 
        list(filter_df$Dwellings1 * 
            ((1 -(filter_df$rate1)) ^ (filter_df$Year1 - year)),
          filter_df$PCDwellings1 * 
            ((1 -(filter_df$rate1)) ^ (filter_df$Year1 - year))
        )
    }
    if (year == filter_df$Year1) {
      builds_df[builds_df$country == country & builds_df$year == year,
                c("Dwellings", "PCDwellings")] <- 
        list(filter_df$Dwellings1, filter_df$PCDwellings1)
    }
    if (year > filter_df$Year1 & year < filter_df$Year2) {
      builds_df[builds_df$country == country & builds_df$year == year,
                c("Dwellings", "PCDwellings")] <- 
        list(filter_df$Dwellings2 * 
               ((1 -(filter_df$rate1)) ^ (filter_df$Year2 - year)),
             filter_df$PCDwellings2 * 
               ((1 -(filter_df$rate1)) ^ (filter_df$Year2 - year)))
        
    }
    if (year == filter_df$Year2) {
      builds_df[builds_df$country == country & builds_df$year == year,
                c("Dwellings", "PCDwellings")] <- 
        list(filter_df$Dwellings2, filter_df$PCDwellings2)
    }
    if (year > filter_df$Year2) {
      builds_df[builds_df$country == country & builds_df$year == year,
                c("Dwellings", "PCDwellings")] <- 
        list(filter_df$Dwellings2 * 
               ((1 + (filter_df$rate2)) ^ (year - filter_df$Year2)),
             filter_df$PCDwellings2 * 
               ((1 + (filter_df$rate2)) ^ (year - filter_df$Year2))
        )
      if (!is.na(filter_df$Year3)){
        if (year == filter_df$Year3) {
          builds_df[builds_df$country == country & builds_df$year == year,
                    c("Dwellings")] <- 
            filter_df$Dwellings3
        }
      }
    }
  }
}
 
# Need to replace the countries until country abbreviation
builds_df <- builds_df %>% 
  mutate(country = as.character(country)) %>% 
  mutate(LOCATION = case_when(
    country == "Australia" ~ "AUS",
    country == "Austria" ~ "AUT",
    country == "Belgium" ~ "BEL",
    country == "Brazil" ~ "BRA",
    country == "Bulgaria" ~ "BGR",
    country == "Canada" ~ "CAN",
    country == "Chile" ~ "CHL",
    country == "Colombia" ~ "COL",
    country == "Costa Rica" ~ "CRI",
    country == "Croatia" ~ "HRV",
    country == "Cyprus" ~ "CYP",
    country == "Czech Republic" ~ "CZE",
    country == "Denmark" ~ "DNK",
    country == "Estonia" ~ "EST",
    country == "Finland" ~ "FIN",
    country == "France" ~ "FRA",
    country == "Germany" ~ "DEU",
    country == "Greece" ~ "GRC",
    country == "Hungary" ~ "HUN",
    country == "Iceland" ~ "ISL",
    country == "Ireland" ~ "IRL",
    country == "Japan" ~ "JPN",
    country == "Latvia" ~ "LVA",
    country == "Lithuania" ~ "LTU",
    country == "Luxembourg" ~ "LUX",
    country == "Netherlands" ~ "NLD",
    country == "New Zealand" ~ "NZL",
    country == "Norway" ~ "NOR",
    country == "Poland" ~ "POL",
    country == "Portugal" ~ "PRT",
    country == "Romania" ~ "ROU",
    country == "Slovak Republic" ~ "SVK",
    country == "Slovenia" ~ "SVN",
    country == "Spain" ~ "ESP",
    country == "Sweden" ~ "SWE",
    country == "Switzerland" ~ "CHE",
    country == "Turkey" ~ "TUR",
    country == "UK (England)" ~ "GBR",
    country == "United States" ~ "USA",
    TRUE ~ country
  )) %>% select(-country)

### Data join annual data
data_A <- left_join(prices_A_real, interest_A_all) %>% 
  left_join(gdp_A) %>% 
  left_join(builds_df) %>% 
  group_by(LOCATION) %>% 
  mutate(lag_houseprice = lag(houseprice, order_by = LOCATION)) %>% 
  ungroup()
data_Q <- left_join(prices_Q_real, interest_Q_all) %>% 
  left_join(gdp_Q)

### Desired starting model
### prices ~ stock + interest + gdp + population
###
### This is price ~ interest + gdp...validates importance of having gdp

lm1 <- lm(log(houseprice) ~ interest + as.factor(year),
           data = data_A[!is.na(data_A$gdp),])
lm2 <- lm(log(houseprice) ~ interest + log(gdp) + as.factor(year),
          data = data_A)
anova(lm1, lm2)

### prices ~ stock + interest + gdp
### The sign on PC dwellings doesn't make sense

lm3 <- lm(log(houseprice) ~ interest  + log(gdp) + lag_houseprice,
       data = data_A[!is.na(data_A$gdp) & !is.na(data_A$PCDwellings),])
summary(lm3)
lm4 <- lm(log(houseprice) ~ interest  + log(gdp) + PCDwellings + lag_houseprice,
             data = data_A[!is.na(data_A$gdp) & !is.na(data_A$PCDwellings),])
summary(lm4)
anova(lm3, lm4)

### MRLE models (testing for formatting)

data_A_2010 <- data_A %>% dplyr::filter(year >= 2010)

# Fixed effect model on LOCATION and year
mle1 <- lmer(log(houseprice) ~ interest + log(gdp) + (1 | LOCATION) +
               (1 | year),
             data = data_A_2010)
summary(mle1)
coef(mle1)$year
coef(mle1)$LOCATION

# Random slope, fixed intercept

mix.hp <- lmer(log(houseprice) ~ log(gdp) + interest +
                 (0 + log(gdp) | LOCATION) + (1 | year),
               data = data_A_2010)
summary(mix.hp)
coef(mix.hp)$year
coef(mix.hp)$LOCATION

# Random effects

re.hp <- lmer(log(houseprice) ~ log(gdp) + interest +
                (log(gdp) | LOCATION) + (1 | year) + (1 | LOCATION),
              data = data_A_2010)
summary(re.hp)
coef(re.hp)$year
coef(re.hp)$LOCATION

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
### Housing Constructs https://www.oecd.org/housing/data/affordable-housing-database/housing-market.htm