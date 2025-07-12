#run proper library and data

library(tidyverse)
library(mlogit) 
library(dfidx)


setwd("C:\\Users\\kchar\\OneDrive\\Desktop\\mgt 100")
cust_dat <- read_csv ("smartphone_customer_data.csv")
phone_dat <- read_csv("phone_dat.csv", show_col_types = F)

#Q1: Brand's Mrk Shares from 3 yrs ago to last
brand_shares_3y <- cust_dat |>
  filter(years_ago == 3) |>
  count(brand, name = "n") |>
  mutate(shr_3y = n / sum(n))


brand_shares_1y <- cust_dat |>
  filter(years_ago == 1) |>
  count(brand, name = "n") |>
  mutate(shr_1y = n / sum(n))

market_share_change <- left_join(brand_shares_1y, brand_shares_3y, by = "brand") |>
  mutate(change = round(shr_1y - shr_3y, 3)) |>
  select(brand, change)

print(market_share_change)

#load in mnl data set
load(mnl_dat)

#q2 multinomial logit model with Huawei as the baseline
out1 <- mlogit(choice ~ apple + samsung | 0, data = mdat1)
summary(out1)

# Extract and round the Apple brand coefficient
round(coef(out1)["apple"], 3)

#Q3: We want samsung coef:
out1 <- mlogit(choice ~ apple + samsung | 0, data = mdat1)
summary(out1)

# Extract and round coefficient
round(coef(out1)["samsung"], 3)

#Q4:covariates and price brand intrecepts. Huawei baseline
out2 <- mlogit(choice ~ apple + samsung + price | 0, data = mdat1)

summary(out2)
round(coef(out2),3)
#brand hit
brand_hit_rate(mdat1, out2)
#MCfadden R^2
ll_ratio(mdat1, out2)