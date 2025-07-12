#run proper library and data

library(tidyverse)
library(mlogit) 
library(dfidx)


setwd("C:\\Users\\kchar\\OneDrive\\Desktop\\mgt 100")
cust_dat <- read_csv ("smartphone_customer_data.csv")
n <- nrow(cust_dat)
phone_dat <- read_csv("phone_dat.csv", show_col_types = F)

# import datasets formatted for mlogit estimation
load("C:\\Users\\kchar\\OneDrive\\Desktop\\mgt 100\\mnl_dataset.RData")

# import hit rate and likelihood ratio index functions
load("C:\\Users\\kchar\\OneDrive\\Desktop\\mgt 100\\mnl_performance_functions.RData")

#Fit "out9" model brought 2 yrs ago question 1-5
cust_dat2yrs <- cust_dat |> filter(years_ago == 2)

out9_2yrs <- mlogit(choice ~ apple:segment + samsung:segment + 
                 price:segment + screen_size:segment + 
                 price:total_minutes:segment | 0, data=mdat2)

summary(out9_2yrs)
brand_hit_rate(mdat2, out9_2yrs)
product_hit_rate(mdat2, out9_2yrs)
ll_ratio(mdat2, out9_2yrs)

#Fit "out9" model brought 1 yr ago
cust_dat1yr <- cust_dat |> filter(years_ago == 1)

out9_1yr <- mlogit(choice ~ apple:segment + samsung:segment + 
                      price:segment + screen_size:segment + 
                      price:total_minutes:segment | 0, data=mdat1)

summary(out9_1yr)
brand_hit_rate(mdat1, out9_1yr)
product_hit_rate(mdat1, out9_1yr)
ll_ratio(mdat1, out9_1yr)