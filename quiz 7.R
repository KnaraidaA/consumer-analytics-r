#run proper library and data

library(tidyverse)
library(mlogit) 
library(gridExtra)


setwd("C:\\Users\\kchar\\OneDrive\\Desktop\\mgt 100")
cust_dat <- read_csv ("smartphone_customer_data.csv")
n <- nrow(cust_dat)
phone_dat <- read_csv("phone_dat.csv", show_col_types = F)

# import datasets formatted for mlogit estimation
load("C:\\Users\\kchar\\OneDrive\\Desktop\\mgt 100\\mnl_dataset.RData")

#Q1: Market share for Large Huawei last yr
cust_dat1yr <- cust_dat |> filter(years_ago == 1)
market_shares <- last_yr |> count(phone_id) |> mutate(shr = round(n / sum(n), 3))
market_shares |> filter(phone_id == "H2")

#Q2 fitted market share for large Huawei phones last yr
# fit mnl data
colnames(smat) <- c("A1", "A2", "S1", "S2", "H1", "H2")

mdat1 <- mdat1 |> mutate(phone_a2 = phone_id == "A2",
                         phone_s1 = phone_id == "S1",
                         phone_s2 = phone_id == "S2",
                         phone_h1 = phone_id == "H1",
                         phone_h2 = phone_id == "H2")
out <- mlogit(choice ~ apple:segment + 
                samsung:segment + 
                price:segment +
                screen_size:segment + 
                price:total_minutes:segment | 0, data=mdat1)
#predictions 
probs <- predict(out, newdata=mdat1)
shr <- colMeans(probs)
#match phone_id and mdat1 
names(shr) <- levels(mdat1$phone_id)
h2_share <- round(shr["H2"], 3)
print(h2_share)

#Q3 own price elasticty for H2

# get a vector of price changes to use
pvec <- seq(from=-200, to=200, by=10)

# and construct empty matrix to store shares at each price
smat <- matrix(NA, nrow=length(pvec), ncol=6)
colnames(smat) <- c("A1", "A2", "S1", "S2", "H1", "H2")

# loop over the price change values
for(i in 1:length(pvec)) {
  
  # print progress
  cat("Working on", i, "of", length(pvec), "\n")
  
  # get the price change amount
  p <- pvec[i]
  
  # change prices for S1 phones
  tempdat <- as_tibble(sub1) |> mutate(price = ifelse(phone_id == "H2", price + p, price))
  
  # make market share predictions with the temporarily-changed S1 prices
  preds <- predict(out, newdata=tempdat)
  
  # calculate and store market shares
  smat[i,] <- colMeans(preds)
}

# gather our prices and estimated shares into a dataframe
relcol <- which(colnames(smat) == "H2")
h2dat <- tibble(scenario=1:length(pvec), price=pvec+799, share=smat[,relcol])

# plot H2's inverse demand curve
ggplot(h2dat, aes(x=share, y=price)) +
  geom_point() + 
  geom_line() + 
  labs(x="Share", y="Price") +
  theme_bw()
 
M <- 10

# Let's scale our demand curve to be in the price-quantity space instead of the price-share space

h2dat <- h2dat |> mutate(quantity = share*M)

ggplot(h2dat, aes(x=quantity, y=price)) + 
  geom_point() + 
  geom_line() + 
  labs(x="Quantity", y="Price") +
  theme_bw()
#Marginal Cost
mc1 <- 600

# Calculate own-price elasticity at +/- $10 from actual price of $799

p1 <- h2dat |> filter(price==799-10) |> pull(price)
q1 <- h2dat |> slice(20) |> pull(quantity)

p2 <- h2dat |> slice(22) |> pull(price)
q2 <- h2dat |> slice(22) |> pull(quantity)

elasticity <- ((q2-q1)/q1) / ((p2-p1)/p1)
elasticity

#Q4  Rule of Thumb H2:

# Approximate optimal price using the elasticity rule

mc1 * 1 / (1 - 1/abs(elasticity))

#Q5 profit maximizing for H2
h2dat <- h2dat |> mutate(revenue = price * quantity)

p1 <- ggplot(h2dat) + geom_point(aes(x=quantity, y=price)) + theme_bw()
p2 <- ggplot(h2dat) + geom_point(aes(x=quantity, y=revenue)) + theme_bw()

grid.arrange(p2, p1, ncol=1)


h2dat <- h2dat |> mutate (cost = 600 * quantity)
p3 <- ggplot(h2dat) + geom_point(aes(x=quantity, y=cost) + theme_bw()
grid.arrange(p3, p2, p1, ncol=1)                                
h2dat |> filter(price == 799)
h2dat <- h2dat |> mutate(profit = revenue - cost)
h2dat |> filter(profit == max(profit))

