#load in tidy verse and data
library (tidyverse)
cust_dat <- read_csv ("smartphone_customer_data (1).csv")
#Q1: heavy users min >1500 combo of brand/size
cust_dat |> 
  filter (total_minutes>1500) |>
  count(brand, size_cat)
#female customers age 19-20 most popular brands
cust_dat |> 
  filter ((age == 19|age == 20) & gender == "female") |>
  count(brand)
# need histogram, yrs purchased: 2 
cust_dat |> 
  filter(years_ago == 2)
ggplot(cust_dat, aes(x = chat)) +
  geom_histogram(binwidth = 20, fill = "dodgerblue4", color = "black", alpha = 0.7) +
  ggtitle("Histogram of chat minutes") +
  xlab("Chat Minutes") +
  ylab("Frequency") +
  theme_classic()
#wanting to test different binwidth
cust_dat |> 
  filter(years_ago == 2)
ggplot(cust_dat, aes(x = chat)) +
  geom_histogram(binwidth = 30, fill = "dodgerblue4", color = "black", alpha = 0.7) +
  ggtitle("Histogram of chat minutes") +
  xlab("Chat Minutes") +
  ylab("Frequency") +
  theme_classic()
#limit data to "A2" phone, hand size 1, 2, and 3 yrs ago
cust_dat |> 
  filter(brand == "apple", phone_id == "A2", years_ago == 1 | years_ago == 2 | years_ago == 3)
ggplot(cust_dat, aes(x= factor(years_ago), y = handsize)) +                            
  geom_boxplot(outlier.color="red", color="white", fill="skyblue") +   
  ggtitle("customer hand size by years ago") +                           
  xlab("years_ago") +                                                       
  ylab("hand size") +                                                                                                        
  theme_dark()  
#wanting scatterplot with total_minutes and handsize
ggplot(cust_dat) +
  geom_point(aes(x=total_minutes, y=handsize))
ggplot(cust_dat, aes(total_minutes, handsize)) +       
  geom_point(color="dodgerblue4", alpha=0.5) + 
  ggtitle("Knar's Scatterplot:Hand size vs. Height") +           
  xlab("total_minutes(weekly)") +                            
  ylab("Hand Size(inches)") +                          
  theme_minimal() 
