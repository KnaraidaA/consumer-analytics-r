cust_dat |> 
              filter(brand=="huawei", size_cat=="s", years_ago==3) |> 
              summarize(avg_social_min = mean(social))
#group brand huawei, small size, and purchased 3 yrs ago, and want to know mean of avg min on social
cust_dat |> 
            group_by(brand, size_cat, years_ago) |> 
            summarize(num_consumers = n()) |> 
            arrange(desc(num_consumers))
#organizes by brand, size, yrs ago purchased . want to know consumer count and descending order
cust_dat |> 
  filter(brand=="samsung", is.na(discount)) |> 
  summarize(my_count_var = n())
#is. na helps with counting number of ppl buying no discount
cust_dat |> 
  filter(brand=="samsung", discount == phone_id) |> 
  summarize(my_count_var = n())
#count if discount
cust_dat |> 
  filter(age >=20) |> 
  summarize(avg_game_min = mean(gaming))
#avg gaming mins , customers over 20 only
cust_dat |> 
  filter(gender== "male") |> 
  summarize(avg_social = mean(social))
#male avg min on social media
cust_dat |> 
  filter(gender== "female") |> 
  summarize(avg_social = mean(social))
#female avg min on social media 