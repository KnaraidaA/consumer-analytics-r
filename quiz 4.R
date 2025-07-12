
library(tidyverse)
library(ggrepel)   # <-- helps label points on our plots
library(ggfortify) # <-- auto-plots PCA results
library(corrplot)  # <-- makes correlations prettier

setwd("C:\\Users\\kchar\\OneDrive\\Desktop\\mgt 100")
cust_dat <- read_csv ("smartphone_customer_data.csv")

#1 subset data into males, and behavior types, minutues of usage
sub <- cust_dat |> 
  filter(gender == "male") |>
  select (gaming, chat, maps, video, social, reading)
#correlations
cor_matrix <- cor(sub)
corrplot::corrplot(cor_matrix, method="number", type="lower")

#2  run PCA on the 6 usage variables
cust_dat |> select(gaming:reading) |> prcomp() -> out2
prcomp(cust_dat |> select(gaming:reading), scale = TRUE)
summary(out2)

#3 proportion of variance third PC 
summary(out2)

#4
autoplot(out2, 
         color="dodgerblue4", alpha=0.2,               
         loadings = TRUE, loadings.colour = "firebrick",
         loadings.label = TRUE, loadings.label.size = 5) 