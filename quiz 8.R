library(tidyverse)

# Specify parameters
M <- 0.5   # market size ( millions)
p <- 0.03 # coefficient of innovation
q <- 0.25   # coefficient of imitation

# Specify number of time periods to simulate
J <- 12

# Allocate memory to store simulated values
A <- R <- N <- vector(mode="numeric", length=J)

# Input period-1 values
A[1] <- N[1] 
R[1] <- M - A[1]
N[1] <- p*M

# Simulate data for correct periods
for(t in 2:J) {
  A[t] <- N[t-1] + A[t-1]
  R[t] <- M - A[t]
  N[t] <- (p + q*A[t]/M)*R[t]
}

# Combine the data into a data.frame
simdat <- data.frame(t=1:J, N, A, R)
View(simdat)


# plot sales to see if peaks first 12 months 
ggplot(simdat) + geom_point(aes(t,N), color="black") + theme_bw()

