m <- 2^(31) - 1
a <- 7^5
x <- numeric(length = 1e3)
x[1] <- 7
for(i in 2:1e3)
{
x[i] <- (a * x[i-1]) %% m
}
par(mfrow = c(1,2))
hist(x/m) # looks close to uniformly distributed
plot.ts(x/m) # look like it's jumping around too