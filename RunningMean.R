# Create the sample
s1 <- c(38,40,35,35,40,35,37)
s2 <- c(28,30,38,36,35,70)
s3 <- c(34,42,37,46,40,39,42)
s4 <- c(41,42,42,43,37,26,39)
s5 <- c(43,34,34,38,42,45,38,42)
s6 <- c(42,41,36,43,45,42,45,40)
sample <- c(s1,s2,s3,s4,s5,s6)

# Create an empty vector to store running mean for each sample size.
running_mean <- vector(mode="numeric", length=length(sample))

# Loop and calculate mean of first i observations in each iteration.
# Sample size increases by in each iteration.
for (i in 1:length(sample)) {
  running_mean[i] <- mean(sample[1:i])
}

#Plot
plot(1:length(sample),running_mean,xlab="sample size", ylab="Running mean", main= "Running mean vs sample size")
lines(1:length(sample),running_mean)