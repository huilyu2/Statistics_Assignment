
sample<-c(38,40,35,35,40,35,37,28,30,38,36,35,70,34,42,37,46,40,39,42,41,42,42,43,37,26,39,43,34,34,38,42,45,38,42,42,41,36,43,45,42,45,40)


# sum(sample)/43
mean(sample)

sd(sample)
mean(sample) + 2* sd(sample)/sqrt(43)
mean(sample) - 2* sd(sample)/sqrt(43)

ma <- function(arr){
  res = arr
  for(i in 1:length(arr)){
    res[i] = mean(arr[1:i])
  }
  res
}
ma(sample)
plot(ma(sample), type="l", col="red")
