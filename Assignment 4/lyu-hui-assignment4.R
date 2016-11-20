# LIS542 Assignment 4
# Hui Lyu

# 4.36
# (d)
pnorm(75, mean = 70, sd = 10/sqrt(40), lower.tail = FALSE)

# 4.40
# (a)
pnorm(10500, mean = 9000, sd = 1000, lower.tail = FALSE)

# (c)
pnorm(10500, mean = 9000, sd = 1000/sqrt(15), lower.tail = FALSE)

# (d)
x = seq(6000, 12000,length = 10000)
y = dnorm(x,9000,1000)
plot(x,y,col="black",ylim=c(0,0.0016),type='l')
lines(x,dnorm(x,9000,1000/sqrt(15)),col="blue",lty=2)
legend("topright",legend=paste(c("Population","Sampling (n = 15)")),lwd=1, lty=c(1,2), col=c("black","blue"),text.font = 1.5)

# 4.44
# (d)
library("tigerstats", lib.loc="C:/Program Files for operation/R-3.3.1/library")
pnormGC(bound = c(-1.436,1.436), region = "outside", mean = 0, sd = 1, graph = TRUE)
