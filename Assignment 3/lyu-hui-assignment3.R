# LIS542 Assignment 3
# Hui Lyu

# 3.2
# (a)
install.packages("tigerstats")
library("tigerstats", lib.loc="C:/Program Files for operation/R-3.3.1/library")
pnormGC(-1.13, region="above", graph=TRUE)

# (b)
pnormGC(0.18, region="below", graph=TRUE)

# (c)
pnormGC(8, region="above", graph=TRUE)

# (d)
pnormGC(bound = c(-0.5,0.5), region="between", graph=TRUE)

# 3.10
# (a)
pnormGC(48, region="below", mean = 55, sd = 6, graph = TRUE)

# (b)
pnormGC(bound = c(60,65), region="between", mean = 55, sd = 6, graph = TRUE)
pnormGC(60, region = "below", mean = 55, sd = 6)
pnormGC(65, region = "above", mean = 55, sd = 6)

# (c)
qnorm(0.9, mean = 55, sd = 6)

# (d)
pnormGC(54, region = "below", mean = 55, sd = 6)

# 3.12
# (a)
pnormGC(80, region = "below", mean = 72.6, sd = 4.78)

# (b)
pnormGC(c(60,80), region = "between", mean = 72.6, sd = 4.78)

# (c)
qnorm(0.95, mean = 72.6, sd = 4.78)

# (d)
pnormGC(70, region = "above", mean = 72.6, sd = 4.78)

# 3.28
# (c)
pbinom(105, size = 120, prob = 0.9)

# 3.30
pbinomGC(1500, region = "above", size = 15000, prob = 0.09, graph = TRUE)
