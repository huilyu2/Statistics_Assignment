# LIS452 -- 9/13/2016
# by Vetle Torvik
# tested with R 3.3.1
#
# Random numbers, distributions, simulation
#########################

#Generating a random number(s)
###########################
#R has built-in support for generating random numbers from
#  many different distributions

#Uniform distribution -- the simplest distn
#  can be continuous or discrete

#Continuous
x = runif(1,0,1)  #one random number between 0 and 1

#Discrete
d1 = ceiling(runif(1,0,1)*6)  #a random number in {1,2,3,4,5,6}
d2 = ceiling(runif(1,0,1)*6)
s = d1+d2  #the sum of two random numbers in {1,2,3,4,5,6}

#Generating many random numbers
#  creates a list of numbers
#  and algebraic operations are performed element-wise
#simulating 1000 rolls of pairs of dice

d1 = ceiling(runif(1000,0,1)*6)  # a thousand die rolls
d2 = ceiling(runif(1000,0,1)*6)  # a thousand die rolls
s = d1 + d2 #

#a simple visual
hist(s,12)

#Q: What is the probability of the sum being > 10?
##################################
#  a) calculate by hand
3/36
#  b) estimate by simulation
#     (think about number of simulations affects accuracy of estimate too)
sum(s>10)/sum(s>1)

#Solve Monty Hall's Problem
############################
# Conditional probabilties and Bayes' Thm at work
# a) by hand
# P(switch to wim) = 2/3
# P(not switch to win) = 1/3
# b) by simulation
doors = c("A","B","C")
xdata = c()
for (i in 1:1000)
{
  prize = sample(doors)[1]
  pick = sample(doors)[1]
  open = sample(doors[which(doors != pick & doors != prize)])[1]
  switchyes = doors[which(doors != pick & doors!=open)]
  if(pick==prize)(xdata=c(xdata,"noswitchwin"))
  if(switchyes==prize)(xdata=c(xdata,"switchwin"))
}
length(which(xdata=="switchwin"))
length(which(xdata=="noswitchwin"))


#Gaussian, aka Normal or bell-shape, is continuous 
############
x = rnorm(1000, 0, 1)
hist(x,20)
plot(function(x) dnorm(x), -4, 4)

#Binomial is discrete
x = rbinom(1000, 100, 0.1)
hist(x,100)
n <- 1000
k <- seq(0, n, by = 20)
plot (k, dbinom(k, n, pi/10),type="l")
#plot (function(x) dbinom(x,100,0.1),1,10)

#Problem: is there a relation between binomial and Gaussian when sample size is large?
#################

#Problem: Prove Central Limit Theorem by simulation
##############






