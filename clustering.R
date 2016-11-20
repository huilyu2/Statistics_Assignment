#Clustering and Unsupervised Learning
#   (as opposed to classification and supervised learning)
#
# Key concepts: 
#   distance or dissimilarity metrics
#      -- Think numerical, string, text, documents, images, music
#   curse of dimensionality
#
# Vetle Torvik 11/2016
# 

############################
# 1:Hierarchical Clustering
##############################
# Retracing the evolution of languages
# text data

install.packages("stringdist")
library(stringdist)

#A set of languages represented by spelling the numbers 1-5
#  an oversimplication
lang <- c("one two three four five", 
  "uno dos tres cuatro cinco",
  "ein zwei drei vier funf",
  "un deux trois quatre cinq",
  "en to tre fire fem",
  "einn tveir thrir fjogur fimm",
  "een twee drie vier vijf",
  "egy ket harom negy ot",
  "yksi kaksi kolme nelja viisi",
  "um dois três quatro cinco",
  "uno due tre quattro cinque",
  "??? ??? ??? ??? ???",
  "??? ??? ??? ??? ???",
  "??? ??? ??? ??? ???")
lbl <- c("ENG","SPA","GER","FRE","NOR","ICE","DUT","HUN","FIN","POR","ITA","CHN","JAN","CHNt")

#Try adding more languages

#Calculate Levenshtein distances
d <- stringdistmatrix(lang, lang,method = "lv")

#Try modifying distance measure
#?stringdistmatrix

#Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it. 
cl <- hclust(as.dist(d),method="average")

#Try modifying methods
#?hclust


#Plot Dendrogram
plot(cl,labels=lbl)


#Iris data, and example of numerical data
############
iris
#?iris

names(iris)
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

##############
#2 : K-means, a centroid-based 
###############
#K-means
set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster

#Confusion matrix
table(irisCluster$cluster, iris$Species)

#######################################
#3: PCA -- Principal Component Analysis
#  for creating a new coordinate system so that first axis explains the most variance
#######################################
pc <- prcomp(iris[,1:4])
pc

#Proportion of variance explained
summary(pc)
plot(pc,type="l")

#The new coordinate system
biplot(pc)



