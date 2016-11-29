# LIS542 Assignment 8
# Hui Lyu

### ISL 10.7.2
# (a)
dissimilarity.matrix = matrix(c(0, 0.3, 0.4, 0.7, 
                                0.3, 0, 0.5, 0.8,
                                0.4, 0.5, 0, 0.45,
                                0.7, 0.8, 0.45, 0), nrow = 4)
plot(hclust(as.dist(dissimilarity.matrix), method = "complete"))

# (b)
plot(hclust(as.dist(dissimilarity.matrix), method = "single"))

# (c)
# If we cut the dendrogram in two clusters result, we will have clusters (1,2) and (3,4).

# (d)
# If we cut the dendrogram in two clusters result, we will have clusters ((1,2),3) and (4).

# (e)
plot(hclust(as.dist(dissimilarity.matrix), method = "complete"), labels = c(2,1,4,3))

### A set of languages represented by spelling the numbers 1-5
lang1 <- c("one two three four five", 
          "uno dos tres cuatro cinco",
          "ein zwei drei vier funf",
          "un deux trois quatre cinq",
          "en to tre fire fem",
          "einn tveir thrir fjogur fimm",
          "een twee drie vier vijf",
          "egy ket harom negy ot",
          "yksi kaksi kolme nelja viisi")
lb1 <- c("ENG","SPA","GER","FRE","NOR","ICE","DUT","HUN","FIN")

lang2 <- c("one two three four five", 
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
          "amháin dhá trí ceathair cúig",
          "bat bi hiru lau bost",
          "satu dua tiga empat lima")
lb2 <- c("ENG","SPA","GER","FRE","NOR","ICE","DUT","HUN","FIN","POR","ITA","IRI","BAS","IND")

#Calculate Levenshtein distances
d1 <- stringdistmatrix(lang1, lang1, method = "lv")
d2 <- stringdistmatrix(lang2, lang2, method = "lv")


#Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it. 
cluster1 <- hclust(as.dist(d1),method="average")
cluster2 <- hclust(as.dist(d1),method="single")
cluster3 <- hclust(as.dist(d2),method="average")
cluster4 <- hclust(as.dist(d2),method="single")

#Plot Dendrogram
par(mfrow=c(2,2))
plot(cluster1,labels=lb1)
plot(cluster2,labels=lb1)
plot(cluster3,labels=lb2)
plot(cluster4,labels=lb2)
