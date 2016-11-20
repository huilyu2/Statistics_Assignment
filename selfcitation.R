#Solo-author papers with known gender, ethnicity, and country of affiliation, and references
#
# ---trying to replicate findinges in
#    http://www.eigenfactor.org/gender/self-citation/SelfCitation.pdf
###############################################
#Read data

####
cite <- read.delim('http://abel.lis.illinois.edu/download/pmcsoloselfcitesuniq.tsv', header=T)
summary(cite)
dim(cite)
head(cite)

#filter by gender
ageF = cite$ageN[cite$Genni == 'F']
ageM = cite$ageN[cite$Genni == 'M']
pF <- hist(log10(ageF))
pM <- hist(log10(ageM))
plot( pM, col=rgb(0,0,1,1/4), xlim=c(0,4))
plot( pF, col=rgb(1,0,0,1/4), xlim=c(0,4), add=T)  

#fit linear model
ageN.lm <- lm(log10(ageN) ~ year + I(year^2) + Genni + Genni*year,data=cite)
summary(ageN.lm)
qqnorm(resid(ageN.lm))
plot(cite$year,log10(cite$ageN))
points(cite$year,fitted(ageN.lm),col='red')


#Clear workspace
##############
rm(list=ls())

#Same set, just with referenced paper IDs too
####################################
cite <- read.delim('C:/Documents/GSLIS/542 Data, Statistics and Information/pmcsoloselfcitesA.tsv', header=T)

#filter data by year: 2002-2008
cite <- cite[cite$year > 2001 & cite$year < 2009,]

summary(cite)
#attach(cite)
dim(cite)
head(cite)

#Can we use simple linear regression?
#################################
xtabs(~ selfcite + Genni, data = cite)
selfcite.lm <- lm(selfcite ~ Genni,data = cite)
summary(selfcite.lm)
qqnorm(resid(selfcite.lm))

#A basic logistic regression model
selfcite.glm1 <- glm(selfcite ~ Genni, data = cite, family = "binomial")
summary(selfcite.glm1)

#Accounting for other variables
selfcite.glm2 <- glm(selfcite ~ Genni + year + log10(ageN), data = cite, family = "binomial")
summary(selfcite.glm2)


#More variables

#no or one prior paper: more likely selfcitation?
ageN1 <- as.numeric(cite$ageN == 1)
ageN2 <- as.numeric(cite$ageN == 2)

#citing into the future: more likely selfcitation?
citedYear0 <- as.numeric(cite$citedYear > cite$year)

#citing more recent papers: more likely selfcitation?
yearDiff <- (cite$year - cite$citedYear)*(1-citedYear0)
yearDiff0 <- as.numeric(yearDiff == 0)


#fit model
selfcite.glm3 <- glm(selfcite ~ Genni + year + log10(ageN) + I(log10(ageN)^2) + ageN1 + ageN2 + citedYear0 , data = cite, family = "binomial")
summary(selfcite.glm3)
#ethnicity: are some more likely to self-cite?
Ethnea1 <- factor(as.character(lapply(strsplit(as.character(cite$Ethnea), split="-"), "[", 1)))

#set reference = ENGLISH
cite <- within(cite, Ethnea1 <- relevel(Ethnea1, ref = 'ENGLISH'))

#set reference = USA
cite <- within(cite, MapAffil <- relevel(MapAffil, ref = 'USA'))

#USA affiliation: more likely to selfcite?
countryUSA <- factor(cite$MapAffil == 'USA')
countryJAPAN <- factor(cite$MapAffil == 'JAPAN')
sameCountry <- as.character(cite$MapAffil) == as.character(cite$citedMapAffil)

#journal effects?
sameJournal <- as.character(cite$journal) == as.character(cite$citedJournal)
jPNAS <- factor(cite$journal == 'Proc Natl Acad Sci U S A')
jBMJ <- factor(cite$journal == 'BMJ')
jClinInvest <- factor(cite$journal == 'J Clin Invest')

#get Gender of first author on paper referenced
citedGenni <- factor(as.character(lapply(strsplit(as.character(cite$citedGenni), split="|"), "[", 1)))
sameGenni <- as.character(cite$Genni) == as.character(cite$citedGenni)
sameGenniM <- sameGenni & cite$Genni == 'M'

#get Ethnea of first author on paper referenced
#citedEthnea <- factor(as.character(lapply(strsplit(as.character(cite$citedEthnea), split="|"), "[", 1)))
#citedEthnea <- factor(as.character(lapply(strsplit(as.character(citedEthnea), split="-"), "[", 1)))
#sameEthnea <- as.character(Ethnea1) == as.character(citedEthnea)


#normalize year so coefficient don't become tiny
cite$year <- (cite$year-2000)/10


#fit model
selfcite.glm4 <- glm(selfcite ~ Genni + year + log10(ageN) + I(log10(ageN)^2) + ageN1 + ageN2 + citedYear0 + sameJournal + sameGenni + sameCountry, data = cite, family = "binomial")
summary(selfcite.glm4)

#another one
selfcite.glm <- glm(selfcite ~ Genni + Ethnea1 + sameGenni + year + log10(ageN) + I(log10(ageN)^2) + ageN1 + ageN2 + citedYear0 + sameJournal + sameGenni + sameCountry + countryUSA + jPNAS + jBMJ + jClinInvest, data = cite, family = "binomial")
summary(selfcite.glm)
