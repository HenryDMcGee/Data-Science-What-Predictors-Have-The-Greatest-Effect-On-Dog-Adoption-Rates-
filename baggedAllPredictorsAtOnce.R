library(ipred)
library(MASS)
library(tree)
library(plyr)

animalShelter <- read.csv("C://Users//Henry//Desktop//School//Dataanalysis4300//AnimalShelter//Final_Dogs_Data.csv", fill=TRUE, header =TRUE, sep=",")
animalShelter <- subset(animalShelter, SexUponOutcome != "Unknown")
sex <- count(animalShelter, "SexUponOutcome")
table(sex)

baggedsample <- bagging(OutcomeType ~., animalShelter, nbagg=500, coob=TRUE, control = list (minbucket=5))
print(baggedsample, 10)
summary(baggedsample)
importance(baggedsample)
