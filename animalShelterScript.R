# Henry McGee 
# 10/26/2019 
# Animal Shelter 

#tricks 
#############
# class("MonthYear") check parameter type
#write.csv(dataset, "filename.csv", row.names = FALSE)   make csv file in documents for windows
#############

#read the csv
#############
animalShelter <- read.csv("C://Users//Henry//Desktop//School//Dataanalysis4300//AnimalShelter//Austin_Animal_Center_Outcomes.csv", fill=TRUE, header =TRUE, sep=",")

#View(animalShelter)
names(animalShelter)
summary(animalShelter)
#############

#dogs only contains dogs
dogs <- subset(animalShelter, AnimalType == "Dog")
animalShelter <- subset(animalShelter, AnimalType == "Dog")

animalShelter <- animalShelter[!duplicated(animalShelter[1]),]


names(dogs)
summary(dogs)

#adoptedDogs contains dogs who were adopted
adoptedDogs <- subset(dogs, OutcomeType == "Adoption")

names(adoptedDogs)
summary(adoptedDogs)

#eliminate subtypes so that only adopted dogs remain
adoptedDogs <- subset(adoptedDogs, OutcomeSubtype == "")

names(adoptedDogs)
summary(adoptedDogs)

#for arrival date
arrivalDate <- adoptedDogs[,3]
summary(arrivalDate)

#this section limits the year or date range of animals in the sample
#######################
#convert dates and time to date object
adoptedDogs[,3] <- as.Date(arrivalDate, format= "%m/%d/%Y %H:%M")
#limits to 2016 from new years to next new years
adoptedDogs <- subset(adoptedDogs, adoptedDogs[,3] > "2013/12/31 24:00" & adoptedDogs[,3] < "2019/12/31/24:00")
#######################

summary(adoptedDogs)
#working on, limit to 100 in a breed and exclude those with less than 100
#######################
library(plyr)
breedList <- count(adoptedDogs, "Breed")
summary(breedList)
names(breedList)
summary(breedList[,2])
#gives only breeds with 100 or more dogs per a breed
breedList <- subset(breedList, breedList[,2] >= 100)
summary(breedList)
names(breedList)


write.csv(breedList, "breedList.csv", row.names = TRUE)
#creating csv of Breed-freq, AgeUponOutcome-freq, SexUponOutcome-freq, Color-freq
##########
#first find top 24 breeds
#so order by max frequency, currently using breeds with > 200 
breedListTop24 <- head(breedList[order(breedList$freq, decreasing= T),], 24)
summary(breedListTop24)
Breed <- breedListTop24$Breed
Freq <- breedListTop24$freq

write.csv(breedListTop24, "Top24Breeds.csv", row.names = TRUE)


#find frequency of arrival dates
#as.Date(adoptedDogs[,3], format= "%m/%d/%Y")
#breedListArrivalDate <- count(adoptedDogs, ArrivalDate)
#summary(breedListArrivalDate)
#write.csv(breedListArrivalDate, "BreedListArrivalDate.csv", row.names = TRUE)

#histogram
####################
summary(Breed)
#plot(Breed,Freq)
summary(breedListTop24)
library("tidyverse")
breedListTop20_plus <- breedListTop24 %>% rowid_to_column(var = "percentAdopted")
summary(breedListTop20_plus)
#gives a basic histogram
breedFrequency_plot <- ggplot(data = breedListTop24, 
                              mapping = aes(x = reorder(Breed,Freq), y = Freq)) + geom_histogram( color="black", fill="black", stat = "identity", binwidth = 100) + 
                              ggtitle("Number of adoptions by breed between 2013 and 2019") + theme_bw() +
                              theme(plot.title = element_text(size=10)) +
                              xlab("Breeds") + ylab("Number of Adoptions")
  
#flips the graph
breedFrequency_plot + coord_flip()
####################

#read in the csv containing only the top 24 breeds (principle is each animal)
####################
AnimalShelterTop24 <- read.csv("C://Users//Henry//Desktop//School//Dataanalysis4300//AnimalShelter//Austin_Animal_Center_OutcomesTop24.csv", fill=TRUE, header =TRUE, sep=",")
#convert age to a date
#AnimalShelterTop24$Age <- as.Date(AnimalShelterTop24$Age, format= "%y years %m months %d days")
Age <- AnimalShelterTop24$Age
AgeFreqTop24 <- count(AnimalShelterTop24, Age)
Age <- AgeFreqTop24$Age
Freq <- AgeFreqTop24$n
write.csv(AgeFreqTop24, "AgeFreqTop24.csv", row.names = TRUE)

write.csv(AnimalShelterTop24, "AnimalShelterTop24$Age.csv", row.names = TRUE)
Age <- subset(AgeFreqTop24, AgeFreqTop24$Age > 5 )
summary(Age)
#Age <- AgeFreqTop24$Age
#Freq <- AgeFreqTop24$n
#library(ggplot2)
#ageFrequency_plot <- ggplot(data = AnimalShelterTop24, 
#                              mapping = aes(x = reorder(Age,Freq), y = Freq)) + geom_histogram( color="black", fill="black", stat = "identity", binwidth = 100) + 
#  ggtitle("Number of adoptions by Age between 2013 and 2019") + theme_bw() +
#  xlab("Age") + ylab("Number of Adoptions")
#ageFrequency_plot + coord_flip()
#arrivalDate <- animalShelterTop24$ArrivalDate
#dateOfBirth <- animalShelterTop24$DateofBirth
#arrivalDate <- as.Date(arrivalDate, "%m/%d/%y") 
#dateOfBirth <- as.Date(dateOfBirth, "%m/%d/%y")

#ageUponAdoption <- difftime(arrivalDate,dateOfBirth)
#write.csv(ageUponAdoption, "ageUponAdoption", row.names = TRUE)
####################



#ideas
#############
#use percentage of total adoptions 
#############

#graphs normally
#breedFrequency_plot +
 # geom_point()

##########








#retrieve 
#histogram
#################
library(ggplot2)
plot(adoptedDogs$AgeUponOutcome)

#create copy of data frame
adoptedDogsByDateofBirth <- data.frame(adoptedDogs)
#check that they have different memory addresses
tracemem(adoptedDogsByDateofBirth)==tracemem(adoptedDogs)
#order by date of birth
adoptedDogsByDateofBirth[order(as.Date(adoptedDogsByDateofBirth$DateofBirth, format= "%m/%d/%Y")),]
write.csv(adoptedDogsByDateofBirth, "dateBirth.csv", row.names = TRUE)
summary(adoptedDogsByDateofBirth)
plot(adoptedDogsByDateofBirth$AgeUponOutcome)
#class(adoptedDogs$DateofBirth)
#tmp <- aggregate(as.numeric(adoptedDogs$Animal_ID) ~ as.numeric(adoptedDogs$DateofBirth), FUN = min)
#summary(tmp)
#plot(tmp$DateofBirth)
#################


#now add frequency's to adoptedDogs
#Wrong code below
############################
#breedList <- subset(breedList, breedList[,2] >= 100)
#summary(breedList) 
#table(breedList)
#summary(breedList[,2] >= 100)

#adoptedDogs[(adoptedDogs$Breed %in% breedList$Breed),]

summary(adoptedDogs)


#df$quantity <- quantity
#adoptedDogs$breedList <- 
#for(Breed in adoptedDogs[,10]) {
#  for(freq in breedList) {
#    if(Breed = breedList[,1]) {
#      adoptedDogs$breedList[,2] <- breedList[,2]
#    }
#  }
#}

  
#adoptedDogs <- subset(adoptedDogs, breedList[,2] >= 100)
#summary(adoptedDogs)
write.csv(adoptedDogs, "4.csv", row.names = TRUE)
##########################

animalShelter <- read.csv("C://Users//Henry//Desktop//School//Dataanalysis4300//AnimalShelter//Austin_Animal_Center_OutcomesTop24.csv", fill=TRUE, header =TRUE, sep=",")
breedList <- count(animalShelter, "Breed")
summary(breedList)
write.csv(breedList, "breedList.csv", row.names = TRUE)
