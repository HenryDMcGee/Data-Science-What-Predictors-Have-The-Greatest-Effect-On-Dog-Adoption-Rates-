require(data.table)
library(rpart)
require(ggplot2)

set.seed(456)

animalShelter <- read.csv("C://Users//Henry//Desktop//School//Dataanalysis4300//AnimalShelter//Final_Dogs_Data.csv", fill=TRUE, header =TRUE, sep=",")



animalShelter <- read.csv("C://Users//Henry//Desktop//School//Dataanalysis4300//AnimalShelter//agesLessThan=1.csv", fill=TRUE, header =TRUE, sep=",")









##Reading data
Age <- animalShelter$Age
OutcomeType <- animalShelter$OutcomeType

Age <- as.numeric(Age)
OutcomeType <- as.integer(OutcomeType)

#animalShelter <- subset(animalShelter, animalShelter$Age <= 1)
#write.csv(animalShelter, "agesLessThan=1.csv", row.names = TRUE)

summary(Age)
animalShelter$Age <- Age
animalShelter$OutcomeType <- OutcomeType

bagging_data=data.table(animalShelter)
class(airquality$Ozone)
class(OutcomeType)

ggplot(bagging_data,aes(Age,OutcomeType))+geom_point()+ggtitle("Age versus Adoption Rate")

data_test=na.omit(bagging_data[,.(OutcomeType,Age)])
##Training data
train_index=sample.int(nrow(data_test),size=round(nrow(data_test)*0.8),replace = F)
data_test[train_index,train:=TRUE][-train_index,train:=FALSE]

##Model without bagging
no_bag_model=rpart(OutcomeType~Age,data_test[train_index],control=rpart.control(minsplit=6))
result_no_bag=predict(no_bag_model, bagging_data) #here

##Training of the bagged model
n_model=100
bagged_models=list()
for (i in 1:n_model)
{
  new_sample=sample(train_index,size=length(train_index),replace=T)
  bagged_models=c(bagged_models,list(rpart(OutcomeType~Age,data_test[new_sample],control=rpart.control(minsplit=6))))
}

##Getting estimate from the bagged model
bagged_result=NULL
i=0
#ADDED
storeResults <- vector(,100)
for (from_bag_model in bagged_models)
{
  if (is.null(bagged_result)) {
    bagged_result=predict(from_bag_model, bagging_data)
    #store each value in a vector
    storeResults[i] <- sqrt(bagged_result) 
  }  
  else {
    bagged_result=(i*bagged_result+predict(from_bag_model, bagging_data))/(i+1)
    #store each value in a vector
    storeResults[i] <- sqrt(bagged_result) 
  }
  i=i+1
}


summary(storeResults)
table(storeResults)
ageImportance <- mean(storeResults)
summary(ageImportance)




##########################


##Plot
require(ggplot2)
gg <- ggplot(data_test,aes(Age,OutcomeType))+geom_point(aes(color=train))
gg
for (tree_model in bagged_models[1:100])
{
  prediction <- predict(tree_model, bagging_data)
  data_plot <- data.table(Age=bagging_data$Age,OutcomeType=prediction)
  gg <- gg+geom_line(data=data_plot[order(Age)],aes(x=Age,y=OutcomeType),alpha=0.2)
}
data_bagged <- data.table(Age=bagging_data$Age,OutcomeType=bagged_result)
gg <- gg+geom_line(data=data_bagged[order(Age)],aes(x=Age,y=OutcomeType),color='green')

data_no_bag <- data.table(Age=bagging_data$Age,OutcomeType=result_no_bag)
gg <- gg+geom_line(data=data_no_bag[order(Age)],aes(x=Age,y=OutcomeType),color='red')
gg




#################################
#getting variance of breed

set.seed(456)

animalShelter <- read.csv("C://Users//Henry//Desktop//School//Dataanalysis4300//AnimalShelter//Final_Dogs_Data.csv", fill=TRUE, header =TRUE, sep=",")



##Reading data
Breed <- animalShelter$Breed
OutcomeType <- animalShelter$OutcomeType

OutcomeType <- as.integer(OutcomeType)

animalShelter$Breed <- Breed
animalShelter$OutcomeType <- OutcomeType

bagging_data=data.table(animalShelter)
class(airquality$Ozone)
class(OutcomeType)

ggplot(bagging_data,aes(Breed,OutcomeType))+geom_point()+ggtitle("Breed versus Adoption Rate")

data_test=na.omit(bagging_data[,.(OutcomeType,Breed)])
##Training data
train_index=sample.int(nrow(data_test),size=round(nrow(data_test)*0.8),replace = F)
data_test[train_index,train:=TRUE][-train_index,train:=FALSE]

##Model without bagging
no_bag_model=rpart(OutcomeType~Breed,data_test[train_index],control=rpart.control(minsplit=6))
result_no_bag=predict(no_bag_model,bagging_data)

##Training of the bagged model
n_model=100
bagged_models=list()
for (i in 1:n_model)
{
  new_sample=sample(train_index,size=length(train_index),replace=T)
  bagged_models=c(bagged_models,list(rpart(OutcomeType~Breed,data_test[new_sample],control=rpart.control(minsplit=6))))
}

##Getting estimate from the bagged model
bagged_result=NULL
i=0
#ADDED
storeResultsBreed <- vector(,100)
for (from_bag_model in bagged_models)
{
  if (is.null(bagged_result)) {
    bagged_result=predict(from_bag_model,bagging_data)
    #store each value in a vector
    storeResultsBreed[i] <- sqrt(bagged_result) 
  }  
  else {
    bagged_result=(i*bagged_result+predict(from_bag_model,bagging_data))/(i+1)
    #store each value in a vector
    storeResultsBreed[i] <- sqrt(bagged_result) 
  }
  i=i+1
}


summary(storeResultsBreed)
table(storeResultsBreed)
BreedImportance <- mean(storeResultsBreed)
summary(BreedImportance)



#################################
##Plot
require(ggplot2)
gg <- ggplot(data_test,aes(Breed,OutcomeType))+geom_point(aes(color=train))
gg
for (tree_model in bagged_models[1:100])
{
  prediction <- predict(tree_model,bagging_data)
  data_plot <- data.table(Breed=bagging_data$Breed,OutcomeType=prediction)
  gg <- gg+geom_line(data=data_plot[order(Breed)],aes(x=Breed,y=OutcomeType),alpha=0.2)
}
data_bagged <- data.table(Breed=bagging_data$Breed,OutcomeType=bagged_result)
gg <- gg+geom_line(data=data_bagged[order(Breed)],aes(x=Breed,y=OutcomeType),color='green')

data_no_bag <- data.table(Breed=bagging_data$Breed,OutcomeType=result_no_bag)
gg <- gg+geom_line(data=data_no_bag[order(Breed)],aes(x=Breed,y=OutcomeType),color='red')
gg

###############################

#################################
#Color

set.seed(456)

animalShelter <- read.csv("C://Users//Henry//Desktop//School//Dataanalysis4300//AnimalShelter//Final_Dogs_Data.csv", fill=TRUE, header =TRUE, sep=",")


##Reading data
Color <- animalShelter$Color
OutcomeType <- animalShelter$OutcomeType

OutcomeType <- as.integer(OutcomeType)

animalShelter$Color <- Color
animalShelter$OutcomeType <- OutcomeType

bagging_data=data.table(animalShelter)
class(airquality$Ozone)
class(OutcomeType)

ggplot(bagging_data,aes(Color,OutcomeType))+geom_point()+ggtitle("Color versus Adoption Rate")

data_test=na.omit(bagging_data[,.(OutcomeType,Color)])
##Training data
train_index=sample.int(nrow(data_test),size=round(nrow(data_test)*0.8),replace = F)
data_test[train_index,train:=TRUE][-train_index,train:=FALSE]

##Model without bagging
no_bag_model=rpart(OutcomeType~Color,data_test[train_index],control=rpart.control(minsplit=6))
result_no_bag=predict(no_bag_model,bagging_data)

##Training of the bagged model
n_model=100
bagged_models=list()
for (i in 1:n_model)
{
  new_sample=sample(train_index,size=length(train_index),replace=T)
  bagged_models=c(bagged_models,list(rpart(OutcomeType~Color,data_test[new_sample],control=rpart.control(minsplit=6))))
}

##Getting estimate from the bagged model
bagged_result=NULL
i=0
#ADDED
storeResultsColor <- vector(,100)
for (from_bag_model in bagged_models)
{
  if (is.null(bagged_result)) {
    bagged_result=predict(from_bag_model,bagging_data)
    #store each value in a vector
    storeResultsColor[i] <- sqrt(bagged_result) 
  }  
  else {
    bagged_result=(i*bagged_result+predict(from_bag_model,bagging_data))/(i+1)
    #store each value in a vector
    storeResultsColor[i] <- sqrt(bagged_result) 
  }
  i=i+1
}


summary(storeResultsColor)
table(storeResultsColor)
colorImportance <- mean(storeResultsColor)
summary(colorImportance)

#################################
#Sex

set.seed(456)

animalShelter <- read.csv("C://Users//Henry//Desktop//School//Dataanalysis4300//AnimalShelter//Final_Dogs_Data.csv", fill=TRUE, header =TRUE, sep=",")
animalShelter <- subset(animalShelter, SexUponOutcome != "Unknown")

##Reading data
SexUponOutcome <- animalShelter$SexUponOutcome
OutcomeType <- animalShelter$OutcomeType

OutcomeType <- as.integer(OutcomeType)

animalShelter$SexUponOutcome <- SexUponOutcome

animalShelter$OutcomeType <- OutcomeType


bagging_data=data.table(animalShelter)
class(airquality$Ozone)
class(OutcomeType)

ggplot(bagging_data,aes(SexUponOutcome,OutcomeType))+geom_point()+ggtitle("Sex versus Adoption Rate")

data_test=na.omit(bagging_data[,.(OutcomeType,SexUponOutcome)])
##Training data
train_index=sample.int(nrow(data_test),size=round(nrow(data_test)*0.8),replace = F)
data_test[train_index,train:=TRUE][-train_index,train:=FALSE]

##Model without bagging
no_bag_model=rpart(OutcomeType~SexUponOutcome,data_test[train_index],control=rpart.control(minsplit=6))
result_no_bag=predict(no_bag_model,bagging_data)

##Training of the bagged model
n_model=100
bagged_models=list()
for (i in 1:n_model)
{
  new_sample=sample(train_index,size=length(train_index),replace=T)
  bagged_models=c(bagged_models,list(rpart(OutcomeType~SexUponOutcome,data_test[new_sample],control=rpart.control(minsplit=6))))
}

##Getting estimate from the bagged model
bagged_result=NULL
i=0
#ADDED
storeResultsSex <- vector(,100)
for (from_bag_model in bagged_models)
{
  if (is.null(bagged_result)) {
    bagged_result=predict(from_bag_model,bagging_data)
    #store each value in a vector
    storeResultsSex[i] <- sqrt(bagged_result) 
  }  
  else {
    bagged_result=(i*bagged_result+predict(from_bag_model,bagging_data))/(i+1)
    #store each value in a vector
    storeResultsSex[i] <- sqrt(bagged_result) 
  }
  i=i+1
}


summary(storeResultsSex)
table(storeResultsSex)
sexImportance <- mean(storeResultsSex)

summary(sexImportance)

#########################


##Plot
require(ggplot2)
gg <- ggplot(data_test,aes(SexUponOutcome,OutcomeType))+geom_point(aes(color=train))
gg
for (tree_model in bagged_models[1:100])
{
  prediction <- predict(tree_model,bagging_data)
  data_plot <- data.table(SexUponOutcome=bagging_data$SexUponOutcome,OutcomeType=prediction)
  gg <- gg+geom_line(data=data_plot[order(SexUponOutcome)],aes(x=SexUponOutcome,y=OutcomeType),alpha=0.2)
}
data_bagged <- data.table(SexUponOutcome=bagging_data$SexUponOUtcome,OutcomeType=bagged_result)
gg <- gg+geom_line(data=data_bagged[order(SexUponOutcome)],aes(x=SexUponOutcome,y=OutcomeType),color='green')

data_no_bag <- data.table(SexUponOutcome=bagging_data$SexUponOutcome,OutcomeType=result_no_bag)
gg <- gg+geom_line(data=data_no_bag[order(SexUponOutcome)],aes(x=SexUponOutcome,y=OutcomeType),color='red')
gg

###############################
summary(sexImportance)
summary(colorImportance)

summary(BreedImportance)

summary(ageImportance)
#results <- data.frame(sexImportance, colorImportance, BreedImportance, ageImportance)
results <- read.csv("C://Users//Henry//Desktop//School//Dataanalysis4300//AnimalShelter//varianceSheet.csv", fill=TRUE, header =TRUE, sep=",")
summary(results)


#figure out how to plot
breedFrequency_plot <- ggplot(data = results, 
                              mapping = aes(x = reorder(results[,1],results[,2]), y = results[,2])) + geom_histogram( color="blue", fill="darkblue", stat = "identity", binwidth = 100) + 
 # ggtitle("Number of adoptions by breed between 2013 and 2019") + theme_bw() +
  theme(plot.title = element_text(size=10)) +
  xlab("") + ylab("Predictor Importance in Relation to Adoption")

#flips the graph
breedFrequency_plot + coord_flip()

