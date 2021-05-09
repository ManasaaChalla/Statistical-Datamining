rm(list = ls())
#----------------------------------------------FIFA 18 Visualisation-------------------------------------------------
install.packages('fpc')
library(readr)
library(rpart)
library(data.table)
library(sqldf)
library(radarchart)
library(tidyr)
library(dplyr)
library(plyr)
library(dtplyr)
library(DT)
library(ggplot2)
library(modeest)
library(data.table)
library(recommenderlab)
library(stringr)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)
library(ggthemes)
library(caret)
library(cluster) 
library(e1071)
library(mclust) 
library(fpc) 
setwd('/Users/manasachalla/Desktop/spring\ 2021/STA/Project')
dataset <- read_csv("complete.csv")
complete = read_csv("CompleteDataset.csv")
dataset<-data.table(dataset)
complete<-data.table(complete)

dataset1<-as.data.frame(dataset)


dataset<-as.data.frame(dataset)
setDT(dataset)
setDT(complete)
dim(dataset)

class(dataset)
#Ages of football players
ggplot(dataset, aes(age, fill = age)) + geom_density(position = "stack") 

#Distribution of ratings of all the players
dataset %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Player's Overall ")

#Rating vs Age
aggregate <- dataset[age<41,.("overall"=mean(overall)),by=age][order(-age)]
ggplot(data = aggregate,aes(x=age,y=overall))+
  geom_line(color="red",size=2)+labs(title="Rating vs Age")+
  annotate("text", x = 30, y = max(aggregate$overall),color="blue", label = "Max", parse = TRUE, size = 3)

#10 Best Players
dataset1 %>% 
  arrange(-overall) %>% 
  top_n(15,wt = overall) %>% 
  select(name, age,overall,club,eur_value) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

#Players Height
dataset %>% 
  ggplot(aes(x = height_cm, fill = factor(height_cm))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Players Height")

#Players Weight
dataset %>% 
  ggplot(aes(x = weight_kg, fill = factor(weight_kg))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Players Weight")

#Players per Country
Nationality <- dataset[dataset$nationality!="",.N,by=.(nationality,`flag`)][order(-N)]

Nationality = Nationality[,c(1,3)] 
head(Nationality[order(Nationality$N, decreasing = TRUE),],10)

#Best Club
#Grouping players by club and appling average on players rating
Team<-arrange(dataset[, list(Avg=mean(overall)), by= "club" ], desc(Avg) )

head(Team, 10)

##################################### CLUSTERING ##########################################

#Number of clusters
cluster = dataset[,c(7,10,11,17:29,34:94)]
cluster[is.na(cluster)] = 0
wss <- (nrow(cluster[,c()])-1)*sum(apply(cluster[,1:ncol(cluster)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(cluster[,1:ncol(cluster)], centers=i)$withinss)

plot(1:
       10, wss, type="b", xlab="Number of Cluster",  ylab="Squares Summatory")

#Realization of clusters
set.seed(123)
km<-kmeans(cluster[,1:ncol(cluster)],4)
cluster$grupo<-km$cluster
dataset$grupo <-km$cluster
g1<- dataset[dataset$grupo==2,]
g2<- dataset[dataset$grupo==1,]
g3<- dataset[dataset$grupo==3,]
g4<- dataset[dataset$grupo==4,]

plotcluster(cluster[,1:(ncol(cluster)-1)],km$cluster)

#Players Overall

#GROUP1
g1 %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Player Rating Group 1")
#GROUP2
g2 %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Player Rating Group 2")
#GROUP3
g3 %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Player Rating Group 3")
#GROUP4
g4 %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Player Rating Group 4")


#Best Players!!!!!!!!!!!!!!!!!!!!!!!!!!!!
g11<- dataset1[dataset$grupo==1,]
g21<- dataset1[dataset$grupo==2,]
g31<- dataset1[dataset$grupo==3,]
g41<- dataset1[dataset$grupo==4,]


#GROUP1
g11 %>% 
  mutate(image = paste0('<img src="', `photo`, '"></img>')) %>% 
  #arrange(-overall) %>% 
  #top_n(15,wt = overall) %>% 
  select(name, age,overall,club,eur_value) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
#GROUP2
g21 %>% 
  mutate(image = paste0('<img src="', `photo`, '"></img>')) %>% 
  #arrange(-overall) %>% 
  #top_n(15,wt = overall) %>% 
  select(name, age,overall,club,eur_value) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
#GROUP3
g31 %>% 
  mutate(image = paste0('<img src="', `photo`, '"></img>')) %>% 
  #arrange(-overall) %>% 
  #top_n(15,wt = overall) %>% 
  select(name, age,overall,club,eur_value) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
#GROUP4
g41 %>% 
  mutate(image = paste0('<img src="', `photo`, '"></img>')) %>% 
  #arrange(-overall) %>% 
  #top_n(15,wt = overall) %>% 
  select(name, age,overall,club,eur_value) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))


#Players per Country

#GROUP1
Nationality <- g1[g1$nationality!="",.N,by=.(nationality,`flag`)][order(-N)] 

Nationality = Nationality[,c(1,3)]
head(Nationality[order(Nationality$N, decreasing = TRUE),],10)
#GROUP2
Nationality <- g2[g2$nationality!="",.N,by=.(nationality,`flag`)][order(-N)]

Nationality = Nationality[,c(1,3)]
head(Nationality[order(Nationality$N, decreasing = TRUE),],10)
#GROUP3
Nationality <- g3[g3$nationality!="",.N,by=.(nationality,`flag`)][order(-N)]

Nationality = Nationality[,c(1,3)]
head(Nationality[order(Nationality$N, decreasing = TRUE),],10)
#GROUP4
Nationality <- g4[g4$nationality!="",.N,by=.(nationality,`flag`)][order(-N)]

Nationality = Nationality[,c(1,3)]
head(Nationality[order(Nationality$N, decreasing = TRUE),],10)


#Players Values

#GROUP1
ggplot(g1, aes(eur_value, fill = eur_value)) +
  geom_density(position = "stack")
summary(g4$eur_value)
#GROUP2
ggplot(g2, aes(eur_value, fill = eur_value)) +
  geom_density(position = "stack")
summary(g4$eur_value)
#GROUP3
ggplot(g3, aes(eur_value, fill = eur_value)) +
  geom_density(position = "stack")
summary(g4$eur_value)
#GROUP4
ggplot(g4, aes(eur_value, fill = eur_value)) +
  geom_density(position = "stack")
summary(g4$eur_value)
#################################### Predicting Position ##################################

## Getting specs from all players
specs = dataset[,c(22:30,36:67)]
specs = data.frame(specs)

## Specs normalization
specsNorm = preProcess(specs,method = "scale")
specs = predict(specsNorm, specs)
## Adding ID and Overall to Specs
id_overall = dataset[,c(1,20)]
specs = cbind(specs,id_overall)

##Getting prefered_positions and ID
positions = complete[,c(53,64)]   
positions = data.frame(positions)

## Cleaning Preferred Positions

prefered_positions = positions[,'Preferred.Positions']
split = strsplit(prefered_positions, split=" ")
positionVector = 0
length = length(split)
for (position in 1:length) {
  positionVector[position] <- unlist(split[[position]][1]) 
}
positions[,'Preferred.Positions'] = positionVector

#Joining data
selected_data = join(specs, positions, by = NULL, type = "full", match = "first") 
selected_data = selected_data[complete.cases(selected_data),]

#Attack or Defense
position = selected_data[,'Preferred.Positions']
attack = c('ST','LW','RW','RM','CM','LM','CAM','CF')
defense = c('CDM','CB','LB','RB','RWB','LWB','GK')
#Replacing
position <- lapply(position, function(x) replace(x,x %in% attack, 1))
position <- lapply(position, function(x) replace(x,x %in% defense, 0))

positionVector = 0
for (i in 1:length(position)){
  positionVector[i] = position[[i]]
}

positionVector = as.numeric(positionVector)

#joining 
selected_data = cbind(selected_data,positionVector)

selected_data <- subset( selected_data, select = -ID )
selected_data[,'Preferred.Positions'] = as.factor(selected_data[,'Preferred.Positions'])

head(selected_data)

#data segmentation
percent <- 70/100

set.seed(3)

trainRowsNumber<-sample(1:nrow(selected_data),percent*nrow(selected_data))
train<-selected_data[trainRowsNumber,] 
test<-selected_data[-trainRowsNumber,] 

attack = selected_data[selected_data[,'positionVector'] ==1,-positionVector]
attack$Preferred.Positions = factor(attack$Preferred.Positions)
defence = selected_data[selected_data[,'positionVector'] ==0,-positionVector]
defence$Preferred.Positions = factor(defence$Preferred.Positions)

trainRowsNumber<-sample(1:nrow(attack),percent*nrow(attack))
trainA<-attack[trainRowsNumber,] 
testA<-attack[-trainRowsNumber,] 

trainRowsNumber<-sample(1:nrow(defence),percent*nrow(defence))
trainD<-defence[trainRowsNumber,] 
testD<-defence[-trainRowsNumber,] 

head(train)

#Logistic Regression

#ATTACK

model<-glm(positionVector~., data = train)
pred<-predict(model,newdata = test)
pred = round(pred)
length(pred)
cfmLR = confusionMatrix(as.factor(pred),as.factor(test$positionVector))
cfmLR
test$positionVector

#Support Vector Machine
model <- svm(Preferred.Positions~. ,data=trainA,kernel = "linear")

prediction <- predict(model,testA)
#prediction = round(prediction, digits = 0)
cfmSVM<-confusionMatrix(prediction,testA[,'Preferred.Positions'])
cfmSVM


#DEFENCE

modelD <- svm(Preferred.Positions~. ,data=trainD,kernel = "linear")
prediction <- predict(modelD,testD)
#prediction = round(prediction, digits = 0)
cfmSVM<-confusionMatrix(prediction,testD[,'Preferred.Positions'])
cfmSVM

