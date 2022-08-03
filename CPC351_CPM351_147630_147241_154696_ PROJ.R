#Project

#DATA PRE-PROCESSING
#Import the ggplot2, dplyr, scatterplot3d, klaR, caret, 
#lattice, rpart, rpart.plot
#data.tree and caTools libraries
library(ggplot2)
library(dplyr)
library(scatterplot3d)
library(klaR)
library(caret)
library(lattice)
library(rpart)
library(rpart.plot)
library(data.tree)
library(caTools)

# Set the working directory for the subsequent import and export operations
setwd("C:/Users/Rafe Ng/Downloads")

# Read the CSV files using read.csv() function
road_accident <- read.csv("Road_Traffic_Accidents.csv")

#Displaying the internal structure of road_accident
str(road_accident)

#Summary of road_accident
summary(road_accident)

#Convert the attributes of Road.Surface, Lighting.Conditions,
#Weather.Conditions and Casualty.Severity to factor
road_accident$Road.Surface <- as.factor(road_accident$Road.Surface)
road_accident$Lighting.Conditions <- as.factor(road_accident$Lighting.Conditions)
road_accident$Weather.Conditions <- as.factor(road_accident$Weather.Conditions)
road_accident$Casualty.Severity <- as.factor(road_accident$Casualty.Severity)

#Displaying the internal structure of road_accident
str(road_accident)

#Summary of road_accident
summary(road_accident)

#Select specific columns
road_accident_2 <- road_accident[, c("Road.Surface", "Lighting.Conditions", "Weather.Conditions", "Casualty.Severity")]

#Summary of road_accident_2
summary(road_accident_2)

#missing value in road_accident_2$Road.Surface
sum(is.na(road_accident_2$Road.Surface))
#missing value in road_accident_2$Lighting.Conditions
sum(is.na(road_accident_2$Lighting.Conditions))
#missing value in road_accident_2$Weather.Conditions
sum(is.na(road_accident_2$Weather.Conditions))
#missing value in road_accident_2$Casualty.Severity
sum(is.na(road_accident_2$Casualty.Severity))

#Remove the outlier from Weather.Conditions
road_accident_3 <- subset(road_accident_2, Weather.Conditions != 8 & Weather.Conditions != 9 & 
                            Lighting.Conditions != 3 & Lighting.Conditions != 7)

#Summary of road_accident_3
summary(road_accident_3)

#Convert the attributes of Road.Surface, Lighting.Conditions,
#Weather.Conditions and Casualty.Severity to numeric
road_accident_3$Road.Surface <- as.numeric(road_accident_3$Road.Surface)
road_accident_3$Lighting.Conditions <- as.numeric(road_accident_3$Lighting.Conditions)
road_accident_3$Weather.Conditions <- as.numeric(road_accident_3$Weather.Conditions)
road_accident_3$Casualty.Severity <- as.numeric(road_accident_3$Casualty.Severity)

#Scatterplot 3D
scatterplot3d(jitter(road_accident_3$Road.Surface), jitter(road_accident_3$Lighting.Conditions), jitter(road_accident_3$Weather.Conditions),
              pch = 20, xlab = "Road Surface", ylab = "Lighting Conditions", zlab = "Weather Conditions")

#Create a scatterplot matrix of the variables of the road accidents dataset
splom(~road_accident_3[c(1:4)], groups = NULL, data = road_accident_3, axis.line.tck = 0,
      axis.text.alpha = 0)

#Plot a histogram for Road.Surface
road_hist <- ggplot(road_accident_3, aes(Road.Surface)) + geom_histogram(binwidth = 1) + 
  labs(x = "Road Surfaces", y = "Frequency", title = "Histogram of Road Surfaces") + 
  theme_bw() + ylim(0,1600) + theme(
    plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "blue", size = 10, face = "italic"),
    axis.title.y = element_text(color = "blue", size = 10, face = "italic")) + 
  stat_count(aes(label = ..count..), geom = "text", vjust = -0.5)
road_hist

#Plot a histogram for Lighting.Conditions
lighting_hist <- ggplot(road_accident_3, aes(Lighting.Conditions)) + geom_histogram(binwidth = 1) + 
  labs(x = "Lighting Conditions", y = "Frequency", title = "Histogram of Lighting Conditions") + 
  theme_bw() + ylim(0,1600) + theme(
    plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "blue", size = 10, face = "italic"),
    axis.title.y = element_text(color = "blue", size = 10, face = "italic")) + 
  stat_count(aes(label = ..count..), geom = "text", vjust = -0.5)
lighting_hist

#Plot a histogram for Weather.Conditions
weather_hist <- ggplot(road_accident_3, aes(Weather.Conditions)) + geom_histogram(binwidth = 1) + 
  labs(x = "Weather Conditions", y = "Frequency", title = "Histogram of Weather Conditions") + 
  theme_bw() + ylim(0,1700) + theme(
    plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "blue", size = 10, face = "italic"),
    axis.title.y = element_text(color = "blue", size = 10, face = "italic")) + 
  stat_count(aes(label = ..count..), geom = "text", vjust = -0.5)
weather_hist

#Plot a histogram for Casualty.Severity
casualty_hist <- ggplot(road_accident_3, aes(Casualty.Severity)) + geom_histogram(binwidth = 1) + 
  labs(x = "Casualty Severity", y = "Frequency", title = "Histogram of Casualty Severity") + 
  theme_bw() + ylim(0,1600) + theme(
    plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "blue", size = 10, face = "italic"),
    axis.title.y = element_text(color = "blue", size = 10, face = "italic")) + 
  stat_count(aes(label = ..count..), geom = "text", vjust = -0.5)
casualty_hist

#Convert the attributes of Road.Surface, Lighting.Conditions,
#Weather.Conditions and Casualty.Severity to factor
road_accident_3$Road.Surface <- as.factor(road_accident_3$Road.Surface)
road_accident_3$Lighting.Conditions <- as.factor(road_accident_3$Lighting.Conditions)
road_accident_3$Weather.Conditions <- as.factor(road_accident_3$Weather.Conditions)
road_accident_3$Casualty.Severity <- as.factor(road_accident_3$Casualty.Severity)

#FIRST ALGORITHM: KMODES CLUSTERING
#Set range from 1 to 10
range <- 1:10

#Create a vector called accuracy_score
accuracy_score <- c()

for (k in range){
  
  #Set seed
  set.seed(k)
  
  #KModes CLustering
  k_modes = kmodes(road_accident_3, 3, iter.max = 10, weighted = FALSE, fast = TRUE)
  k_modes
  
  #Create a new table
  kmodes_table = cbind(road_accident_3, k_modes$cluster)
  kmodes_table
  
  #Rename the columns in the table
  colnames(kmodes_table)[1] <- "Road Surface"
  colnames(kmodes_table)[2] <- "Lighting Conditions"
  colnames(kmodes_table)[3] <- "Weather Conditions"
  colnames(kmodes_table)[4] <- "Casualty Severity"
  colnames(kmodes_table)[5] <- "Predicted Casualty Severity"
  
  #Convert the attributes of Road Surface, Lighting Conditions, Weather Conditions
  #Predicted Casualty Severity and Casualty.Severity to factor
  kmodes_table$`Road Surface` <- as.factor(kmodes_table$`Road Surface` )
  kmodes_table$`Lighting Conditions` <- as.factor(kmodes_table$`Lighting Conditions` )
  kmodes_table$`Weather Conditions` <- as.factor(kmodes_table$`Weather Conditions` )
  kmodes_table$`Casualty Severity` <- as.factor(kmodes_table$`Casualty Severity`)
  kmodes_table$`Predicted Casualty Severity` <- as.factor(kmodes_table$`Predicted Casualty Severity`)
  
  #Create a table for the prediction and actual values
  xtab <- table(kmodes_table$`Predicted Casualty Severity` , kmodes_table$`Casualty Severity`)
  xtab 
  
  #Apply Confusion Matrix
  con_mat <- confusionMatrix(xtab)
  
  #Extract accuracy score
  score <- con_mat$overall['Accuracy']
  
  #Append score to accuracy_score
  accuracy_score <- append(accuracy_score, score)
}

#Show the accuracy scores obtained from KModes CLustering
accuracy_score


#SECOND ALGORITHM: DECISION TREE CLASSIFIER
#Create a vector called accuracy_score_2
accuracy_score_2 <- c()

for (k in range){
  
  #Set seed
  set.seed(k)
  
  id_2 <- sample(2, nrow(road_accident_3), prob = c(0.8, 0.2), replace = T)
  train_set_2 <- road_accident_3[id_2 == 1,]
  test_set_2 <- road_accident_3[id_2 == 2,]
  
  #Decision Tree Classifier
  dec_tree <- rpart(Casualty.Severity ~ Road.Surface + Lighting.Conditions + Weather.Conditions, data = train_set_2, control=rpart.control(minsplit=1, minbucket=1, cp=0.001) , parms=list(split='gini'))
  dec_tree
  
  pred <- predict(dec_tree, test_set_2, type = 'class')
  con_mat_2 <- confusionMatrix(table(pred, test_set_2$Casualty.Severity))
  con_mat_2
  
  score <- con_mat_2$overall['Accuracy']
  accuracy_score_2 <- append(accuracy_score_2, score)
  
}

#Show the accuracy scores obtained from Decision Tree Classifier
accuracy_score_2

#Plot the decision tree 
rpart.plot(dec_tree, box.palette = "blue")


#WELCH'S T-TEST
#Null Hypothesis: KModes predicts better than Decision Tree
#Alternative Hypothesis: KModes does not predict better than Decision Tree
#p-value = 0.05
#p-value obtained from Welch's t-test is less than 0.05
#Null Hypothesis is rejected
t.test(accuracy_score, accuracy_score_2, var.equal = FALSE) 




