#IMPORT THE DATA SET 

data <- read.table(file.choose(),h=T)

attach(data)
names(data)
str(data)

#CONVERT THE GROUP VARIABLE IN FACTOR 


data$Group <- as.factor(data$Group)
data$Group = factor(data$Group,labels = c("Wall lizard", "Viviparous lizard", "Snake-eyed lizard"))

str(data)

############## CHECK FOR THE ASSUMPTION ################

# 1. ALL SAMPLES ARE INDEPENDENT, 
# AND MORE THAN 2 CATEGORICAL GROUP

summary(data$Group)

#2. DEPENDENT VARIABLE IS CONTINUOUS

#3. CHECK FOR OUTLIERS

Group1 <- subset(data, Group == "Wall lizard")
Group2 <- subset(data, Group == "Viviparous lizard")
Group3 <- subset(data, Group == "Snake-eyed lizard")

boxplot(Group1$Length)
boxplot(Group2$Length)
boxplot(Group3$Length)

#4. NORMAL DISTRIBUTIONS OF EACH GROUP 

qqnorm(Group1$Length)
qqline(Group1$Length)

qqnorm(Group2$Length)
qqline(Group2$Length)

qqnorm(Group3$Length)
qqline(Group3$Length)

#5. Homogeneneity of variances

bartlett.test(Length ~ Group, data = data)

############# ANOVA TEST #######################

model <-  aov(Length ~ Group, data = data)


summary(model)

#POST-HOC TEST - WHICH OF THE GROUP HAVE DIFF MEANS

TukeyHSD(aov(model))

# DATA VISUALISATIOIN

library("ggplot2")

ggplot(data, aes(x = Group, y = Length)) +
  geom_boxplot() 
  
