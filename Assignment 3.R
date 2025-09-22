# 1. Load dataset
titanic <- read.csv("G:/N and B/Neha/MSc Data Analytics/FDS/Titanic_train.csv")
print(titanic)

# 2. Head and tail
head(titanic)
tail(titanic)

# 3. Dimensions
dim(titanic)

# 4. Structure and summary
str(titanic)
summary(titanic)

# 5. Class of all columns
sapply(titanic, class)

# 6. Convert Survived and Sex to factor
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)

# 7. Convert Pclass, Embarked, SibSp to factor
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$SibSp <- as.factor(titanic$SibSp)

# 8. Check NA count per column
colSums(is.na(titanic))

# 9. Boxplot for Age
boxplot(titanic$Age, main="Boxplot of Age", col="lightblue")

# 10. Impute median to Age
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm=TRUE)

# 11. ggplot2 barplot Pclass
library(ggplot2)
ggplot(titanic, aes(x=Pclass, fill=Pclass)) + geom_bar() + ggtitle("Univariate Analysis - Pclass")

# 12. ggplot2 barplot Survived
ggplot(titanic, aes(x=Survived, fill=Survived)) +
  geom_bar() +
  ggtitle("Univariate Analysis - Survived") +
  ylab("No. of Passengers")

# 13. Histogram of Age
ggplot(titanic, aes(x=Age)) +
  geom_histogram(binwidth=5, fill="purple", color="black") +
  ggtitle("Univariate Analysis - Age") +
  xlab("Age")

# 14. Barplot Survived vs Sex
ggplot(titanic, aes(x=Survived, fill=Sex)) +
  geom_bar(position="dodge") +
  geom_text(stat="count", aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Survived") + ylab("Count") +
  ggtitle("Survived vs Sex")

# 15. Barplot Survived vs Pclass
ggplot(titanic, aes(x=Survived, fill=Pclass)) +
  geom_bar(position="dodge") +
  geom_text(stat="count", aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Survived") + ylab("Count") +
  ggtitle("Survived vs Pclass")

# 16. Mean and median of Age
mean(titanic$Age)
median(titanic$Age)

# 17. IQR and Range for Age
IQR(titanic$Age)
range(titanic$Age)

# 18. Separate survived and not survived
titanic_survived <- subset(titanic, Survived==1)
titanic_notsurvived <- subset(titanic, Survived==0)

# 19. Select Age and Survived, first 10 rows
library(dplyr)
titanic %>% select(Age, Survived) %>% head(10)

# 20. Drop Name, Fare, Ticket, PassengerId, Cabin
titanic <- titanic %>% select(-Name, -Fare, -Ticket, -PassengerId, -Cabin)

# 21. Group by Survived and Sex
titanic %>% group_by(Survived, Sex) %>% summarise(count=n())

# 22. Group by Sex and Survived
titanic %>% group_by(Sex, Survived) %>% summarise(count=n())

# 23. Group by Survived and summarise mean Age
titanic %>% group_by(Survived) %>% summarise(mean_age=mean(Age, na.rm=TRUE))

# 24. Create Age_Status column
titanic <- titanic %>% mutate(Age_Status = ifelse(Age<18, "Minor", "Major"))

# 25. Filter only male and get Embarked count
titanic %>% filter(Sex=="male") %>% group_by(Embarked) %>% summarise(count=n())
