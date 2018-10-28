data.df <- read.csv("train.csv") #Loading the dataset
print(data.df)
data.df.n_rows <- nrow(data.df) #Counting the number of rows
data.df.n_cols <- ncol(data.df) #Counting the number of columns
print(data.df.n_cols)
print(data.df.n_rows)
data.df.subset <- subset(data.df, select=c("Age", "Embarked", "Fare", "PassengerId")) #Extracting columns from data.df
print(data.df.subset)
data.df.subset$Age[is.na(data.df.subset$Age)] <- median(data.df.subset$Age, na.rm=TRUE) #Replacing missing values with median
print(data.df.subset)
data.df.subset[data.df.subset$Embarked==""] <- NA #Replacing the blanks with NA 
emb <- data.df.subset$Embarked
embarked <- function(emb) {
  un <- unique(emb)
  un[which.max(tabulate(match(emb, un)))]
  } #Mode calculation
output <- embarked(emb)  
data.df.subset$Embarked[is.na(data.df.subset$Embarked)] <- output #Replacing missing values with mode
print(data.df.subset)
hist(data.df.subset$Age) #Histogram
a=mean(data.df.subset$Age) #Mean of Age
b=sd(data.df.subset$Age) #Standard Deviation of Age
plot(data.df.subset$Age, data.df.subset$Fare, xlab='Age', ylab='Fare') #Plotting an Age vs Fare graph
anomalous_indices <- subset(data.df.subset, Age > a+2*b | Age < a-2*b, select=c("PassengerId")) #Extracting anomalous values of passenger id corresponding to age
print(anomalous_indices)
data.df.subset.v2 <- subset(data.df.subset, Age >= 25 & Age <= 80, select=c("Age", "Fare", "Embarked")) #Extract the certain age ranger
print(data.df.subset.v2)
data.df.subset.v2$Fare_Rescaled <- rescale(data.df.subset.v2$Fare, to=c(0, 100)) #Rescaling the Fare Column
print(data.df.subset.v2)

