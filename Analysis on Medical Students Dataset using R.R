## Reading medical_students data set
medical_students<-read.csv(file.choose(),header=T)
View(medical_students)
## It shows no of rows and columns
dim(medical_students)
## displays first six rows
head(medical_students)
## displays last  six rows
tail(medical_students)
## shows column names
names(medical_students)
## displays the type of a data frame
class(medical_students)
##shows no of columns
ncol(medical_students)
## shows no of  rows
nrow(medical_students)
## displays basic statistics of a data set
summary(medical_students)
## checking  the existent null values
sum(is.na(medical_students))
## checking the duplicate values
sum(duplicated(medical_students))
# Transforming the Boolean values into 0's and 1's.
medical_students$Gender[medical_students$Gender == 'Female']<- 0
medical_students$Gender[medical_students$Gender == 'Male']<- 1
print(medical_students)
## Add " y" string to the age column
medical_students$Age <- paste(medical_students$Age, "y")
# View the modified data frame
print(medical_students)
# Remove "years" from the Age column
medical_students$Age <- gsub(" y", "", medical_students$Age)
print(medical_students)
## It shows mean values for every column
medical_students%>%
  summarise_if(is.numeric,mean,na.rm=TRUE)
## It shows the structure of the data frame  
str(medical_students)
# replacing null values in age column with mean taken from summary()
medical_students$Age <- replace(medical_students$Age, is.na(medical_students$Age), 26)
print(medical_students)
# Replace unknown values with a specific value (e.g., "Unknown")
medical_students$Gender <- replace(medical_students$Gender, is.na(medical_students$Gender), "1")
print(medical_students)
## replacing unknown values in the gender with the male(1)
medical_students$Gender[medical_students$Gender == "Unknown"] <- 1
print(medical_students)
## shows maximum value in gender column
max(df$Gender)
##
# replacing null values in Height column with mean.
medical_students$Height <- replace(medical_students$Height, is.na(medical_students$Height), 174.9495)
print(medical_students)


#replacing null values in Weight column with mean.
medical_students$Weight <- replace(medical_students$Weight, is.na(medical_students$Weight), 69.96527)
print(df$Weight)

# Create a frequency table
freq_table <- table(medical_students$Blood.Type)
print(freq_table)
##
m <- names(which.max(table(medical_students$Blood.Type)))
m



mode <- function(medical_students){
  distinct_values<- unique(medical_students)
  distinct_tabulate <- tabulate(match(medical_students,distinct_values))
  distinct_values[which.max(distinct_tabulate)]
}
##
mode(medical_students$Blood.Type)

###
medical_students$Blood.Type[is.na(medical_students$Blood.Type)]<-mode(medical_students$Blood.Type)
sum(is.na(medical_students$Blood.Type))


##
# replacing null values in Height column with mean.
medical_students$BMI <- replace(medical_students$BMI, is.na(medical_students$BMI),23.33071 )
sum(is.na(medical_students$BMI))


##
# replacing null values in Height column with mean.
medical_students$Temperature <- replace(medical_students$Temperature, is.na(medical_students$Temperature),98.60056)
sum(is.na(medical_students$Temperature))

## replacing null values in Height column with mean.
medical_students$Heart.Rate <- replace(medical_students$Heart.Rate, is.na(medical_students$Heart.Rate),79)
sum(is.na(medical_students$Heart.Rate))


## replacing null values in Height column with mean.
medical_students$Blood.Pressure <- replace(medical_students$Blood.Pressure, is.na(medical_students$Blood.Pressure), 114.5475)
sum(is.na(medical_students$Heart.Rate))

## replacing null values in Height column with mean.
medical_students$Cholesterol <- replace(medical_students$Cholesterol, is.na(medical_students$Cholesterol), 184.5033)
sum(is.na(medical_students$Cholesterol))

# Transforming the Boolean values into 0's and 1's.
medical_students$Diabetes[medical_students$Diabetes == 'Yes']<- 0
medical_students$Diabetes[medical_students$Diabetes == 'No']<- 1
print(medical_students)

mode(medical_students$Diabetes)

medical_students$Diabetes[is.na(medical_students$Diabetes)]<-mode(medical_students$Diabetes)
sum(is.na(medical_students$Diabetes))


# Transforming the Boolean values into 0's and 1's.
medical_students$Smoking[medical_students$Smoking == 'Yes']<- 0
medical_students$Smoking[medical_students$Smoking == 'No']<- 1
print(medical_students)

mode(medical_students$Smoking)

medical_students$Smoking[is.na(medical_students$Smoking)]<-mode(medical_students$Smoking)
sum(is.na(medical_students$Smoking))

sum(is.na(medical_students))



###outliers
# Boxplot to visually identify outliers
boxplot(medical_students$Cholesterol)
boxplot(medical_students$Temperature)
boxplot(medical_students$Heart.Rate)
boxplot(medical_students$BMI)
boxplot(medical_students$Blood.Pressure)




medical_students$Height <- as.integer(medical_students$Height)
medical_students$Weight <- as.integer(medical_students$Weight)
medical_students$BMI <- as.integer(medical_students$BMI)
medical_students$Temperature <- as.integer(medical_students$Temperature)
medical_students$Blood.Pressure <- as.integer(medical_students$Blood.Pressure)
medical_students$Cholesterol <- as.integer(medical_students$Cholesterol)
 


# Calculate mean and standard deviation
mean_value <- mean(medical_students$Weight)
sd_value <- sd(medical_students$Weight)

# Standardize the column
medical_students$standardized_Weight <- (medical_students$Weight - mean_value) / sd_value

# Calculate minimum and maximum values
min_value <- min(medical_students$Heart.Rate)
max_value <- max(medical_students$Heart.Rate)

# Normalize the column
medical_students$normalized_Heart.Rate <- (medical_students$Heart.Rate - min_value) / (max_value - min_value)

##
z_score<-scale(medical_students$Temperature)
outliers<-which(abs(z_score)>3)
outliers
removeoutliers<-which(abs(z_score)<3)
removeoutliers
boxplot(removeoutliers)

##z-score normalization
medical_students2<-scale(medical_students$Heart.Rate)
print(medical_students2)

#mutate (to add a new column)
medical_students<-mutate(medical_students,M=Height/Weight+Cholesterol)

# Reorder Columns
medical_students3 <- medical_students[, c("Student.ID","BMI", "Blood.Pressure", "Blood.Type","Age","Gender","Height","Weight","Temperature","Heart.Rate","Cholesterol","Diabetes","Smoking","standardized_Weight","normalized_Heart.Rate","M")]

# Print the Result
print(medical_students3)


## splitting strings in a column
fav_food=c("Butter naan paneer")
medical_students<-mutate(medical_students,fav_food)

#using strsplit() method
medical_students4<- strsplit(medical_students$fav_food," ")
print(medical_students4)

##anonymous  functions like lambda
lambda_function<- function(arguments)
{
  
}

add_numbers<- function(x,y){
  return(x+y)
}
##adding two column values
result<-add_numbers(medical_students$Height,medical_students$Weight)
print(result)


multiply<- function(x,y)x*y
#multiplying two columns
result<-multiply(medical_students$Cholesterol,medical_students$Diabetes)
print(result)
  
##
# Convert "Age" to numeric (if it's not already)
medical_students$Age <- as.numeric(medical_students$Age)

##1. Histogram of Age:
ggplot(medical_students, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()
unique(medical_students$Age)

#Gender Distribution:
ggplot(medical_students, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal()

##Scatter Plot of Weight vs. Height:
ggplot(medical_students, aes(x = Height, y = Weight, color = Gender)) +
  geom_point() +
  labs(title = "Scatter Plot of Weight vs. Height", x = "Height", y = "Weight") +
  theme_minimal()

##  Boxplot of Cholesterol by Diabetes Status:
ggplot(medical_students, aes(x = Diabetes, y = Cholesterol, fill = Diabetes)) +
  geom_boxplot() +
  labs(title = "Cholesterol by Diabetes Status", x = "Diabetes", y = "Cholesterol") +
  theme_minimal()

##grouped bar chart
# Reshape the data into long format
medical_students_long <- medical_students %>%
  pivot_longer(cols = c("BMI", "Heart.Rate", "Cholesterol"), names_to = "Variable", values_to = "Value")

# Create a grouped bar chart
ggplot(medical_students_long, aes(x = Variable, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gender-Based Analysis",
       x = "Variable",
       y = "Value",
       fill = "Gender") +
  theme_minimal()

##grouped bar chart
# Subset the dataset for numeric columns
numeric_subset <- medical_students %>% select(Gender, Heart.Rate, Weight, Height)

# Reshape the numeric data into long format
numeric_subset_long <- numeric_subset %>%
  pivot_longer(cols = c("Heart.Rate", "Weight", "Height"), names_to = "Variable", values_to = "Value")

# Subset the dataset for character columns
character_subset <- medical_students %>% select(Gender, Smoking)

# Reshape the character data into long format
character_subset_long <- character_subset %>%
  pivot_longer(cols = "Smoking", names_to = "Variable", values_to = "Value")

# Combine the two subsets
your_combined_long <- rbind(numeric_subset_long, character_subset_long)

# Create a grouped bar chart for health and lifestyle analysis
ggplot(your_combined_long, aes(x = Variable, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Health and Lifestyle Analysis",
       x = "Variable",
       y = "Value",
       fill = "Gender") +
  theme_minimal()

##
# Subset the dataset for the relevant columns
health_risk_data <- medical_students %>% 
  select(Gender, Smoking, Cholesterol, Diabetes)

# Convert "Cholesterol" to character
health_risk_data$Cholesterol <- as.character(health_risk_data$Cholesterol)

# Reshape the data into long format
health_risk_long <- health_risk_data %>%
  pivot_longer(cols = c("Smoking", "Cholesterol", "Diabetes"), 
               names_to = "Variable", values_to = "Value")

# Create a grouped bar chart for health risk identification
library(ggplot2)

ggplot(health_risk_long, aes(x = Variable, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Health Risk Identification",
       x = "Variable",
       y = "Value",
       fill = "Gender") +
  theme_minimal()













