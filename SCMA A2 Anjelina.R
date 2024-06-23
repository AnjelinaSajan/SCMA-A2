#NSSO
library(dplyr)
setwd('C:\\Users\\anjel\\Downloads')
getwd()
# Load the dataset
data <- read.csv("NSSO68.csv")
unique(data$state_1)
# Subset data to state assigned
subset_data <- data %>%
  filter(state_1 == 'MH') %>%
  select(foodtotal_q, MPCE_MRP, MPCE_URP,Age,Meals_At_Home,Possess_ration_card,Education, No_of_Meals_per_day)
print(subset_data)

sum(is.na(subset_data$MPCE_MRP))
sum(is.na(subset_data$MPCE_URP))
sum(is.na(subset_data$Age))
sum(is.na(subset_data$Possess_ration_card))
sum(is.na(data$Education))

impute_with_mean <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
}

# Columns to impute
columns_to_impute <- c("Education")

# Impute missing values with mean
data <- impute_with_mean(data, columns_to_impute)

sum(is.na(data$Education))

# Fit the regression model
model <- lm(foodtotal_q~ MPCE_MRP+MPCE_URP+Age+Meals_At_Home+Possess_ration_card+Education, data = subset_data)

# Print the regression results
print(summary(model))


library(car)
# Check for multicollinearity using Variance Inflation Factor (VIF)
vif(model) # VIF Value more than 8 its problematic

# Extract the coefficients from the model
coefficients <- coef(model)

# Construct the equation
equation <- paste0("y = ", round(coefficients[1], 2))
for (i in 2:length(coefficients)) {
  equation <- paste0(equation, " + ", round(coefficients[i], 6), "*x", i-1)
}
# Print the equation
print(equation)




head(subset_data$MPCE_MRP,1)
head(subset_data$MPCE_URP,1)
head(subset_data$Age,1) 
head(subset_data$Meals_At_Home,1)
head(subset_data$Possess_ration_card,1) 
head(subset_data$Education,1)
head(subset_data$foodtotal_q,1)



setwd('C:\\Users\\anjel\\Downloads\\SCMA')
getwd()
library(dplyr)
library(readr)
library(readxl)
library(stringdist)

df_ipl <- read_csv("IPL_ball_by_ball_updated till 2024.csv")
salary <- read_excel("IPL SALARIES 2024.xlsx")

colnames(df_ipl)

grouped_data <- df_ipl %>%
  group_by(Season, `Innings No`, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored), wicket_confirmation = sum(wicket_confirmation)) %>%
  ungroup()

total_runs_each_year <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored)) %>%
  ungroup()

total_wicket_each_year <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation)) %>%
  ungroup()

match_names <- function(name, names_list) {
  matches <- stringsim(name, names_list, method = 'jw')
  best_match <- names_list[which.max(matches)]
  if (max(matches) >= 0.8) {
    return(best_match)
  } else {
    return(NA)
  }
}

df_salary <- salary
df_runs <- total_runs_each_year

df_salary$Matched_Player <- sapply(df_salary$Player, function(x) match_names(x, df_runs$Striker))

df_merged <- merge(df_salary, df_runs, by.x = 'Matched_Player', by.y = 'Striker', all.x = TRUE)


install.packages("caret")

library(caret)
library(MASS)

X <- df_merged$runs_scored  # Independent variable(s)
y <- df_merged$Rs  # Dependent variable

trainIndex <- createDataPartition(y, p = .8, 
                                  list = FALSE, 
                                  times = 1)

# Ensure X is a data frame
X <- data.frame(runs_scored = df_merged$runs_scored)

# Dependent variable
y <- df_merged$Rs

# Create train/test split indices
trainIndex <- createDataPartition(y, p = .8, list = FALSE, times = 1)

# Split the data
X_train <- X[trainIndex, , drop = FALSE]  # drop = FALSE to maintain data frame structure
X_test <- X[-trainIndex, , drop = FALSE]  # drop = FALSE to maintain data frame structure

# Check the resulting dimensions
print(dim(X_train))
print(dim(X_test))


X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

model <- lm(y_train ~ X_train)

y_pred <- predict(model, newdata = data.frame(X_train = X_test))

mse <- mean((y_test - y_pred)^2)

X <- as.data.frame(X)
 X_train <- X[trainIndex, ]

 X <- as.matrix(X)
 X_train <- X[trainIndex, , drop = FALSE]
 
 X_train <- X[trainIndex]
 
 library(car)
 
 # Example data (replace with your actual data)
 set.seed(123)  # For reproducibility
 
 # Generate example data
 X_train <- matrix(rnorm(100), ncol = 5)  # Example matrix for X_train
 y_train <- rnorm(20)  # Example numeric vector for y_train
 
 # Add intercept to X_train
 X_train <- cbind(1, X_train)
 
 # Combine y_train and X_train into a data frame
 data <- data.frame(y_train = y_train, X_train)
 
 # Fit linear model
 model <- lm(y_train ~ ., data = data)
 
 # View summary of the model
 summary(model)
 
 # Install and load the stringdist package
 if (!requireNamespace("stringdist", quietly = TRUE)) {
   install.packages("stringdist")
 }
 library(stringdist)
 
 # Define the match_names function
 match_names <- function(name, names_list) {
   # Compute the similarity score
   similarity_scores <- stringsim(name, names_list, method = "jw")
   # Return the index of the most similar name
   return(names_list[which.max(similarity_scores)])
 }
 
 
 # Install and load the required packages
 if (!requireNamespace("stringdist", quietly = TRUE)) {
   install.packages("stringdist")
 }
 if (!requireNamespace("dplyr", quietly = TRUE)) {
   install.packages("dplyr")
 }
 
 # Load the packages
 library(stringdist)
 library(dplyr)
 
 # Define the match_names function
 match_names <- function(name, names_list) {
   # Compute the similarity score
   similarity_scores <- stringsim(name, names_list, method = "jw")
   # Return the index of the most similar name
   return(names_list[which.max(similarity_scores)])
 }
 
 # Ensure the two data frames are correctly named and contain the necessary columns
 df_salary <- salary
 df_runs <- total_wicket_each_year
 
 # Add a matched player column based on name similarity
 df_salary$Matched_Player <- sapply(df_salary$Player, function(x) match_names(x, df_runs$Bowler))
 
 # Merge the data frames on the matched player names
 df_merged <- merge(df_salary, df_runs, by.x = 'Matched_Player', by.y = 'Bowler', all.x = TRUE)
 
 # Filter rows based on wicket confirmation
 df_merged <- df_merged %>% filter(wicket_confirmation > 10)
 
 # Further filter rows based on season
 df_merged <- df_merged %>% filter(Season %in% c('2022'))
 
 
 
 
 
 
 
 # Further filter rows based on season
 df_merged <- df_merged %>% filter(Season %in% c('2022'))
 
 
 
 X <- df_merged$wicket_confirmation  # Independent variable(s)
 y <- df_merged$Rs  # Dependent variable
 
 trainIndex <- createDataPartition(y, p = .8, 
                                   list = FALSE, 
                                   times = 1)
 
 # Install and load required packages
 if (!requireNamespace("caret", quietly = TRUE)) {
   install.packages("caret")
 }
 library(caret)
 
 # Assuming df_merged is already created and contains runs_scored and Rs columns
 
 # Convert X to a data frame
 X <- data.frame(runs_scored = df_merged$runs_scored)
 
 # Dependent variable
 y <- df_merged$Rs
 
 # Create train/test split indices
 trainIndex <- createDataPartition(y, p = .8, list = FALSE, times = 1)
 
 # Split the data
 X_train <- X[trainIndex, , drop = FALSE]  # use drop = FALSE to maintain data frame structure
 X_test <- X[-trainIndex, , drop = FALSE]  # use drop = FALSE to maintain data frame structure
 
 # Print dimensions to verify
 print(dim(X_train))
 print(dim(X_test))
 
 
 
 
 X_train <- X[trainIndex, ]
 X_test <- X[-trainIndex, ]
 y_train <- y[trainIndex]
 y_test <- y[-trainIndex]
 
 # Example data (replace with your actual data)
 set.seed(123)  # For reproducibility
 
 # Generate example data
 X <- rnorm(100)  # Example numeric vector for X
 y <- rnorm(100)  # Example numeric vector for y
 
 # Install and load required packages
 if (!requireNamespace("caret", quietly = TRUE)) {
   install.packages("caret")
 }
 library(caret)
 
 # Assuming df_merged is already created and contains runs_scored and Rs columns
 
 # Convert X to a data frame
 X <- data.frame(runs_scored = df_merged$runs_scored)
 
 # Dependent variable
 y <- df_merged$Rs
 
 # Create train/test split indices
 trainIndex <- createDataPartition(y, p = .8, list = FALSE, times = 1)
 
 # Split the data
 X_train <- X[trainIndex, , drop = FALSE]  # use drop = FALSE to maintain data frame structure
 X_test <- X[-trainIndex, , drop = FALSE]  # use drop = FALSE to maintain data frame structure
 
 # Print dimensions to verify
 print(dim(X_train))
 print(dim(X_test))
 
 
 X_train <- X[trainIndex]
 X_test <- X[-trainIndex]
 y_train <- y[trainIndex]
 y_test <- y[-trainIndex]
 
 # Check dimensions
 length(X_train)
 length(X_test)
 length(y_train)
 length(y_test)
 
 
 
 X_train <- cbind(1, X_train)  # Adding constant
 model <- lm(y_train ~ ., data = data.frame(X_train))
 summary(model)
 


