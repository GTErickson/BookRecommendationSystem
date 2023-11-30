getwd()
setwd("C:/Users/gmone/475/Project")

# Load packages
library(tidyverse)

# Load datasets
books <- read.csv("BX_Books.csv", sep = ";", header = TRUE)
ratings <- read.csv("BX-Book-Ratings.csv", sep = ";", header = TRUE)
users <- read.csv("BX-Users.csv", sep = ";", header = TRUE)

# Prompt user to create a profile
cat("Welcome! Let's create your profile.\n")

# Get user location
cat("Enter your location details:\n")
location <- readline(prompt = "Location (City, State, Country): ")

# Get user age
user_age <- as.numeric(readline(prompt = "Enter your age: "))

# Create a new user profile with fixed ID 0
new_user <- data.frame(User.ID = 0, Location = location, Age = user_age)

# Insert the new user at the top of the users dataframe
users <- rbind(new_user, users)

# Display the updated users dataframe
cat("\nUser Profile Created:\n")
print(new_user)
print(users)

# Data cleaning and processing for general ratings
book_ratings <- ratings %>%
  group_by(ISBN) %>%
  summarise(avg_rating = mean(Book.Rating)) %>%
  filter(ISBN %in% books$ISBN)

book_counts <- ratings %>%
  group_by(ISBN) %>%
  count()

book_ratings$count <- book_counts$n

# Display the first few rows of the general rating data frame
head(book_ratings)

# General rating algorithm
general_ratings <- book_ratings %>%
  filter(avg_rating > 8) %>%
  filter(count > 1)

# User-specific ratings (incomplete, you may need to define what you want to do with the count)
user_counts <- ratings %>%
  group_by(User.ID) %>%
  count()

user_ratings <- ratings %>%
  group_by(User.ID) %>%
  summarise(avg_rating = mean(Book.Rating))

# Display the first few rows of user-specific ratings
head(user_ratings)
