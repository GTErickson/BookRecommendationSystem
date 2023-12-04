getwd()
setwd("C:/Users/gmone/475/Project")

# Load packages
library(tidyverse)
library(dplyr)
library(stringr)


# Load datasets
books <- read.csv("BX_Books.csv", sep=";")
ratings <- read.csv("BX-Book-Ratings.csv", sep = ";", header = TRUE)
users <- read.csv("BX-Users.csv", sep = ";", header = TRUE)
users1 <- read.csv("BX-Users1.csv", sep = ";", header = TRUE)

books[c('ISBN', 'Book.Title','Book.Author', 'Year.Of.Publication', 'Publisher', )] <- str_split_fixed(books$x, ',', 3)
users <- users[c('User.ID','City','State','Country','Age')]

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

# Data cleaning ratings data
book_ratings <- ratings %>%
  group_by(ISBN) %>%
  summarise(avg_rating = mean(Book.Rating)) %>%
  filter(ISBN %in% books$ISBN)

book_counts <- ratings %>%
  group_by(ISBN) %>%
  count()

book_ratings <- left_join(book_ratings, book_counts, by="ISBN")

# Display the first few rows of the general rating data frame
head(book_ratings)

# General rating algorithm
general_ratings <- book_ratings %>%
  filter(avg_rating > 8) %>%
  filter(count > 1)

# More data cleaning (incomplete, but want to filter out users with large number of ratings in the future)
user_counts <- ratings %>%
  group_by(User.ID) %>%
  count()

user_ratings <- ratings %>%
  group_by(User.ID) %>%
  summarise(avg_rating = mean(Book.Rating))

# Display the first few rows of user-specific ratings
head(user_ratings)

#Clean User data

#Expand Location into city state and country
users[c('City', 'State','Country')] <- str_split_fixed(users$Location, ',', 3)
users <- users[c('User.ID','City','State','Country','Age')]

#Clean Entries within users
dirty <- users1 %>%
  filter(grepl(";",Location))

dirty <- books %>%
  filter(grepl(";",Book.Title))

#Age-based recommendation (User 218411)

#Get users age
current_user <- users %>%
  filter(User.ID == 218411)

age <- as.numeric(current_user$Age)
country <- current_user$Country

#Filter all books that have already been read
temp1 <- users %>%
  filter(!(Age == "NULL")) %>%
  filter(grepl("<",Age))

upper <- temp1 %>%
  filter(as.numeric(Age) <= (age + 5))

lower <- temp1 %>%
  filter(as.numeric(Age) >= (age - 5))

temp1 <- inner_join(upper, lower, by="User.ID")
temp1$City = temp1$City.x
temp1$State = temp1$State.x
temp1$Country = temp1$Country.x
temp1$Age = temp1$Age.x
temp1 <- temp1[c('User.ID','City','State','Country','Age')]

temp2 <- ratings %>%
  filter(ratings$User.ID %in% temp1$User.ID)

book_ratings <- temp2 %>%
  group_by(ISBN) %>%
  summarise(avg_rating = mean(Book.Rating)) %>%
  filter(ISBN %in% books$ISBN)

book_counts <- ratings %>%
  group_by(ISBN) %>%
  count()

book_ratings <- left_join(book_ratings, book_counts, by="ISBN")

#Location-based recommendation (User 2110)
Location <- current_user$Country

temp1 <- users %>%
  filter(Country == Location)

temp2 <- ratings %>%
  filter(ratings$User.ID %in% temp1$User.ID)

book_ratings <- temp2 %>%
  group_by(ISBN) %>%
  summarise(avg_rating = mean(Book.Rating)) %>%
  filter(ISBN %in% books$ISBN)

book_counts <- ratings %>%
  group_by(ISBN) %>%
  count()

book_ratings <- left_join(book_ratings, book_counts, by="ISBN")
