# Original code for loading data frames
getwd()
setwd("C:/Users/gmone/475/Project")

# Load packages
library(tidyverse)

# Load datasets
books <- read.csv("BX_Book.csv", sep = ";", header = TRUE)
ratings <- read.csv("BX-Book-Ratings.csv", sep = ";", header = TRUE)
users <- read.csv("BX-Users1.csv", sep = ";", header = TRUE)

#Clean book data
books[c('ISBN', 'Book.Title','Book.Author', 'Year.Of.Publication', 'Publisher', 'Image.URL.S', 'Image.URL.M', 'Image.URL.L')] <- str_split_fixed(books$x, ';', 8)
books <- books[c('ISBN', 'Book.Title','Book.Author', 'Year.Of.Publication', 'Publisher', 'Image.URL.S', 'Image.URL.M', 'Image.URL.L')] 

books <- books %>% mutate(Book.Title = (gsub("\"", "", Book.Title)))
books <- books %>% mutate(Book.Author = (gsub("\"", "", Book.Author)))
books <- books %>% mutate(Year.Of.Publication = (gsub("\"", "", Year.Of.Publication)))
books <- books %>% mutate(Publisher = (gsub("\"", "", Publisher)))
books <- books %>% mutate(Image.URL.S = (gsub("\"", "", Image.URL.S)))
books <- books %>% mutate(Image.URL.M = (gsub("\"", "", Image.URL.M)))
books <- books %>% mutate(Image.URL.L = (gsub("\"", "", Image.URL.L)))

#Clean User data
users$Age <- users$Age........
users <- users %>%
  filter(!grepl("<",Location))
users <- users %>%
  filter(!grepl("<",Age))
users <- users %>%
  filter(!grepl("<",User.ID))
users <- users %>%
  filter(!grepl(";",Location))
users <- users %>%
  filter(!grepl(";",Age))
users <- users %>%
  filter(!grepl(";",User.ID))
#Expand Location into city state and country
users[c('City', 'State','Country')] <- str_split_fixed(users$Location, ',', 3)
users <- users[c('User.ID','City','State','Country','Age')]
users <- users %>%
  filter(!grepl(";",Age))

users$Age = substr(users$Age,1,2)

# Function for user sign in capabilities
user_sign_in <- function() {
  ID <- readline(prompt = "Please Enter your user ID.): ")
  if(ID %in% users$User.ID){
    user_id <<- as.numeric(ID)
  } else {
    cat("Invalid user ID! Returning to menu.\n")
  }
}

# Function to create a user profile
create_user_profile <- function() {
  cat("\nWelcome! Let's create your profile.\n")
  
  # Get user location
  cat("Enter your location details:\n")
  location <- readline(prompt = "Location (City, State, Country): ")
  
  # Get user age
  user_age <- as.numeric(readline(prompt = "Enter your age: "))
  
  # Create a new user profile with fixed ID 0
  if (nrow(users) == 0 || any(!is.numeric(users$User.ID))) {
    new_user <- data.frame(User.ID = 0, Location = location, Age = user_age)
  } else {
    new_user <- data.frame(User.ID = max(as.numeric(users$User.ID), na.rm = TRUE) + 1,
                           Location = location, Age = user_age)
  }
  
  # Insert the new user into the users dataframe
  users <<- rbind(users, new_user)  # Update the global users data frame
  
  cat("\nUser Profile Created:\n")
  print(new_user)
}

# Function to add a book to the system
add_book <- function(user_id) {
  cat("\nAdd a Book to the System:\n")
  
  # Get user ratings for the book
  book_rating <- as.numeric(readline(prompt = "Enter your rating for the book (out of 10): "))
  
  # Get book details
  isbn <- readline(prompt = "Enter the ISBN of the book: ")
  title <- readline(prompt = "Enter the title of the book: ")
  author <- readline(prompt = "Enter the author of the book: ")
  year <- as.numeric(readline(prompt = "Enter the year of publication: "))
  publisher <- readline(prompt = "Enter the publisher of the book: ")
  
  # Check if the book is already in the system
  if (!(isbn %in% books$ISBN)) {
    # If not found, add the book to the data frame with placeholder URLs
    new_book <- data.frame(ISBN = isbn, 
                           Book.Title = title, 
                           Book.Author = author, 
                           Year.Of.Publication = year, 
                           Publisher = publisher,
                           Image.URL.S = "empty",
                           Image.URL.M = "empty",
                           Image.URL.L = "empty")
    
    books <<- rbind(books, new_book)  # Update the global books data frame
    
    cat("\nBook added to the system:\n")
    print(new_book)
  }
  
  # Add the user rating to the ratings data frame
  new_rating <- data.frame(User.ID = user_id, ISBN = isbn, Book.Rating = book_rating)
  ratings <<- rbind(ratings, new_rating)  # Update the global ratings data frame
  
  cat("\nYour rating added to the system:\n")
  print(new_rating)
}

# Function to display top 10 average-ranked books with over 3 ratings
display_top_books <- function() {
  cat("\nTop 10 Average Ranked Books with Over 3 Ratings:\n")
  
  # Data cleaning and processing for general ratings
  book_ratings <- ratings %>%
    filter(User.ID %in% users$User.ID) %>%
    group_by(ISBN) %>%
    summarise(avg_rating = mean(Book.Rating),
              count = n()) %>%
    filter(ISBN %in% books$ISBN)
  
  
  # Filter for books with over 3 ratings
  top_books <- book_ratings %>%
    filter(count > 3)
  
  top_books <- left_join(top_books, books, by="ISBN")
  
  # Display the top 10 average-ranked books
  top_10_books <- head(top_books[order(top_books$avg_rating, decreasing = TRUE), ], 10)
  
  print(top_10_books[, c("ISBN","Book.Title","avg_rating")])
}

# Function to display top 10 average-ranked books with over 3 ratings based on age
display_top_books_by_Age <- function() {
  
  current_user <- users %>%
    filter(User.ID == user_id)
  age <- as.numeric(current_user$Age)
  
  temp1 <- users %>%
    filter(!(Age == "NULL")) %>%
    filter(!grepl("<",Age))
  
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
  
  # Data cleaning and processing for general ratings
  book_ratings <- temp2 %>%
    filter(User.ID %in% users$User.ID) %>%
    group_by(ISBN) %>%
    summarise(avg_rating = mean(Book.Rating),
              count = n()) %>%
    filter(ISBN %in% books$ISBN)
  
  
  # Filter for books with over 3 ratings
  top_books <- book_ratings %>%
    filter(count > 3)
  
  top_books <- left_join(top_books, books, by="ISBN")
  
  # Display the top 10 average-ranked books
  top_10_books <- head(top_books[order(top_books$avg_rating, decreasing = TRUE), ], 10)
  
  print(top_10_books[, c("ISBN","Book.Title","avg_rating")])
}

# Function to display top 10 average-ranked books with over 3 ratings based on Location
display_top_books_by_Location <- function() {
  
  current_user <- users %>%
    filter(User.ID == user_id)
  country <- current_user$Country

  temp1 <- users %>%
    filter(Country == country)
  
  temp2 <- ratings %>%
    filter(ratings$User.ID %in% temp1$User.ID)
  
  # Data cleaning and processing for general ratings
  book_ratings <- temp2 %>%
    filter(User.ID %in% users$User.ID) %>%
    group_by(ISBN) %>%
    summarise(avg_rating = mean(Book.Rating),
              count = n()) %>%
    filter(ISBN %in% books$ISBN)
  
  
  # Filter for books with over 3 ratings
  top_books <- book_ratings %>%
    filter(count > 3)
  
  top_books <- left_join(top_books, books, by="ISBN")
  
  # Display the top 10 average-ranked books
  top_10_books <- head(top_books[order(top_books$avg_rating, decreasing = TRUE), ], 10)
  
  print(top_10_books[, c("ISBN","Book.Title","avg_rating")])
}

# Menu-based interaction
user_id <- NA
while (TRUE) {
  cat("\nMenu Options:\n")
  cat("1. Display top 10 average-ranked books with over 3 ratings\n")
  cat("2. Sign in as existing user\n")
  cat("3. Create a user profile\n")
  cat("4. Add a book to the system and rate it (available after creating a profile)\n")
  cat("5. Display top 10 average-ranked books based of of Age\n")
  cat("6. Display top 10 average-ranked books based of of Location\n")
  cat("7. Quit\n")
  
  choice <- as.numeric(readline(prompt = "Enter your choice (1-4): "))
  
  if (choice == 1) {
    display_top_books()
  } else if (choice == 2) {
    if (is.na(user_id)) {
      user_sign_in()
    } else {
      cat("\nAlready logged in!\n")
    }
  }else if (choice == 3) {
    if (is.na(user_id)) {
      create_user_profile()
      user_id <- max(users$User.ID)
    } else {
      cat("\nAlready logged in!\n")
    }
  } else if (choice == 4) {
    if (!is.na(user_id)) {
      add_book(user_id)
    } else {
      cat("\nPlease create a user profile first!\n")
    }
  } else if (choice == 5) {
    display_top_books_by_Age()
  } else if (choice == 6) {
    display_top_books_by_Location()
  }else if (choice == 7) {
    cat("\nExiting the system. Goodbye!\n")
    break
  } else {
    cat("\nInvalid choice. Please enter a number between 1 and 4.\n")
  }
}

