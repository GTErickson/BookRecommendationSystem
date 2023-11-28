#Set to proper working directory
getwd()
setwd("C:/Users/gmone/475/Project")

#Load proper packages
library(tidyverse)

#Load data sets
books <- read.csv("BX_Books.csv", sep = ";", header = TRUE)
ratings <- read.csv("BX-Book-Ratings.csv", sep = ";", header = TRUE)
users <- read.csv("BX-Users.csv", sep = ";", header = TRUE)

#Get general rating data frame
x = ratings %>%
  group_by(ISBN) %>%  #Group ratings based on book
  summarise(avg_rating = mean(Book.Rating)) %>%  #Get average rating for each book
  filter(ISBN %in% books$ISBN)  #Clean data for poor rating entries

y = ratings %>%
  group_by(ISBN) %>%
  count()

x$count = y$n

head(x) 

#Now general rating algorithm
out = x %>%
  filter(avg_rating > 8) %>%
  filter(count > 1)
  

#Now we want it to get user specific (User # )

y = ratings %>%
  group_by(User.ID) %>%
  count()

x = ratings %>%
  group_by(User.ID) %>%
  summarise(avg_rating = mean(Book.Rating))
