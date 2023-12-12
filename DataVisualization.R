getwd()
setwd("C:/Users/gmone/475/Project/BookRecommendationSystem")

# Load datasets
books <- read.csv("BX_Books.csv", sep = ";", header = TRUE)
ratings <- read.csv("BX-Book-Ratings.csv", sep = ";", header = TRUE)
users <- read.csv("BX-Users.csv", sep = ";", header = TRUE)

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
users$Age = as.numeric(users$Age)

#Visualization of user data

#Age
out <- users %>%
  filter(!is.na(Age)) %>%
  group_by(Age) %>%
  count()

ggplot(data = out, mapping = aes(x = Age, y = n)) +
  geom_point() + 
  expand_limits(y=c(0, 5500)) +
  labs(x="Age",y="Count",
       title="User Age Plot")

#Location
out <- users %>%
  group_by(Country) %>%
  count() %>%
  filter(Country != "")

out <- users %>%
  filter(Country != "")

out <-  head(out[order(out$n, decreasing = TRUE), ], 50)
out <-  head(out[order(out$n, decreasing = TRUE), ], 10)

ggplot(data = out, mapping = aes(x = Country, y = n)) +
  geom_point() + 
  expand_limits(y=c(0, 5500)) +
  labs(x="Country",y="Count",
       title="Top 50 Countries of Users")
