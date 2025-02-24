############################# LESSON 03: DATAFRAMES #############################

############################# 01. Loading and Exploring Data #############################
setwd('/Users/ariat/r_course')

data <- read.csv('imdb_top_1000.csv')
data <- read.csv('/Users/ariat/r_course/imdb_top_1000.csv')

nrow(data)
ncol(data)
str(data)
summary(data)
head(data)
tail(data)

############################# 02. Accessing Data #############################
is.data.frame(data)

data$Series_Title
head(data$Series_Title)

head(data[,2])
data[2,]

head(data[,"Series_Title"])

titles <- head(data[,"Series_Title"])
is.vector(titles)
titles

row <- data[1,]
is.vector(row)
typeof(row)

############################# Building Dataframes #############################
matches_played <- c(19, 36, 34, 32, 21, 29)
player_names <- c("Juanmi Callejon", "Antoine Griezmann", "Luis Suarez", 
                 "Ruben Castro", "Kevin Gameiro", "Cristiano Ronaldo")
minutes_played <- c(1849, 3129, 2940, 2842, 1745, 2634)
goals_scored <- c(11, 16, 28, 13, 13, 25)

?data.frame
df  <-  data.frame(matches_played, goals_scored, minutes_played)
is.data.frame(df)
is.matrix(df)
str(df)

df <- cbind (matches_played, minutes_played, player_names)
is.data.frame(df)
is.matrix(df)
str(df)

############################# 03. Data Manipulation #############################
data$Overall_score <- (data$IMDB_Rating * 10 + data$Meta_score) / 2

data$Overall_score <- NULL

############################# 04. Filtering Data #############################
avg_votes <- mean(data$No_of_Votes)
above_avg_movies <- data[data$No_of_Votes > avg_votes & data$Meta_score > 80,]

head(above_avg_movies)
str(above_avg_movies)

inception_data <- data[data$Series_Title == 'Inception',]
is.data.frame(inception_data)

############################# 05. Piping with Dplyr #############################
library(dplyr)
# install.packages("dplyr")

meta_good_movies <- data %>%
  filter(Meta_score >= 80) %>% 
  select(Series_Title, IMDB_Rating, Meta_score, No_of_Votes, Certificate)

data %>% 
  filter(IMDB_Rating >= 8) %>% 
  filter(No_of_Votes > avg_votes) %>% 
  select(Series_Title, IMDB_Rating, Meta_score, No_of_Votes, Certificate) -> highly_rated_movies

nrow(meta_good_movies)
nrow(highly_rated_movies)

############################# 06. Merging Dataframes #############################
best_movies <- rbind(meta_good_movies, highly_rated_movies)

best_movies <- merge(meta_good_movies, highly_rated_movies,
                     by = c('Series_Title', 'Meta_score'),
                     suffixes = c("_meta", "_imdb"))

best_movies <- best_movies %>% 
  select(Series_Title,
         Meta_score,
         IMDB_Rating_meta,
         No_of_Votes_meta,
         Certificate_meta)

colnames(best_movies) <- c("Title", "Metascore", "IMDB_Rating", "Votes", "Certificate") 
best_movies$`IMDB Rating`

head(best_movies)

############################# 07. Basic Visualisation #############################
?qplot
library(ggplot2)
# install.packages("ggplot2")

best_movies_plot <- qplot(data = best_movies,
      x = reorder(Title, -IMDB_Rating),
      y = IMDB_Rating,
      size = I(1),
      colour = Certificate,
      shape = I(15),
      alpha = I(0.7)) +
      labs(title = "Best movies by IMDB rating",
           x = "Movie title",
           y = "IMDB Rating")

print(best_movies_plot)

ggplot(data=best_movies,
       x= reorder(Title, -IMDB_Rating),
       y = IMDB_Rating)

############################# 08. Data Export #############################
?write.csv()
write.csv(best_movies, "best_movies.csv", row.names = F)
read.csv("best_movies.csv")
