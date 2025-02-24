# Using the IMDB top 1000 movies dataset, explore the following tasks:
  # 1. Find out which 10 directors are the highest grossing in this list
  # 2. Find out what the best movie is for each (individual) genre, eg. Crime, Drama, Action etc.
  # 3. Compile a plot that shows the association between Runtime and Grossing
  # 4. Assess whether the movie runtime is associated with higher Grossing numbers
  # Bonus. Deliver the following in a list format.
  # Challenge. Do so in as few lines of code as possible!

############################## LESSON 06: LISTS AND APPLIES ############################
M <- matrix(1:9, nrow = 3)

?apply
apply(M, 2, mean)
rowMeans(M)
colMeans(M)
rowSums(M)
colSums(M)

### lapply
numbers <- list(a = 1:3, b=4:6)
lapply(numbers, sum)

### sapply
sapply(numbers, sum,  simplify = F)
?sapply

### Nesting
apply(M, 1, max)
lapply(numbers, function(x) apply(M, 1, max) )

############################## 1. Top Grossing Directors ##############################
movies <- read.csv('imdb_top_1000_clean.csv')
movies

?tapply
directors_gross <- tapply(movies$Gross, movies$Director, sum)
top_directors <- sort(directors_gross, decreasing = T)[1:10]
top_directors

attributes(top_directors)
names(top_directors)
as.numeric(top_directors)

############################### 2. Best Movies by Genre ##############################
?regex
?strsplit
unique_genres <- unique(unlist(strsplit(movies$Genre, ", ")))

best_by_genre <- function(genre, data){
  genre_subset <- data[sapply(strsplit(data$Genre, ", "), function(x) genre %in% x),]
  return(genre_subset[which.max(genre_subset$Gross), c("Series_Title", "Genre", "Gross")])
}

genre_bests <- lapply(unique_genres, best_by_genre, data = movies)
names(genre_bests) <- unique_genres
genre_bests

###############################  3. Correlational Analysis ############################### 
### STEP 1: Parametric or not?
hist(movies$Gross)

ggplot(movies, aes(x = Gross)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Gross Earnings",
       x = "Gross Earnings ($)",
       y = "Count") +
  theme_minimal()

ggplot(movies, aes(x = Runtime)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Runtimes",
       x = "Gross Earnings ($)",
       y = "Count") +
  theme_minimal()

### Q_Q plot
ggplot(movies, aes(sample=Gross))+
  geom_qq()+
  geom_qq_line() +
  labs(x= "Theoretical Quantiles",
      y= "sample quantiles")+
  theme_minimal()

ggplot(movies, aes(sample=Runtime))+
  geom_qq()+
  geom_qq_line() +
  labs(x= "Theoretical Quantiles",
       y= "sample quantiles")+
  theme_minimal()

#### Normality test
?shapiro.test
?ks.test
ks.test(scale(movies$Gross), "pnorm")
ks.test(scale(movies$Runtime), "pnorm")
??jarque.bera.test

?cor.test
cor_test <- cor.test(movies$Gross, movies$Runtime, method="spearman")

runtime_analysis <- ggplot(movies,
       aes(x = Runtime, y = Gross))+
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm")+
  labs(title = "Gross vs Runtime",
       x = "Runtime (mins)",
       y = "Gross") +
  theme_minimal() +
  annotate("text", x = max(movies$Runtime), y = max(movies$Gross),
           label = paste("R2 =", sprintf("%.3f", cor_test$estimate^2),
                         "\np =", sprintf("%.3f", cor_test$p.value)),
           size = 5,
           hjust = 1, vjust = 1)

############################### 5. Other Stat tests Demo ##########################
####### T-test compare means between groups
movie_len <- factor(ifelse(movies$Runtime>90, "Long", "Short"))
t.test(Gross ~ movie_len, data= movies)

####### Non-parametric cases -> Wilcoxon test
wilcox.test(Gross ~ movie_len, data= movies)

####### ANOVA compares means across multiple gruops (parametric)
runtime_cat <- cut(movies$Runtime,
                   breaks = c(0,90,120,150, Inf),
                   labels = c("Short", "Medium", "Long", "Very Long"))
summary(aov(Gross ~ runtime_cat, data = movies))

####### Non-parametric cases -> Kruskal-Wallis test
kruskal.test(Gross ~ runtime_cat, data = movies)

############################ 6. Building Final List ############################
movie_analysis <- list(
  "Top Grossing Director" = top_directors,
  "Best Movie per Genre" = genre_bests,
  "Runtime analysis Plot" = runtime_analysis,
  "Runtime Significance" = T
)

movie_analysis
names(movie_analysis)

movie_analysis$`Top Grossing Director`[3]
movie_analysis[3]
movie_analysis["Runtime Significance"]
movie_analysis[["Top Grossing Director"]][5]

movie_analysis$new <- "Hello"
names(movie_analysis)
movie_analysis$new <- NULL

subset <- movie_analysis[c("Top Grossing Director", "Runtime analysis Plot")]
subset
str(subset)
length(subset)
names(subset)

"Runtime Significance" %in% names(subset)
is.null(subset$nonexistent)

#### GOODBYE!