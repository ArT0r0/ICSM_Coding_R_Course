############################# LESSON 05: GRAMMAR OF GRAPHICS ############################
# 1. DATA
# 2. AESTHETICS
# 3. GEOMETRIES
# 4. FACETS
# 5. STATISTICS
# 6. COORDINATES
# 7. THEMES

library(ggplot2)
?ggplot2
?ggplot

base_plot <- ggplot(movies, # Data
       aes(x = Runtime, y = Meta_score, colour = Certificate)) +  # Aesthetics
  geom_point() + # Geometries
  facet_wrap(vars(Certificate)) + # Facets
  geom_smooth() + # Statistics
  coord_cartesian(ylim = c(0,100)) + # Coordinates
  theme_minimal() +
  labs(title = "Meta Ratings against Runtime", subtitle = "Stratified by Certificate",
       x = "Runtime (mins)",
       y = "Meta Score") # Theme

base_plot + geom_line() + aes(colour = Released_Year)

##################### Distributions ######################
ggplot(movies, aes(x= No_of_Votes))+
  geom_histogram(binwidth = 100000,
                 aes(fill = Certificate), colour = 'black') +
  labs(title = "Distribution of Votes by Certificate",
       x = "Nmber of Votes",
       y = "Frequency")

ggplot(movies[movies$No_of_Votes >100000,],
       aes(x = IMDB_Rating)) +
  geom_density(aes(fill = Certificate), position = "stack")

#################### Boxplots #######################
ggplot(movies, aes(x = Certificate, 
                   y = Meta_score,
                   colour = I("black"))) +
  geom_jitter(size = 0.8, alpha = 0.5) +
  geom_boxplot(aes(fill = Certificate), alpha = 0.7) +
  labs(title = "Score distribution by Certificate",
       x = "Certificate", 
       y = "Meta Score")

################### Faceting #########################
ggplot(movies[1:500,],
       aes(x = Meta_score,
           y = log(No_of_Votes))) +
  geom_point(aes(colour = Certificate, 
                 size = Gross)) +
  facet_grid(Certificate ~ .) +
  labs(title = "Meta score by certificate",
       subtitle = "Sizes corresponding to Gross",
       x = "Meta score", 
       y = "Log(Number of Votes)")

################## Timeseries ########################
ggplot(movies, aes(x = Released_Year, y = Runtime)) +
  geom_line() +
  geom_smooth()

################### Styling ###########################
ggplot(movies, aes(x= Meta_score))+
  geom_histogram(binwidth = 10, 
                 aes(fill = Certificate),
                 colour = 'Black') + 
  labs(title = "Distribution of Meta scores by certificate",
       x = "Meta Score",
       y = "Number of movies") +
  theme_minimal()+
  theme(
    axis.title = element_text(colour = 'Blue', size = 12),
    axis.text = element_text(colour = 'Black', size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = c(0.9,0.7),
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(
      colour = "Black",
      size = 20,
      face = "bold"
    )
  )


