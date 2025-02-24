############################# LESSON 04: DATA WRANGLING #############################
library(ggplot2)
library(mice)
############################# 01. Identifying Issues #############################
movies <- read.csv('imdb_top_1000.csv', na.strings = c('', 'NA', 'N/A'))
?read.csv

ggplot(movies[20:40,], aes(x = Runtime, y = Meta_score)) +
  geom_point() +
  labs(title = "Runtime vs Metascore")

str(movies)

############################# 02. Data Cleaning #############################
# Step 1: Remove unneeded cols
movies$Poster_Link <- NULL
movies$Overview <- NULL

# Step 2: Formatting
# Release -> Date
# lubridate
?as.POSIXct
movies$Released_Year <- as.POSIXct(movies$Released_Year, format='%Y')

# Certificate -> Factor/Cats. ######### BEWARE NAs
movies$Certificate <- as.factor(movies$Certificate)
summary(movies$Certificate)
movies$Certificate

# Factor variable conversion issue
a <- c("12", '13', '14','12','12')
a <- as.factor(a)
a

b <- as.numeric(as.character(a))
b

# Runtime -> number
as.numeric(movies$Runtime)

?gsub
movies$Runtime <- as.numeric(gsub(' min', '', movies$Runtime))
str(movies)

# Gross -> number
movies$Gross <- as.numeric(gsub(',','', movies$Gross))

str(movies)

############################# 03. Missing Data Analysis #############################
typeof(NA)
T == F
F == F
NA == 15

complete.cases(movies)
movies[!complete.cases(movies),]

missing_df <- movies[which(is.na(movies$Meta_score)),]

colSums(is.na(movies))

md.pattern(movies)
?md.pattern
# Unmasking functions if needed
#mice::md.pattern(movies)
which(is.na(movies$Certificate))

############################# 04. Missing Data Imputation Methods #############################
## Method 1: Factual analysis
movies[967, 'Released_Year'] <- as.POSIXct("1995-09-22", format = "%Y-%m-%d")

movie_ratings <- c(
  'R',     # Seppuku
  'R',     # Ayla: The Daughter of War
  'R',     # Tengoku to Jigoku
  'PG-13', # Babam ve Oglum
  'PG',    # Ikiru
  'PG',    # Ladri di Biciclette
  'PG-13', # Metropolis
  'R',     # Mandariinid
  'R',     # Eskiya
  'R',     # Yôjinbô
  'R',     # Du rififi chez les hommes
  'R',     # Rashômon
  'PG-13', # Kis Uykusu (official IMDb rating)
  'PG-13', # Mary and Max
  'R',     # Underground
  'PG-13', # Persona
  'R',     # La battaglia di Algeri
  'R',     # El ángel exterminador
  'PG',    # The Man Who Shot Liberty Valance
  'R',     # Ivanovo detstvo
  'PG',    # Les quatre cents coups
  'PG-13', # Kakushi-toride no san-akunin
  'PG-13', # Le notti di Cabiria
  'PG',    # Kumonosu-jô
  'PG-13', # White Heat
  'PG',    # The Red Shoes
  'PG',    # The Shop Around the Corner
  'PG',    # La Grande Illusion
  'PG',    # Das Cabinet des Dr. Caligari
  'R',     # Tropa de Elite 2: O Inimigo Agora é Outro
  'PG-13', # Nefes: Vatan Sagolsun
  'PG-13', # G.O.R.A
  'R',     # Vozvrashchenie
  'PG',    # Yeopgijeogin geunyeo
  'PG',    # Vizontele
  'R',     # Knockin' on Heaven's Door
  'PG-13', # 8½
  'PG-13', # Vivre sa vie: Film en douze tableaux
  'R',     # Anatomy of a Murder
  'R',     # The Night of the Hunter
  'PG',    # La Strada
  'PG-13', # Les diaboliques
  'PG',    # Stalag 17
  'PG',    # Roman Holiday
  'PG-13', # In a Lonely Place
  'PG-13', # Out of the Past
  'PG',    # Arsenic and Old Lace
  'PG',    # The Maltese Falcon
  'PG',    # La règle du jeu
  'PG',    # Bronenosets Potemkin
  'R',     # Bir Zamanlar Anadolu'da
  'R',     # 4 luni, 3 saptamâni si 2 zile
  'PG-13', # The Man from Earth
  'R',     # C.R.A.Z.Y
  'R',     # Aguirre, der Zorn Gottes
  'R',     # Night of the Living Dead
  'R',     # Hiroshima mon amour
  'PG',    # Miracle on 34th Street
  'PG',    # The Philadelphia Story
  'PG-13', # Freaks
  'PG',    # Nosferatu
  'PG-13', # Perfetti sconosciuti
  'PG-13', # La grande bellezza
  'PG-13', # Kokuhaku
  'R',     # Ang-ma-reul bo-at-da
  'R',     # Chugyeokja
  'R',     # Druk
  'PG-13', # Auf der anderen Seite
  'R',     # Ondskan
  'PG-13', # Gongdong gyeongbi guyeok JSA
  'R',     # Hana-bi
  'R',     # Naked
  'PG-13', # Wait Until Dark
  'PG-13', # Guess Who's Coming to Dinner
  'PG-13', # Jules et Jim
  'PG',    # Key Largo
  'PG',    # The Lady Vanishes
  'PG',    # Bride of Frankenstein
  'PG',    # Duck Soup
  'R',     # God's Own Country
  'R',     # The Broken Circle Breakdown
  'PG-13', # Detachment
  'R',     # Beasts of No Nation
  'R',     # Death Note: Desu nôto
  'R',     # This Is England
  'PG-13', # Happiness
  'PG-13', # Spoorloos
  'R',     # The Dirty Dozen
  'PG-13', # Repulsion
  'R',     # Peeping Tom
  'PG-13', # Les yeux sans visage
  'PG',    # The Ladykillers
  'PG-13', # Victoria
  'R',     # El cuerpo
  'R',     # Celda 211
  'PG-13', # Die Welle
  'PG',    # The Secret of Kells
  'R',     # Dead Man's Shoes
  'R',     # Batoru rowaiaru
  'PG-13', # Lifeboat
  'PG'     # The 39 Steps
)

movies$Certificate[which(is.na(movies$Certificate))] <- movie_ratings
summary(movies$Certificate)

## Method 2: Median Imputation
median(movies$Gross, na.rm= T)

median_impute_by_group <- function(data, target_col, group_col){
  if (!is.factor(data[[group_col]])){ # factor if necessary
    data[[group_col]] <- as.factor(data[[group_col]])
  }
  print("Column is factor with levels")
  print(paste(levels(data[[group_col]])))
  
  for (level in levels(data[[group_col]])){
    median_val <- median(data[data[[group_col]]==level, target_col], na.rm = T)
    print(paste("Median of",level, "is", median_val))
    
    data[which(is.na(data[[target_col]])) & data[[group_col]] == level, target_col] <- round(median_val)
  }
  
  remaining_nas <- sum(is.na(data[[target_col]]))
  if (remaining_nas != 0){
    print("Grouped NAs still remaining")
    overall_median <- median(data[[target_col]], na.rm = T)
    print(paste("Imputing", remaining_nas, "with overall median:", overall_median))
    data[which(is.na(data[[target_col]])), target_col] <- round(overall_median)
  }
}

movies[which(is.na(movies$Gross)),"Gross"] <- median_impute_by_group(movies, "Gross", "Certificate")

## Method 3: Multiple Chained Equations with MICE
?mice

length(which(is.na(movies$Meta_score)))

cols_for_imp <- c("Meta_score", "Runtime", "Certificate", "Genre", "IMDB_Rating", "Director")

imp_model <- mice(movies[cols_for_imp], maxit = 50, method = 'pmm', seed = 123)

completed_cases <- complete(imp_model, 3)
?complete

movies$Meta_score[which(is.na(movies$Meta_score))] <- completed_cases$Meta_score[which(is.na(movies$Meta_score))]

############################# 05. Comparing Imputation Methods #############################
compare_score_distributions <- function(movies_data, imputed_data) {
  meta_combined <- data.frame(
    value = c(movies_data$Meta_score, imputed_data$Meta_score),
    type = c(rep("Original", nrow(movies_data)), 
             rep("Imputed", nrow(imputed_data))),
    metric = "Meta_score"
  )
  
  imdb_data <- data.frame(
    value = movies_data$IMDB_Rating,
    type = "IMDB_Rating",
    metric = "IMDB_Rating"
  )
  
  p1 <- ggplot(meta_combined, aes(x = value, fill = type)) +
    geom_density(alpha = 0.5) +
    geom_rug(aes(color = type), alpha = 0.6) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Set2") +
    labs(
      title = "Distribution of Meta_score: Original vs Imputed",
      subtitle = paste(
        "Original mean:", round(mean(movies_data$Meta_score, na.rm = TRUE), 2),
        "| Imputed mean:", round(mean(imputed_data$Meta_score, na.rm = TRUE), 2)
      ),
      x = "Meta_score",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  p2 <- ggplot(imdb_data, aes(x = value)) +
    geom_density(fill = "darkgreen", alpha = 0.5) +
    geom_rug(color = "darkgreen", alpha = 0.6) +
    scale_x_continuous(limits = c(5, 10)) +
    labs(
      title = "Distribution of IMDB_Rating (Reference)",
      subtitle = paste("Mean:", round(mean(movies_data$IMDB_Rating, na.rm = TRUE), 2)),
      x = "IMDB_Rating",
      y = "Density"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  combined_plot <- p1 / p2 +
    plot_annotation(
      title = "Comparison of Rating Distributions",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
  return(combined_plot)
    }

compare_distributions(movies, completed_cases)

############################# 06. Best Practices and Considerations #############################
which(!complete.cases(movies))
write.csv(movies, "imdb_top_1000_clean.csv", row.names = F)
