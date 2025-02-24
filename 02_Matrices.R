############################# LESSON 02: MATRICES #############################

############################# 01. Building Matrices #############################
matches_played <- c(19, 36, 34, 32, 21, 29)
player_names <- c("Juanmi Callejon", "Antoine Griezmann", "Luis Suarez", 
                  "Ruben Castro", "Kevin Gameiro", "Cristiano Ronaldo")
minutes_played <- c(1849, 3129, 2940, 2842, 1745, 2634)
goals_scored <- c(11, 16, 28, 13, 13, 25)

?matrix()
player_matrix <-  matrix(player_names, nrow = 2, ncol = 3)
player_matrix

player_matrix <-  matrix(player_names, nrow = 2, ncol = 3, byrow= T)
player_matrix

is.matrix(player_matrix)
#[ROW, COLUMN] # 1-based index 

### rbind
stats_matrix <-  rbind(player_names, minutes_played, goals_scored)
stats_matrix

is.matrix(stats_matrix)
is.numeric(stats_matrix)

### cbind
stats_matrix <- cbind(matches_played, minutes_played, goals_scored)
stats_matrix

is.numeric(stats_matrix)
?rownames()
rownames(stats_matrix) <- player_names

############################# 02. Matrix Indexing #############################
stats_matrix[1,]
stats_matrix[1,1]
stats_matrix[1,3]
stats_matrix[,1]

stats_matrix["Juanmi Callejon",]
stats_matrix["Cristiano Ronaldo",]
stats_matrix[,"goals_scored"]
stats_matrix["Cristiano Ronaldo",1]

############################# 03. Matrix Operations #############################
goals_per_match <- stats_matrix[,"goals_scored"] / stats_matrix[,"matches_played"]
goals_per_match

stats_matrix <- cbind(stats_matrix, goals_per_match)
stats_matrix

############################# 04. Matrix Visualisation #############################
?matplot()
?barplot()
?t()
barplot(t(stats_matrix[,c("goals_scored", "goals_per_match")]),
        col = c('darkblue','darkred'), # R colour palette
        las = 2,
        main = "Plot of Goals Scored vs Efficiency",
        #sub = "Goals vs efficiency per player",
        ylim = c(0,30),
        legend.text = c("Total Goals", "Goals per Match"),
        args.legend = list(x="bottomright")
        )

grid()



