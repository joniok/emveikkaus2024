pacman::p_load(tidyverse, 
               #janitor,
               knitr, 
               kableExtra)

source("funs.R")

# Data
players <- c("Joni", "Juha", "Make", "Miku","Patrik", "Reni", "Rikson", "Santeri")

group_labels <- rep(4, 8)
names(group_labels) <- paste("Lohko", LETTERS[1:8])

user_files <- list.files("users/", full.names = TRUE)

users <- user_files %>% 
  str_extract_all(pattern = paste(players, collapse = "|")) %>%
  unlist()

user_pts <- paste(users, "pts", sep = ".")

user_data <- lapply(user_files, get_user_data) %>% 
  bind_rows()

match_ids <- read.csv("schedule/schedule_odds.csv") %>%
  rename(`1` = X1, `2` = X2) %>%
  mutate(Nro = row_number()) %>%
  pivot_longer(cols = c("1", "X", "2"),names_to = "Merkki", values_to = "Kerroin") %>%
  group_by(game.id) %>%
  mutate(rank = rank(Kerroin)) %>%
  mutate(rank = factor(rank, levels = 1:3, labels = c("Ennakkosuosikki", "Neutraali","Altavastaaja"))) %>%
  mutate(rank = replace_na(rank, "Neutraali")) %>%
  select(-game.id) 

master <- user_data %>%
  left_join(match_ids, by = c("oma.merkki" = "Merkki", "Koti", "Vieras", "Nro"))

# Similarity
similar_list <- master %>% 
  select(Nro, game.id, tulos = oma.tulos, merkki = oma.merkki, Nimi) %>%
  split(f= master$Nimi)


simply_similar <- lapply(similar_list, function(x){
  
  res <- lapply(similar_list, function(y){
    similar <- data.frame(nimi2 = y$Nimi, 
                          similar_prediction = x$tulos == y$tulos,
                          similar_symbol = x$merkki == y$merkki) %>%
      pivot_longer(cols = -nimi2, values_to = "similar", names_to = "type") %>%
      select(-type)
    
    return(similar)
  })
  return(res %>% bind_rows())
}) %>% 
  bind_rows(.id = "nimi1") 

simply_similar_table <- simply_similar %>% 
  group_by(nimi1, nimi2) %>%
  summarize(similar = sum(similar) / n(), .groups = "drop") %>%
  pivot_wider(id_cols = nimi1, names_from = nimi2, values_from = similar)


simply_similar_table %>% 
  pivot_longer(cols = -nimi1, names_to = "nimi2", values_to = "similarity") %>% 
  mutate(similarity = round(similarity, 2)) %>%
  ggplot(aes(nimi1, nimi2, fill = similarity)) + 
  geom_tile() + 
  geom_text(aes(label = similarity), col = "white") +
  viridis::scale_fill_viridis(discrete=FALSE, limits = c(0,1)) +
  labs(x= "", y = "y")

# Odds

user_odds <- master %>% 
  group_by(Nimi) %>%
  summarize(Ennakkosuosikki = sum(rank == "Ennakkosuosikki", na.rm = TRUE),
            Neutraali = sum(rank == "Neutraali", na.rm = TRUE),
            Altavastaaja = sum(rank == "Altavastaaja", na.rm = TRUE),
            Kerroin = sum(Kerroin))

master %>%
  group_by(Nimi, rank) %>%
  summarize(Kerroin = sum(Kerroin)) %>%
  #pivot_longer(cols = c("Ennakkosuosikki", "Neutraali","Altavastaaja"), names_to = "luokka", values_to = "n") %>%
  mutate(luokka = factor(rank, levels = c("Altavastaaja", "Neutraali", "Ennakkosuosikki"))) %>%
  ggplot(aes(x = Nimi, y = Kerroin, fill = luokka )) +
  geom_bar(stat = "identity") +
  labs(y = "Yhteenlaskettu kerroin", x ="", fill ="") +
  theme_minimal() +
  theme(legend.position = "top")
