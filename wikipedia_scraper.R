library(rvest)
library(tidyverse)

base_url <- "https://fi.wikipedia.org/wiki/Jalkapallon_Euroopan-mestaruuskilpailut_2024#Alkulohkot"

tables <- base_url %>%
  read_html() %>%
  html_nodes(".wikitable") %>%
  keep(~grepl("Sij\\.", .)) %>%
  html_table()

process_table <- function(tab){
  tab %>%
    dplyr::rename(Joukkue = 2 ) %>%
    mutate(Joukkue = stringr::str_replace(Joukkue, " \\s*\\([^\\)]+\\)", "")) %>%
    mutate(P = as.character(P))
}

positions <- lapply(tables[1:6], process_table) %>%
  bind_rows(.id = "Lohko") %>%
  mutate(Lohko = plyr::mapvalues(Lohko, from = as.character(1:6), to = LETTERS[1:6]))

evaluate_groups <- c("A" = TRUE,
                     "B" = TRUE,
                     "C" = TRUE,
                     "D" = TRUE,
                     "E" = TRUE,
                     "F" = FALSE)

group_stage_positions <- positions %>%
  select(Lohko, Sij., Joukkue) %>%
  mutate(Joukkue = ifelse(Joukkue == "Alankomaat", "Hollanti", Joukkue)) %>%
  mutate(Joukkue = ifelse(Joukkue == "Tšekki", "Tshekki", Joukkue)) %>%
  dplyr::rename (Sij = Sij.) %>%
  filter(Lohko %in% names(evaluate_groups)[evaluate_groups]) 

write.csv(group_stage_positions, file = "points/group_stage.csv", row.names = FALSE)