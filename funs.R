format_num <- function(x){
  sprintf("%.2f", as.numeric(x))
}

highlight_col <- function(x){
  cell_spec(x,
            color = ifelse(as.numeric(gsub("-", "0",x)) > 0, "white","white"),
            background = ifelse(as.numeric(gsub("-", "0",x)) > 0, "darkgreen","darksalmon"))
}

add_symbol <- function(x){
  
  process_fun <- function(y){
    
    goals <- unlist(stringr::str_split(y, "-"))
    
    if(all(goals == "")){
      return("-")
    } 
    
    if(goals[1] == goals[2]){
      return("X")
    } else if(goals[1] >goals[2]){
      return("1")    
    } else if( goals[1] < goals[2]){
      return("2")
    } else {
      return("-")
    }
  }
  
  out <- as.character(lapply(x, process_fun))
  return(out)
}


country_codes <- function(){
  cnt <- data.frame(
    rbind(
      c("Albania", "Albania", "ALB", "B"),
      c("Austria", "Itävalta", "AUT", "D"),
      c("Belgium", "Belgia", "BEL", "E"),
      c("Croatia", "Kroatia", "CRO", "B"),
      c("Czech Republic", "Tshekki", "CZE", "F"),
      c("Denmark", "Tanska", "DEN", "C"),
      c("England", "Englanti", "ENG", "C"),
      c("France", "Ranska", "FRA", "D"),
      c("Georgia", "Georgia", "GEO", "F"),
      c("Germany", "Saksa", "GER", "A"),
      c("Hungary", "Unkari", "HUN", "A"),
      c("Italy", "Italia", "ITA", "B"),
      c("Netherlands", "Hollanti", "NED", "D"),
      c("Poland", "Puola", "POL", "D"),
      c("Portugal", "Portugali", "POR", "F"),
      c("Romania", "Romania", "ROM", "E"),
      c("Scotland", "Skotlanti", "SCO", "A"),
      c("Serbia", "Serbia", "SRB", "C"),
      c("Slovakia", "Slovakia", "SVK", "E"),
      c("Slovenia", "Slovenia", "SLO", "C"),
      c("Spain", "Espanja", "ESP", "B"),
      c("Switzerland", "Sveitsi", "SUI", "A"),
      c("Turkey", "Turkki", "TUR", "F"),
      c("Ukraine", "Ukraina", "UKR", "E")
    )
  )
  colnames(cnt) <- c("en", "fi", "cnt", "group")
  
  return(cnt)
}

translate_countries <- function(col, old, new){
  require("plyr")
  
  if(is.factor(col) == FALSE){
    col <- as.factor(col)
  }
  
  return(
    plyr::mapvalues(col,
                    from = as.character(old[old %in% levels(col)]),
                    to = as.character(new[old %in% levels(col)]))
  )
}

flags <- function(){
  
  df <- country_codes() %>%
    rowwise() %>%
    mutate(flag = get_flag(en)) %>%
    select(Joukkue = fi, flag)
  
  return(df)
}

get_flag <- function(cnt) {
  require(emoji)
  require(stringr)
  
  if(str_detect(cnt, "Korea")) {
    cnt <- "kr"
  }
  
  if(str_detect(cnt, "Japan")) {
    cnt <- "jp"
  }
  
  if(str_detect(cnt, "USA")) {
    cnt <- "us"
  }
  
  if(str_detect(cnt, "Turkey")) {
    cnt <- "tr"
  }
  
  # if(str_detect(cnt, "Czech Republic")) {
  #   cnt <- "cz"
  # }
  
  name <- str_replace(tolower(cnt), " ", "_")
  flag <- emoji(name)
  
  return(flag)
}

get_user <- function(file, eval_df, game.ids = all_games, pts_only = TRUE, bets = FALSE){
  require(dplyr)
  require(tidyr)
  require(readr)
  require(janitor)
  
  name <- trimws(stringr::str_to_title(stringr::str_extract(file, "\\w+(?=.csv)")))
  name.pts <- paste(name, "pts", sep = ".")
  
  df <- file %>%
    read.csv(stringsAsFactors = FALSE) %>%
    select(1:13) %>%
    drop_na() %>%
    janitor::row_to_names(1) %>%
    mutate(Tulos = stringr::str_replace_all(Tulos, "\\s", "")) %>% 
    mutate(Merkki = add_symbol(Tulos)) %>%
    left_join(game.ids, by  = "Nro") %>%
    mutate_if(is.character, parse_guess) %>%
    pivot_longer(cols = c("1", "X", "2"), names_to = "odds.labels", values_to = "Kerroin") %>%
    filter(Merkki == odds.labels) %>%
    select(-odds.labels, -Kotimaalit, -Vierasmaalit, -Kerroin) %>%
    dplyr::rename(oma.merkki = Merkki,
                  oma.tulos = Tulos)
  
  base_df <- df %>%
    left_join(eval_df, by = c("game.id")) %>%
    mutate(multiplier = ifelse(oma.tulos == Tulos, 2, 1)) %>% 
    mutate(kerroin.pts = ifelse(oma.merkki == Merkki, as.numeric(Kerroin), 0)) %>%
    mutate(Pts = multiplier * kerroin.pts)
  
  
  if(pts_only == FALSE){
    
    pts_df <- base_df %>%
      select(game.id, oma.tulos, Pts)       
    
    colnames(pts_df) <- c("game.id", name, name.pts)
    
  } else {
    pts_df <- base_df %>%
      summarise(Tulos = sum(oma.tulos == Tulos, na.rm = TRUE),
                `1X2` = sum(oma.merkki == Merkki, na.rm = TRUE),
                Kert = sum(kerroin.pts, na.rm = TRUE),
                Pts = sum(Pts, na.rm = TRUE),
                Nimi = name
      )
  }
  
  return(pts_df) 
}

get_user_data <- function(file) {
  require(tidyverse)
  require(readr)
  
  name <- trimws(stringr::str_to_title(stringr::str_extract(file, "\\w+(?=.csv)")))
  name.pts <- paste(name, "pts", sep = ".")
  
  df <- file %>%
    read.csv(stringsAsFactors = FALSE) %>%
    select(1:13) %>%
    drop_na() %>%
    janitor::row_to_names(1) %>%
    mutate(Tulos = stringr::str_replace_all(Tulos, "\\s", "")) %>% 
    mutate(Merkki = add_symbol(Tulos)) %>%
    mutate_if(is.character, parse_guess) %>%
    pivot_longer(cols = c("1", "X", "2"), names_to = "odds.labels", values_to = "Kerroin") %>%
    filter(Merkki == odds.labels) %>%
    select(-odds.labels, -Kotimaalit, -Vierasmaalit, -Kerroin) %>%
    dplyr::rename(oma.merkki = Merkki,
                  oma.tulos = Tulos)
  
  
  pred <- df %>%
    select(Nro, Koti, Vieras, oma.merkki, oma.tulos) %>%
    mutate(Nimi = name)
  
  return(pred)
}


playoff_stage_fun <- function(x){
  indicator_count <- sum(x, na.rm = TRUE)
  
  if(indicator_count == 0 | indicator_count < 8){
    stage <- "Neljännesvälierät"
  } else if (indicator_count < 12){
    stage <- "Puolivälierät"
  } else if (indicator_count < 14){
    stage <- "Välierät"
  } else if(indicator_count < 15){
    stage <- "Pronssiottelu"
  } else {
    stage <- "Finaali"
  }
  return(stage)
}

## FUNCTIONS
get_user_positions <- function(file){
  require(dplyr)
  require(tidyr)
  require(readr)
  
  name <- trimws(stringr::str_to_title(stringr::str_extract(file, "\\w+(?=.csv)")))
  
  df <- file %>%
    read.csv(stringsAsFactors = FALSE) %>%
    select(23:24) %>% # update each time
    slice(-(1:4)) %>%
    rename(Sij = 1, Veikkaus = 2) %>%
    filter(Veikkaus != "") %>%
    mutate(Lohko = rep(LETTERS[1:6], each = 4),
           Nimi = name) %>%
    mutate_if(is.character, readr::parse_guess)
  
  
  
  return(df)
}

user_position_points <- function(users, truth, write_file = TRUE){
  group_positions <- users %>%
    left_join(truth, by = c("Sij", "Lohko")) %>%
    filter(!is.na(Joukkue)) %>%
    mutate(Pts = ifelse(Joukkue == Veikkaus, 2, 0)) %>%
    group_by(Nimi) %>%
    summarise(Lohko = sum(Pts))
  
  if(write_file == TRUE){
    write.csv(group_positions, "points/group_stage_points.csv", row.names = FALSE)
  }
  
  return(group_positions)
}

get_playoff_bracket <- function(file, country_names){
  require(dplyr)
  
  name <- trimws(stringr::str_to_title(stringr::str_extract(file, "\\w+(?=.csv)")))
  
  df <- file %>%
    read.csv(stringsAsFactors = FALSE) %>%
    select(26:33) %>% # update these to match 
    janitor::remove_empty("cols") %>%
    dplyr::rename(Neljännesvälierät= 1,
                  Puolivälierät = 2,
                  Välierät = 3,
                  #Pronssiottelu = 4,
                  Finaali = 4) %>%
    filter(if_any(everything(), ~ . %in% country_names)) %>%
    mutate(across(where(is.character), ~ na_if(.,""))) %>%
    janitor::remove_empty("rows") %>%
    mutate(Nimi = name) %>% 
    pivot_longer(cols = -Nimi, names_to = "Vaihe", values_to = "Joukkue") %>%
    filter(Joukkue %in% country_names)
  
  
  return(df)
}


bracket_points_by_stage <- function(x){
  if(x == "Neljännesvälierät"){
    stage_pts <- 4
  } else if (x == "Puolivälierät"){
    stage_pts <- 8
  } else if (x == "Välierät"){
    stage_pts <- 12
  } else if (x == "Pronssiottelu"){
    stage_pts <- 10
  } else if(x == "Finaali"){
    stage_pts <- 16
  } else {
    stage_pts <- 0
  }
  return(stage_pts)
}

bracket_points <- function(df) {
  pts <- df %>%
    mutate(Pts = unlist(lapply(Vaihe, bracket_points_by_stage))) %>%
    pivot_wider(id_cols = c("Nimi", "Joukkue"), names_from = Vaihe, values_from = Pts) %>%
    filter(!is.na(Puolivälierät)) %>%
    mutate(Puolivälierät = ifelse(is.na(Välierät), NA, Puolivälierät),
           Välierät = ifelse(is.na(Finaali) , NA, Välierät)) %>%
    pivot_longer(cols = -c("Nimi", "Joukkue"),names_to = "Vaihe", values_to = "Pts") %>% 
    filter(!is.na(Pts))
  
  return(pts)
}

bracket_points_viridis <- function(x){
  if(x == "Neljännesvälierät"){
    stage_pts <- 0.2
  } else if (x == "Puolivälierät"){
    stage_pts <- 0.4
  } else if (x == "Välierät"){
    stage_pts <- 0.6
  } else if(x == "Finaali"){
    stage_pts <- 1
  } else {
    stage_pts <- NA
  }
  return(stage_pts)
}

user_brackets_table <- function(x){
  df <- x %>%
    rowwise() %>% 
    mutate(vaihe_p = bracket_points_viridis(Vaihe)) %>%
    group_by(Nimi, Joukkue) %>%
    filter(vaihe_p == max(vaihe_p)) %>%
    ungroup() %>%
    arrange(Joukkue) %>%
    select(-vaihe_p) %>%
    pivot_wider(names_from = Nimi, values_from = Vaihe)
  
  return(df)
}


player_selections_points <- function(file){
  df <- read.csv(file, stringsAsFactors = FALSE) %>%
    mutate(total = ifelse(!is.na(Oikein),  Oikein * Pts, 0)) %>%
    group_by(Nimi) %>%
    summarize(Pelaaj = sum(total, na.rm =TRUE), .groups = "keep")
  
  return(df)
}




