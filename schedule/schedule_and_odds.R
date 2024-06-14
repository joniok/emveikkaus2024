library(tidyverse)
source("funs.R")


schedule <- read.delim("schedule/schedule.txt", header=FALSE) %>% 
  separate(V1, c("Pvm", "Klo"), sep = " klo ") %>%
  mutate(Nro = row_number(),
         Lohko = str_extract(V2, "^\\w+"),
         Koti = str_extract(V3, "^\\w+"),
         Vieras = str_extract(V5, "^\\w+")
         ) %>%
  select("Nro", "Pvm", "Klo", "Lohko", "Koti", "Vieras")
  

cnt <- country_codes()

bwin <- read.csv("odds/bwin.csv") %>%
  mutate_all(trimws) %>%
  mutate(home = translate_countries(home, cnt$en, cnt$fi)) %>%
  mutate(away = translate_countries(away, cnt$en, cnt$fi)) %>%
  dplyr::rename(Koti = home, Vieras = away)

schedule_odds <- schedule %>% 
  left_join(bwin, by = c("Koti", "Vieras")) %>%
  mutate(game.id = paste0(translate_countries(Koti, cnt$fi, cnt$cnt),
                          translate_countries(Vieras, cnt$fi, cnt$cnt))
  ) %>%  #add game id
  select(game.id, everything())

schedule_odds <- schedule_odds %>%
  dplyr::rename(`1` = home.odds,
                X = tie.odds,
                `2` = away.odds) %>%
  mutate(Pvm = as.Date(Pvm, format = "%d.%m.%Y")) %>%
  arrange(Pvm, Klo) %>%
  mutate(Pvm = format(Pvm, "%d.%m.%Y"))

write.csv(schedule_odds, "schedule/schedule_odds.csv", row.names = FALSE)