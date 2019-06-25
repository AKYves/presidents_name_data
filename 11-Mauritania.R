source("00-initialisations.R")

webpage <- read_webpage("Mauritania")
vectors_ranges <- 112:132

presidents_list <- extract_presidents(webpage, token_word_two, vectors_ranges)

special_one <- presidents_list[9] %>% str_split("\\s") %>% unlist()

special_one <- c(paste(special_one[1:17],  collapse = " "),
                 paste(special_one[18:41], collapse = " "),
                 paste(special_one[42:55], collapse = " "),
                 paste(special_one[56:73], collapse = " "),
                 paste(special_one[74:83], collapse = " "))

normal_one <- presidents_list[-9]
normal_one <- c(normal_one, special_one)
normal_one[3] <- str_remove(normal_one[3], "^[A-z, ]+")
normal_one[4] <- str_remove(normal_one[4], "^[A-z, ]+")


country_data <- data.frame(country = "Mauritania", presidents = normal_one)

country_data <- country_data %>%
  mutate(presidents = iconv(presidents, "UTF-8", "ASCII//TRANSLIT")) %>%
  mutate(transition_step = str_extract(presidents, regex_begining)) %>%
  mutate(provisional = grepl("provisional", presidents, ignore.case = TRUE)) %>%
  mutate(military = grepl("mil", presidents, ignore.case = TRUE)) %>%
  mutate(provisional = ifelse(is.na(transition_step), NA, provisional),
         provisional = zoo::na.locf(provisional)) %>%
  dplyr::filter(!is.na(transition_step) | provisional | military) %>%
  mutate(transition_step = ifelse(is.na(transition_step), presidents,
                                  transition_step)) %>%
  mutate(president_name = str_remove(transition_step, regex_period),
         president_name = str_squish(president_name),
         president_name = str_extract(president_name, "^[-A-z ]+"),
         transition_step = str_extract(transition_step, regex_period)) %>%
  mutate(transition_step = str_squish(str_remove(transition_step, "-")),
         transition_step = zoo::na.locf(transition_step)) %>%
  # a warning due to the last president.
  separate(transition_step, into = c("start_day", "start_month", "start_year",
                                     "end_day", "end_month", "end_year"),
           sep = "\\s") %>%
  dplyr::select(-presidents) %>%
  dplyr::filter(!is.na(president_name))

write.csv(country_data, file = "output_data/Mauritania.csv",
          row.names = FALSE)
