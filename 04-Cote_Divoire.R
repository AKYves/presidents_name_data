source("00-initialisations.R")

webpage <- read_webpage("Cote dIvorie")
vectors_ranges <- 347:375

presidents_list <- extract_presidents(webpage, token_word_two, vectors_ranges)
special_line <- presidents_list[22:29]

special_line[1] <- paste(special_line[1], special_line[2])
special_line <- special_line[special_line != ""]
special_line[3] <- paste(special_line[3], special_line[4])

presidents_list <- presidents_list[1:21]
presidents_list <- presidents_list[presidents_list != ""]
special_line <- str_remove(special_line, "^(\\([^(]+\\))\\s+")

normal_line <- c(presidents_list, special_line)
country_data <- data.frame(country = "Côte d'Ivoire", presidents = normal_line)

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
  dplyr::select(-presidents)

write.csv(country_data, file = "output_data/Cote_dIvoire.csv", row.names = FALSE)
