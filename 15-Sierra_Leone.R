source("00-initialisations.R")

webpage <- read_webpage("Sierra Leone")
vectors_ranges <- 439:489
presidents_list <- extract_presidents(webpage, token_word_two, vectors_ranges)

presidents_list <- presidents_list[presidents_list != ""]

presidents_list[1] <- paste(presidents_list[1], presidents_list[3])
presidents_list <- presidents_list[-c(2, 3)]
presidents_list[5] <- paste(presidents_list[5], presidents_list[6])
presidents_list[7] <- str_remove(presidents_list[7], "^[A-z ]+")
presidents_list[8] <- paste(presidents_list[8], presidents_list[10],
                            presidents_list[9])
presidents_list[11] <- paste(presidents_list[11],
                             presidents_list[12])

presidents_list[17] <- paste(presidents_list[17], presidents_list[18])
presidents_list[17:18] <- str_split(presidents_list[17], "State",
                                    simplify = TRUE)

presidents_list[21] <- paste(presidents_list[20], presidents_list[21])
presidents_list[22] <- paste(presidents_list[22], presidents_list[23])
presidents_list[22:23] <- str_split(presidents_list[22], "APC",
                                    simplify = TRUE)

presidents_list <- str_squish(presidents_list)


country_data <- data.frame(country = "Sierra Leone", presidents = presidents_list)

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

write.csv(country_data, file = "output_data/Sierra_Leone.csv",
          row.names = FALSE)
