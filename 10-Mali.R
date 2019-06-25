source("00-initialisations.R")

webpage <- read_webpage("Mali")
vectors_ranges <- 206:253

presidents_list <- extract_presidents(webpage, token_word_two, vectors_ranges)
presidents_list <- presidents_list[presidents_list != ""]

regex_words <- regex("^[A-z()= ]+")
presidents_list[8] <- paste(presidents_list[7], presidents_list[8])
presidents_list <- iconv(presidents_list, from = "UTF-8", to = "ASCII//TRANSLIT")
presidents_list[1] <- str_remove(presidents_list[1], regex_words)
presidents_list[2] <- str_remove(presidents_list[2], regex_words)
presidents_list[2] <- str_remove(presidents_list[2], regex_period)
presidents_list[2] <- str_remove(presidents_list[2], regex_words)

presidents_list[14] <- paste(presidents_list[13], presidents_list[14])
presidents_list[14] <- str_remove(presidents_list[14], regex_words)

presidents_list <- presidents_list[-5]

country_data <- data.frame(country = "Mali", presidents = presidents_list)

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

write.csv(country_data, file = "output_data/Mali.csv",
          row.names = FALSE)
