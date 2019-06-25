source("00-initialisations.R")

webpage <- read_webpage("Niger")

vectors_ranges <- 151:223
presidents_list <- extract_presidents(webpage, token_word_two, vectors_ranges)

presidents_list <- presidents_list[presidents_list != ""]

presidents_list[8] <- paste(presidents_list[5:8], collapse = "")
presidents_list <- presidents_list[-c(5:7)]
presidents_list[9] <- paste(presidents_list[8], presidents_list[9])
presidents_list[12] <- paste(presidents_list[9:12], collapse = "")
presidents_list <- presidents_list[-c(9:11)]
presidents_list[12] <- paste(presidents_list[11], presidents_list[12])
presidents_list[15] <- paste(presidents_list[12:15], collapse = "")
presidents_list <- presidents_list[-c(12:14)]
presidents_list[19] <- paste(presidents_list[19], presidents_list[20])

presidents_list <- presidents_list[-c(2, 4, 6, 7, 8, 10, 11, 14, 16,
                                      18, 20, 21, 23, 25, 26, 28)]


country_data <- data.frame(country = "Niger", presidents = presidents_list)

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

write.csv(country_data, file = "output_data/Niger.csv",
          row.names = FALSE)
