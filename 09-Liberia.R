source("00-initialisations.R")

webpage <- read_webpage("Liberia")
vectors_ranges <- 87:270

presidents_list <- extract_presidents(webpage, token_word_two, vectors_ranges)
presidents_list <- presidents_list[presidents_list != ""]

presidents_list[9] <- paste(presidents_list[7], presidents_list[9])
presidents_list[10] <- paste(presidents_list[7], presidents_list[10])
presidents_list[11] <- paste(presidents_list[7], presidents_list[11])

presidents_list[34] <- paste(presidents_list[32], presidents_list[34])
presidents_list[37] <- paste(presidents_list[32], presidents_list[37],
                             presidents_list[39], presidents_list[40])

presidents_list[43] <- paste(presidents_list[32], presidents_list[43])
presidents_list[47] <- paste(presidents_list[46], presidents_list[47])
presidents_list[52] <- paste(presidents_list[52], presidents_list[53])

presidents_list <- presidents_list[-c(4, 8, 21, 26, 30, 31, 33,  35,
                                      36, 38, 39, 40, 41, 42, 44, 45,
                                      46, 48, 49, 50, 55, 57, 58, 62)]


country_data <- data.frame(country = "Liberia", presidents = presidents_list)

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
         president_name = str_extract(president_name, "^[-A-z. ]+"),
         transition_step = str_extract(transition_step, regex_period)) %>%
  mutate(transition_step = str_squish(str_remove(transition_step, "-")),
         transition_step = zoo::na.locf(transition_step)) %>%
  # a warning due to the last president.
  separate(transition_step, into = c("start_day", "start_month", "start_year",
                                     "end_day", "end_month", "end_year"),
           sep = "\\s") %>%
  dplyr::select(-presidents) %>%
  dplyr::filter(!is.na(president_name)) %>%
  mutate(president_name = str_squish(str_remove(president_name, "[[:punct:]]")))

write.csv(country_data, file = "output_data/Liberia.csv",
          row.names = FALSE)
