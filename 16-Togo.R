source("00-initialisations.R")

webpage <- read_webpage("Togo")
vectors_ranges <- 99:146
presidents_list <- extract_presidents(webpage, token_word_two, vectors_ranges)

presidents_list <- presidents_list[presidents_list != ""]
presidents_list <- presidents_list[-c(1:7)]

country_data <- data.frame(country = "Togo", presidents = presidents_list)

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

write.csv(country_data, file = "output_data/Togo.csv",
          row.names = FALSE)

