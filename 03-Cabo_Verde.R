source("00-initialisations.R")

# 2019-06-19
webpage <- read_webpage("Cape Verde")
vectors_ranges <- 606:617

presidents_list <- extract_presidents(webpage, token_word_two, vectors_ranges)

special_line <- presidents_list[12]
special_line <- str_split(special_line, "PAICV") %>%
  unlist() %>%
  str_squish()

normal_line <- c(presidents_list, special_line)

country_data <- data.frame(country = "Cabo Verde", presidents = normal_line)

country_data <- country_data %>%
  mutate(presidents = iconv(presidents, "UTF-8", "ASCII//TRANSLIT")) %>%
  mutate(transition_step = str_extract(presidents, regex_begining)) %>%
  mutate(provisional = grepl("provisional", presidents, ignore.case = TRUE)) %>%
  mutate(military = grepl("mil", presidents, ignore.case = TRUE)) %>%
  dplyr::filter(!is.na(transition_step)) %>%
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

write.csv(country_data, file = "output_data/Cabo_Verde.csv",
          row.names = FALSE)


