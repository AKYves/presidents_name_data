source("00-initialisations.R")

# 2019-06-19
webpage <- read_webpage("Burkina Faso")
vectors_ranges <- 60:79

presidents_list <- extract_presidents(webpage, token_word_two, vectors_ranges)

#special treatment to the line 16.
special_line <- presidents_list[16]
normal_line <- presidents_list[-16]

#special_line treatment
special_line <- str_remove(special_line, "^[^)]+\\)\\s?")


special_line <- special_line %>%
  str_split("Mil") %>%
  unlist() %>%
  paste(c("Mil", "Mil", "")) %>%
  str_squish()

normal_line <- c(normal_line, special_line)
country_data <- data.frame(country = "Burkina Faso", presidents = normal_line)

regex_hstate <- regex("\\(?head of state\\)?", ignore_case = TRUE)

country_data <- country_data %>%
  mutate(presidents = str_remove_all(presidents, regex_hstate),
         presidents = str_squish(presidents),
         presidents = iconv(presidents, "UTF-8", "ASCII//TRANSLIT"),
         presidents = str_remove(presidents, "\\(transitional\\)\\s*")) %>%
  mutate(transition_step = str_extract(presidents, regex_begining)) %>%
  mutate(provisional = grepl("provisional", presidents, ignore.case = TRUE)) %>%
  mutate(military = grepl("mil", presidents, ignore.case = TRUE)) %>%
  dplyr::filter(!is.na(transition_step)) %>%
  mutate(provisional = as.numeric(provisional), military = as.numeric(military)) %>%
  mutate(provisional = ifelse(grepl("Jean-Baptiste", presidents), 2, provisional)) %>%
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
  arrange(start_year, end_year) %>%
  dplyr::select(-presidents)

write.csv(country_data, file = "output_data/Burkina_Faso.csv",
          row.names = FALSE)
