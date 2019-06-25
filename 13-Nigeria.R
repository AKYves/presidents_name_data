source("00-initialisations.R")

webpage <- read_webpage("Nigeria")

vectors_ranges <- 70:128
presidents_list <- extract_presidents(webpage, token_word_two, vectors_ranges)

presidents_list <- presidents_list[presidents_list != ""]

presidents_list <- presidents_list[-c(1:48)]
presidents_list <- presidents_list[-c(2:4)]
presidents_list[2] <- paste(presidents_list[2], presidents_list[3])
presidents_list[2:3] <- str_split(presidents_list[2], "Head",
                                  simplify = TRUE)
presidents_list[3] <- paste(presidents_list[3],
                            presidents_list[4],
                            presidents_list[5])
presidents_list[3] <- str_remove(presidents_list[3], "^[A-z, ]+")
presidents_list <- presidents_list[-c(4,5)]

presidents_list[4] <- paste(presidents_list[4],
                            presidents_list[5])

presidents_list[4:5] <- str_split(presidents_list[4], "Vacant",
                                  simplify = TRUE)
presidents_list[4] <- paste(presidents_list[4], "Vacant")
presidents_list <- presidents_list[-6]
presidents_list[6] <- str_remove(presidents_list[6], "^(\\([^)]+\\))")
presidents_list[8] <- paste(presidents_list[7], presidents_list[8],
                       presidents_list[9])

presidents_list[6:7] <- str_split(presidents_list[6], "s\\.a\\.",
                                  simplify = TRUE)
presidents_list[6] <- paste(presidents_list[6], "Mil")
presidents_list <- presidents_list[-c(9, 10)]
presidents_list[9] <- paste(presidents_list[9], presidents_list[10])
presidents_list[17] <- str_remove(presidents_list[17], "^[-A-z ]+")

to_repair <- paste(presidents_list[17:21], collapse = " ")

#I know it looks dirty, but no choice...
to_repair <- c("17 Nov 1993 - 8 Jun 1998 Sani Abacha (b. 1943 - d. 1998) Mil",
               "9 Jun 1998 - 29 May 1999 Abdulsalam Abubakar (b. 1942) Mil",
               "29 May 1999 - 29 May 2007 Olusegun Fajinmi Okikiolakan Aremu  Obasanjo (s.a.) PDP",
               "29 May 2007 - 5 May 2010 Umaru Musa Yar'Adua (b. 1951 - d. 2010) PDP (incapacitated by illness from 9 Feb 2010)",
               "9 Feb 2010 - 29 May 2015 Goodluck Ebele Azikiwe Jonathan (b. 1957) PDP (acting for Yar'Adua to 5 May 2010)",
               "29 May 2015 - Muhammadu Buhari")

presidents_list <- presidents_list[-c(17:21)]
presidents_list <- c(presidents_list, to_repair)
presidents_list <- str_remove(presidents_list, "^[A-z()? ]+")

country_data <- data.frame(country = "Nigeria", presidents = presidents_list)


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

write.csv(country_data, file = "output_data/Nigeria.csv",
          row.names = FALSE)
