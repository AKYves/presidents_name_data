source("00-initialisations.R")

# This time it was really difficult to process directly from web
presidents_list <- c("1 Jul 1960 - 24 Feb 1966 Kwame Nkrumah (b. 1909 - d. 1972) CPP",
  "24 Feb 1966 - 2 Apr 1969 Joseph Arthur Ankrah (b. 1915 - d. 1992) Mil",
  "2 Apr 1969 - 7 Aug 1970 Okatakyie Akwasi Amankwaa Afrifa (b. 1936 - d. 1979) Mil",
 " 7 Aug 1970 - 31 Aug 1970 Raphael Nii Amaa Ollennu (acting) (b. 1906 - d. 1986) Non-party",
  "31 Aug 1970 - 13 Jan 1972 Edward Akufo-Addo (b. 1906 - d. 1979) Non-party",
  "13 Jan 1972 - 5 Jul 1978 Ignatius Kutu Kwasi Acheampong (b. 1931 - d. 1979) Mil",
  "5 Jul 1978 - 4 Jun 1979 Frederick William Kwasi Akuffo (b. 1937 - d. 1979) Mil",
  "4 Jun 1979 - 24 Sep 1979 Jerry John Kwasi Rawlings (b. 1947) Mil",
  "24 Sep 1979 - 31 Dec 1981 Hilla Limann (= Hilla Babini) (b. 1934 - d. 1998) PNP",
  "31 Dec 1981 - 7 Jan 1993 Jerry John Kwasi Rawlings (s.a.) Mil",
  "7 Jan 1993 - 7 Jan 2001 Jerry John Kwasi Rawlings (s.a.) NDC",
  "7 Jan 2001 - 7 Jan 2009 John Kofi Agyekum Kufuor (b. 1938) NPP",
  "7 Jan 2009 - 24 Jul 2012 John Evans Atta Mills (b. 1944 - d. 2012) NDC",
  "24 Jul 2012 - 7 Jan 2017 John Dramani Mahama (b. 1958) NDC",
  "7 Jan 2017 - Nana Addo Dankwa Akufo-Addo (b. 1944) NPP")

country_data <- data.frame(country = "Ghana", presidents = presidents_list)

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

write.csv(country_data, file = "output_data/Ghana.csv",
          row.names = FALSE)
