#Ensure to have all the needed packages
packages_needed <- c("rvest", "dplyr", "stringr", "purrr", "tidyr", "zoo")
remained_packages <-setdiff(packages_needed, installed.packages())
if(length(remained_packages)) lapply(remained_packages, install.packages)

# loading required packages
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# reading the data for all countries.
coun_web <- c("http://www.worldstatesmen.org/Benin.html",
              "http://www.worldstatesmen.org/Burkina_Faso.html",
              "http://www.worldstatesmen.org/Cape_Verde.html",
              "http://www.worldstatesmen.org/Cote_dIvorie.html",
              "http://www.worldstatesmen.org/Gambia.html",
              "http://www.worldstatesmen.org/Ghana.html",
              "http://www.worldstatesmen.org/Guinea.html",
              "http://www.worldstatesmen.org/Guinea-Bissau.htm",
              "http://www.worldstatesmen.org/Liberia.htm",
              "http://www.worldstatesmen.org/Mali.htm",
              "http://www.worldstatesmen.org/Mauritania.htm",
              "http://www.worldstatesmen.org/Niger.htm",
              "http://www.worldstatesmen.org/Nigeria.htm",
              "http://www.worldstatesmen.org/Senegal.html",
              "http://www.worldstatesmen.org/Sierra_Leone.html",
              "http://www.worldstatesmen.org/Togo.html"
              )

#countries name
countries <- str_extract(coun_web, "([^\\/]+\\.(html|htm))$") %>%
  str_replace_all("\\.|html|htm", "") %>%
  str_replace("_|-", " ")

# read webpage of a country

read_webpage <- function(country, web_list = coun_web, names_list = countries){
  read_html(web_list[which(names_list == country)])
}

# this function will do a first processing step for presidents data. It has
# three main arguments: the wepage name -page-, the world (either tt:nth-child or
# dt:nth-child) - token_word-, the begining and the end of the range where to extract
# informations: vect_range

extract_presidents <- function(page, token_word, vect_range){

  #creating the node character
  nodes_to_extract <- paste(token_word, "(",
                            vect_range[-length(vect_range)],
                            "), ", sep = "", collapse = "") %>%
    paste(token_word, "(", vect_range[length(vect_range)], ")",
          sep = "", collapse = "")

  page %>%
    html_nodes(nodes_to_extract) %>%
    html_text() %>%
    map_chr(str_squish)
}

# na_reverse: replace na with previous data, from last to top
na_reverse <- function(vect) zoo::na.locf(vect, fromLast = TRUE, na.rm = FALSE)

# token words
token_word_one <- "dt:nth-child"
token_word_two <- "tt:nth-child"

# regular expression to match the begining of the text
regex_begining <- regex( "^(\\d+[A-z ]+\\d{4} - (\\d+[A-z ]+\\d{4})?[-A-z ]+)")
regex_period <- regex("^(\\d+[A-z ]+\\d{4} - (\\d+[A-z ]+\\d{4})?)")
