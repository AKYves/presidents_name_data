# cleaning the bulkdata for a further use.
library(magrittr)
library(dplyr)

bulkdata <- read.csv(file = "bulkdata/bulkdata.csv", header = TRUE)

bulkdata_cleaned <- bulkdata %>%
  dplyr::filter(!grepl("provisional", president_name, ignore.case = TRUE)) %>%
  mutate(president_name = gsub("[[:punct:]]", " ", president_name),
         president_name = trimws(president_name)) %>%
  distinct()

visdat::vis_miss(bulkdata_cleaned)

write.csv(bulkdata_cleaned, file = "bulkdata/bulkdata_cleaned.csv",
          row.names = FALSE)
