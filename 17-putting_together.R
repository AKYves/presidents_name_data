# putting all together

# all_files <- list.files(".")
# all_r_files <- grep("(\\.R)$", all_files, value = T)
# all_r_files <- all_r_files[-1]
#
# lapply(all_r_files, source)

all_data <- list.files("./output_data")
all_data <- paste("./output_data", all_data, sep = "/")
bulkdata <- lapply(all_data,
                   function(x) read.csv(x, header = T, stringsAsFactors = F)
                   )
bulkdata <- dplyr::bind_rows(bulkdata)

write.csv(bulkdata, file = "bulkdata/bulkdata.csv", row.names = F)

all_wepages_data <- lapply(countries, read_webpage)

#load for use next time.
saveRDS(all_wepages_data, file = "bulkdata/all_webpage_data.rds")

