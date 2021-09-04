# read_data.R

library(tidyverse)
library(lubridate)

read_and_save_data <- function(infile, outfile) {
  
  data <- read_csv(infile)
  
  data <- data %>%
    mutate(
      release_date = mdy(release_date),
      mpaa_rating = as.factor(mpaa_rating),
      year = year(release_date),
      month = month(release_date),
      season = case_when(
        month %in% 3:5 ~ "Spring",
        month %in% 6:8 ~ "Summer",
        month %in% 9:11 ~ "Autumn",
        TRUE ~ "Winter"
      ),
      decade = round(year, -1),
      ratio = worldwide_gross / production_budget,
      season = fct_rev(factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")))
    ) %>%
    filter(year >= 1970)
  
  saveRDS(data, outfile)
  
  data
}
