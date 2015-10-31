library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# scp had.co.nz://home/hadley/public/recipes.had.co.nz/db/recipes.sqlite3 .
recipes <- src_sqlite("data-raw/recipes.sqlite3") %>% 
  tbl("recipes") %>% 
  collect()

categories <- read_csv("data-raw/categories.csv")

slug <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("'", "") %>% 
    str_replace_all("[^a-z0-9]+", "-") %>% 
    str_replace_all("-+", "-") %>% 
    str_replace_all("^-|-$", "")
}

recipes <- recipes %>% 
  replace_na(list(category_id = 12)) %>%
  left_join(categories, by = "category_id") %>% 
  mutate(
    updated_on = parse_datetime(updated_on),
    slug = slug(name)
  )

devtools::use_data(categories, recipes, overwrite = TRUE, internal = TRUE)
