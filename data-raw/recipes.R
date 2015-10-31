library(dplyr)
library(readr)
library(tidyr)

# scp had.co.nz://home/hadley/public/recipes.had.co.nz/db/recipes.sqlite3 .
src <- src_sqlite("data-raw/recipes.sqlite3")

categories <- src %>%
  tbl("categories") %>% 
  collect() %>%
  rename(category = name, category_id = id) %>% 
  left_join(read_csv("data-raw/categories.csv"), "category")

slug <- function(x) {
  x <- tolower(x)
  x <- gsub("'", "", x)
  x <- gsub("[^a-z]+", "-", x)
  x <- gsub("-+", "-", x)
  x <- gsub("^-|-$", "", x)  
  x <- make.unique(x, "-")
}

recipes <- src %>%
  tbl("recipes") %>%
  collect() %>% 
  replace_na(list(category_id = 12)) %>%
  left_join(categories, by = "category_id") %>% 
  mutate(
    updated_on = parse_datetime(updated_on),
    slug = slug(name)
  )

devtools::use_data(categories, recipes, overwrite = TRUE, internal = TRUE)
