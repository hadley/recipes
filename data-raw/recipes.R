library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# scp 50.57.167.97://home/hadley/public/recipes.had.co.nz/db/recipes.sqlite3 .
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

# Save out recipes as md so can easily diff
paths <- file.path("recipes", categories$path)
lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE)
for (i in seq_len(nrow(recipes))) {
  recipes:::save_recipe(recipes[i, ])  
}

devtools::use_data(categories, overwrite = TRUE, internal = TRUE)
