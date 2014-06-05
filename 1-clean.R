library(dplyr)

# scp had.co.nz://home/hadley/public/recipes.had.co.nz/db/recipes.sqlite3 .
src <- src_sqlite("recipes.sqlite3")

categories <- src %>%
  tbl("categories") %>%
  select(category_id = id, category = name)

recipes <- src %>%
  tbl("recipes") %>%
  left_join(categories) %>%
  select(-category_id) %>%
  collect()

slug <- function(x) {
  x <- tolower(x)
  x <- gsub("'", "", x)
  x <- gsub("[^a-z]+", "-", x)
  x <- gsub("-+", "-", x)
  x <- gsub("^-|-$", "", x)  
  x <- make.unique(x, "-")
}
recipes$slug <- slug(recipes$name)

saveRDS(recipes, "recipes.rds")
