library(dplyr)
library(yaml)
library(readr)

categories <- read.csv("categories.csv", stringsAsFactors = FALSE)

paths <- file.path("recipes", categories$path)
invisible(lapply(paths[!file.exists(paths)], dir.create))

recipes <- readRDS("recipes.rds") %>%
  mutate(category = ifelse(is.na(category), "Miscellaneous", category)) %>% 
  left_join(categories) %>% 
  mutate(updated_on = parse_datetime(updated_on))

trim <- function(x) {
  x <- gsub("\r", "", x)
  x <- gsub("^\n+", "", x)
  x <- gsub("\n+$", "", x)
  x
}


save_recipe <- function(recipe) {
  meta <- unlist(recipe[c("name", "source", "cookTime", "preparationTime", "comments")])
  meta <- meta[!is.na(meta) & meta != ""]
  meta <- lapply(meta, trim)
  
  if (meta$name == toupper(meta$name)) {
    meta$name <- tolower(meta$name)
    substring(meta$name, 1, 1) <- toupper(substring(meta$name, 1, 1))
  }
  
  if (length(meta) > 0) {
    meta_yaml <- as.yaml(as.list(meta))
  } else {
    meta_yaml <- "\n"
  }
  
  out <- paste0(
    "---\n",
    meta_yaml,
    "---\n\n",
    trim(recipe$ingredients),
    "\n\n", 
    trim(recipe$method),
    "\n"
  )

  path <- paste0("recipes/", recipe$path, "/", recipe$slug, ".md")
  writeLines(out, path)
  if (!is.na(recipe$updated_on)) {
    system(paste("touch -t ", format(recipe$updated_on, "%Y%m%d%H%M.%S"), " ", path))  
  }
  
}

for(i in seq_len(nrow(recipes))) save_recipe(recipes[i, ])
