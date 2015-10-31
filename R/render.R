write_site <- function() {
  if (!file.exists("www"))
    dir.create("www")
  
  write_index()
  categories$path %>% walk(write_category)
}

write_index <- function() {
  body <- md_to_html(read_lines("recipes/index.md"))
  page <- render_page("All recipes", category_nav(), body)
  writeLines(page, "www/index.html")
}

write_category <- function(path) {
  title <- categories$category[categories$path == path]
  nav <- category_nav(path)
  body <- recipes_nav(path)
  
  page <- render_page(title, nav, body)
  
  mkdir(file.path("www", path))
  writeLines(page, file.path("www", path, "index.html"))
}

link <- function(href, text, current = "") {
  ifelse(href == current, 
    text,
    paste0("<a href='", href, "'>", text, "</a>")
  )
}
bullets <- function(href, text, current = "") {
  li <- paste0("<li>", link(href, text, current), "</li>")
  paste0(
    "<ul>\n",
    paste0("  ", li, "\n", collapse = ""), 
    "</ul>\n"
  )
}

category_nav <- function(current = "") {
  bullets(categories$path, categories$category, current)
}

recipes_nav <- function(category, current = "") {
  recipes <- recipes_list(category)
  bullets(recipes$html, recipes$name, current)
}

recipes_list <- function(category) {
  path <- file.path("recipes", category)
  stopifnot(file.exists(path))
  
  files <- dir(path, full.names = TRUE)
  
  names <- files %>% 
    map_chr(~read_lines(., n_max = 1)) %>% 
    unname() %>%
    stringr::str_replace("^# ", "")
  
  dplyr::data_frame(
    name = names, 
    md = basename(files), 
    html = str_replace(md, "\\.md$", ".html"),
    path = files
  ) %>%
    dplyr::arrange(name)
}

render_page <- function(title, navigation, body) {
  layout <- read_file("templates/layout.html")
  
  whisker.render(layout, list(
    title = htmltools::htmlEscape(title), 
    navigation = navigation, 
    body = body)
  )
}

