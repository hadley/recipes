#' Create the complete site.
#' 
#' @export
write_site <- function(path = "www") {
  mkdir(path)

  write_index(path)
  categories$category_id %>% walk(write_category, path = path)
  file.copy(system.file("public", "orchid.gif", package = "recipes"), path, overwrite = TRUE)
  file.copy(system.file("public", "scaffold.css", package = "recipes"), path, overwrite = TRUE)
}

write_index <- function(path = "www") {
  body <- md_to_html(render_template("index.md"))
  page <- render_page("All recipes", category_nav(), body)
  writeLines(page, file.path(path, "index.html"))
}

write_category <- function(cat_id, path = "www") {
  cat <- find_category(cat_id)
  
  title <- cat$title
  nav <- category_nav(cat_id)
  body <- recipes_nav(cat_id)
  
  page <- render_page(title, nav, body)

  mkdir(file.path(path, cat$path))
  writeLines(page, file.path(path, cat$path, "index.html"))
}


link <- function(href, text, current = "") {
  if (is.null(current)) {
    current <- ""
  }
  
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

category_nav <- function(cat_id = NULL) {
  bullets(paste0("/", categories$path, "/index.html"), categories$category, find_category(cat_id)$path)
}

recipes_nav <- function(cat_id, current = "") {
  recipes <- recipes_list(cat_id)
  bullets(recipes$html, recipes$name, current)
}

recipes_list <- function(cat_id) {
  path <- file.path("recipes", find_category(cat_id)$path)
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
  render_template("layout.html", list(
    title = htmltools::htmlEscape(title), 
    navigation = navigation, 
    body = body)
  )
}

render_template <- function(name, data = list()) {
  path <- system.file("templates", name, package = "recipes", mustWork = TRUE)
  template <- readLines(path)
  
  whisker.render(template, data)
}


find_category <- function(cat_id) {
  if (is.null(cat_id)) return(NULL)
  
  as.list(categories[categories$category_id == cat_id, , drop = FALSE])
}
