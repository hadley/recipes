#' Create the complete site.
#' 
#' @export
write_site <- function(path = "www") {
  mkdir(path)

  copy_static("orchid.gif", path)
  copy_static("recipes.css", path)
  write_index(path)
  
  recipes <- load_recipes()
  
  categories$category_id %>% walk(write_category, recipes, path = path)
  list(
    name = recipes$name, 
    cat_id = recipes$category_id, 
    recipe = recipes$recipe,
    dest = recipes$dest
  ) %>% 
    pwalk(write_recipe, recipes = recipes, path = path)

}

write_index <- function(path = "www") {
  body <- md_to_html(render_template("index.md"))
  page <- render_page("All recipes", category_nav(), body)
  writeLines(page, file.path(path, "index.html"))
}

write_category <- function(cat_id, recipes, path = "www") {
  cat <- find_category(cat_id)
  
  title <- cat$title
  body <- recipes_nav(cat_id, recipes)
  nav <- category_nav(cat_id)
  
  page <- render_page(title, nav, body)

  mkdir(file.path(path, cat$path))
  writeLines(page, file.path(path, cat$path, "index.html"))
}

write_recipe <- function(name, cat_id, recipe, dest, recipes = load_recipes(), path = "www") {
  
  title <- name
  nav <- paste(category_nav(cat_id), recipes_nav(cat_id, recipes, dest))
  body <- md_to_html(recipe)
    
  page <- render_template("layout.html", list(
    title = htmltools::htmlEscape(title), 
    navigation = nav, 
    body = body)
  )
  
  writeLines(page, file.path(path, dest))
}


link <- function(href, text, current = "") {
  if (is.null(current)) {
    current <- ""
  }
  
  ifelse(href == current, 
    paste0("<a href='", href, "' class='current'>", text, "</a>"),
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
  paste0(
    "<div class='nav'>\n",
    "<h2>Categories</h2>\n\n",
    bullets(
      paste0("/", categories$path, "/index.html"), 
      categories$category, 
      find_category(cat_id)$path
    ),
    "</div>"
  )
}

recipes_nav <- function(cat_id, recipes, current = "") {
  recipes <- dplyr::filter(recipes, category_id == cat_id)
  paste0(
    "<div class='nav'>\n",
    "<h2>Recipes</h2>\n\n",
    bullets(
      paste0("/", recipes$dest), 
      recipes$name, 
      paste0("/", current)
    ),
    "</div>"
  )
}

render_page <- function(title, navigation, body) {
  render_template("layout.html", list(
    title = htmltools::htmlEscape(title), 
    navigation = navigation, 
    body = body)
  )
}

render_recipe <- function(path, navigation, body) {
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

copy_static <- function(src, dest) {
  from <- system.file("static", src, package = "recipes")
  file.copy(from, dest, overwrite = TRUE)
}


find_category <- function(cat_id) {
  if (is.null(cat_id)) return(NULL)
  
  as.list(categories[categories$category_id == cat_id, , drop = FALSE])
}


load_recipes <- function() {
  paths <- dir("recipes", recursive = TRUE)
  recipes <- paths %>% map_chr(~ readr::read_file(file.path("recipes", .)))
  
  names <- recipes %>% 
    stringr::str_split(stringr::fixed("\n"), n = 2) %>% 
    map_chr(1) %>% 
    stringr::str_replace("^# ", "")

  dplyr::data_frame(
    name = names,
    category = dirname(paths),
    src = paths, 
    dest = stringr::str_replace(paths, "\\.md$", ".html"),
    recipe = recipes
  ) %>%
    dplyr::arrange(name) %>% 
    dplyr::inner_join(dplyr::select(categories, category_id, category = path), by = "category")
}
