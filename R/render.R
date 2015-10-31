write_site <- function() {
  if (!file.exists("www"))
    dir.create("www")
  
  write_index()
}

write_index <- function() {
  body <- md_to_html(read_lines("recipes/index.md"))
  page <- render_page("All recipes", category_nav(), body)
  writeLines(page, "www/index.html")
}

category_nav <- function(cur = NULL) {
  bullets <- paste0(
    "<li><a href='/", categories$path, "'>", categories$category, "</a></li>"
  )

  if (!is.null(cur)) {
    cur <- cur == categories$path
    bullets[cur] <- paste0("<li>", categories$category[cur], "</li>")
  }
  
  paste0(
    "<ul>\n",
    paste0("  ", bullets, "\n", collapse = ""),
    "</ul>\n"
  )
}


render_page <- function(title, navigation, body) {
  layout <- read_file("templates/layout.html")
  
  whisker.render(layout, list(
    title = htmltools::htmlEscape(title), 
    navigation = navigation, 
    body = body)
  )
}

