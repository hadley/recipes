
md_to_html <- function(input) {
  tmp_in <- tempfile()
  tmp_out <- tempfile()
  
  on.exit(unlink(c(tmp_in, tmp_out), force = TRUE))
  writeLines(input, tmp_in)
  
  pandoc_convert(tmp_in, to = "html", from = "markdown", output = tmp_out)
  
  read_lines(tmp_out)
}

render_page <- function(title, navigation, body) {
  layout <- read_file("templates/layout.html")
  
  whisker.render(layout, list(
    title = htmltools::htmlEscape(title), 
    navigation = navigation, 
    body = body)
  )
}

write_index <- function() {
  body <- md_to_html(read_lines("recipes/index.md"))
  page <- render_page("All recipes", "", body)
  writeLines(page, "www/index.html")
}

write_site <- function() {
  if (!file.exists("www"))
    dir.create("www")
  
  write_index()
}
