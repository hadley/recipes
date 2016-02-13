`%?%` <- function(x, y) if (length(x) > 0) y
`%||%` <- function(x, y) if (length(x) > 0) x else y

md_to_html <- function(input) {
  tmp_in <- tempfile()
  tmp_out <- tempfile()
  
  on.exit(unlink(c(tmp_in, tmp_out), force = TRUE))
  writeLines(input, tmp_in)
  
  rmarkdown::pandoc_convert(tmp_in, to = "html", from = "markdown", output = tmp_out)
  
  readr::read_file(tmp_out)
}

mkdir <- function(x, recursive = FALSE) {
  walk(x, dir.create, showWarnings = FALSE, recursive = recursive)
}
