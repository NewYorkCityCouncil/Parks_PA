caption_template <- function(header, body) {
  paste(header, body, sep = "<hr>") %>%
    councilPopup()
}


header_template <- function(name, ...) {
  dots <- list(..., sep = "<br>")
  dots <- dots[!is.na(dots)]
  dots <- do.call(paste, dots)

  # browser()
  paste(tags$h4(name), "<small><em>", dots, "</em></small>")
}

body_template <- function(...) {
  dots <- list(...)
  # browser()
  dots <- dots[!is.na(dots)]


  labels <- names(dots)

  reduce2(labels, dots, function(old, x, y) paste0(old,  tags$strong(x), ": ", y, "<br>"), .init = "")
}


# caption_template(header_template("Test", "123 Main St", "212 555 1234"), body_template(test = "Yes", fail = "No"))
