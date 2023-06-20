#' Create a blog post
#'
#' Handles the boilerplate for a new blog post. Creates the directory and a
#' template index.qmd file.
#'
#' See `post_yaml()` for parameter descriptions
#'
#' @return Nothing, but opens index.qmd in a text editor (if the session is
#' interactive)
create_post <- function(title, ...) {
  stopifnot(is.character(title),
            length(title) == 1)
  # post directory name: prepend the date and convert the title to kebab-case
  title_kebab <- substr(str_to_kebab(title), 1, 30)
  post_basename <- stringr::str_glue("{Sys.Date()}-{title_kebab}")
  post_dir <- here::here("posts", post_basename)

  # Create post directory and populate it with index.qmd
  dir.create(post_dir)
  index_path <- file.path(post_dir, "index.qmd")
  file.create(index_path)
  writeLines(post_yaml(title, ...), index_path)
  if (interactive()) {
    file.edit(index_path)
  }
}

#' Convert string to kebab case
#'
#' Converts an arbitrary string to kebab case. E.g., "An input string" becomes
#' "an-input-string".
#'
#' Relies on `snakecase::to_snake_case()`, but replaces "_" with "-"
#'
#' @param s `[character]` A character vector of strings to be converted.
#'
#' @return A character vector of the strings in `s` in kebab-case
str_to_kebab <- function(s) {
  snake_s <- snakecase::to_snake_case(s)
  kebab_s <- gsub("_", "-", snake_s)
  kebab_s
}

#' Blog post YAML header
#'
#' Generates boilerplate YAML header
#'
#' @param title Post title (defaults to "")
#' @param description Post description (defaults to "")
#' @param author Post author (defaults to "Max Czapanskiy")
#' @param date Post date (defaults to current date)
#' @param image Path to post image, relative to post directory (defaults to "")
#' @param categories Vector of post categories (defaults to "")
#'
#' @return
post_yaml <- function(
    title = "",
    description = "",
    author = "Max Czapanskiy",
    date = Sys.Date(),
    image = "",
    categories = ""
) {
  stringr::str_glue("
---
title: {title}
description: {description}
author:
  - name: {author}
format: html
date: {date}
image: {image}
categories: [{paste(categories, collapse = ', ')}]
---
  ")
}
