library(fs)
library(readr)
library(stringr)
library(tibble)
library(purrr)
library(markdown)

wiki_dir <- "data/wiki/concept_pages"

strip_front_matter <- function(x) {
  if (length(x) == 0) return("")
  txt <- paste(x, collapse = "\n")
  
  if (str_starts(txt, "---")) {
    txt <- str_replace(txt, "^---\\s*\\n[\\s\\S]*?\\n---\\s*\\n", "")
  }
  
  txt
}

# Simple front-matter parser.
# This is more forgiving than yaml::yaml.load() for labels with colons.
extract_front_matter <- function(x) {
  txt <- paste(x, collapse = "\n")
  
  if (!str_starts(txt, "---")) {
    return(list())
  }
  
  fm <- str_match(txt, "^---\\s*\\n([\\s\\S]*?)\\n---")[, 2]
  
  if (is.na(fm)) {
    return(list())
  }
  
  lines <- str_split(fm, "\n")[[1]]
  out <- list()
  
  for (line in lines) {
    if (!str_detect(line, ":")) next
    
    parts <- str_split_fixed(line, ":", 2)
    key <- str_trim(parts[, 1])
    value <- str_trim(parts[, 2])
    
    if (identical(key, "") || identical(value, "")) next
    value <- str_remove_all(value, '^"|"$')
    
    out[[key]] <- value
  }
  
  out
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

normalize_wiki_topic_id <- function(topic_id) {
  if (is.null(topic_id) || length(topic_id) != 1 || all(is.na(topic_id))) {
    return(NULL)
  }
  
  topic_value <- str_squish(as.character(topic_id[[1]]))
  if (!nzchar(topic_value)) {
    return(NULL)
  }
  
  topic_value
}

prep_math_for_html <- function(x) {
  x %>%
    str_replace_all("\\\\\\[\\s*", "$$") %>%
    str_replace_all("\\s*\\\\\\]", "$$") %>%
    str_replace_all("\\\\\\(\\s*", "$") %>%
    str_replace_all("\\s*\\\\\\)", "$")
}

load_concept_pages <- function(path = wiki_dir) {
  files <- fs::dir_ls(path, glob = "*.md", recurse = FALSE) %>%
    purrr::discard(~ fs::path_file(.x) == "README.md")
  purrr::map_dfr(files, function(file) {
    lines <- readr::read_lines(file)
    fm <- extract_front_matter(lines)
    body <- strip_front_matter(lines)
    
    fallback_topic <- fs::path_ext_remove(fs::path_file(file))
    topic_id <- fm$topic_id %||% fallback_topic
    student_label <- fm$student_label %||% topic_id
    
    tibble(
      file_path = as.character(file),
      file_name = fs::path_file(file),
      topic_id = topic_id,
      student_label = student_label,
      status = fm$status %||% "unknown",
      markdown_body = body,
      html_body = markdown::markdownToHTML(
        text = prep_math_for_html(body),
        fragment.only = TRUE
      ))
  }) %>%
    arrange(student_label)
}

get_concept_page <- function(topic_id, path = wiki_dir) {
  topic_id <- normalize_wiki_topic_id(topic_id)
  if (is.null(topic_id)) {
    return(NULL)
  }
  
  pages <- load_concept_pages(path)
  if (!is.data.frame(pages) || nrow(pages) == 0) {
    return(NULL)
  }
  
  page <- pages %>% filter(topic_id == !!topic_id)
  
  if (nrow(page) == 0) {
    return(NULL)
  }
  
  page[1, ]
}
