library(dplyr)
library(fs)
library(purrr)
library(stringr)
library(tibble)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

if (!exists("get_textbook_chapter_module_map", mode = "function") && file.exists("R/chunk_schema.R")) {
  source("R/chunk_schema.R")
}
if (!exists("normalize_chunk_text", mode = "function") && file.exists("R/aliases.R")) {
  source("R/aliases.R")
}

textbook_safe_text <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\r\\n?", "\n") %>%
    str_replace_all("[ \t]+", " ") %>%
    str_squish()
}

ingest_textbook_pdf <- function(path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("The pdftools package is required to ingest textbook PDFs.", call. = FALSE)
  }
  if (!fs::file_exists(path)) {
    stop("Textbook PDF not found: ", path, call. = FALSE)
  }

  page_text <- pdftools::pdf_text(path)
  raw_pages <- tibble(
    page_number = seq_along(page_text),
    text = page_text
  )

  chunks <- chunk_textbook_sections(raw_pages)
  tagged <- tag_textbook_chunks(chunks)
  concepts <- create_textbook_concept_pages(tagged)

  list(
    raw_pages = raw_pages,
    chapters = detect_textbook_chapters(raw_pages),
    chunks = tagged,
    concept_pages = concepts
  )
}

detect_textbook_chapters <- function(text) {
  pages <- coerce_text_pages(text)
  matches <- pages %>%
    mutate(
      chapter_match = str_match(text, regex("\\bchapter\\s+([0-9]{1,2})\\b", ignore_case = TRUE))[, 2],
      chapter = suppressWarnings(as.integer(chapter_match))
    ) %>%
    filter(!is.na(chapter)) %>%
    group_by(chapter) %>%
    summarise(first_page = min(page_number), .groups = "drop") %>%
    arrange(chapter)

  if (nrow(matches) == 0) {
    # TODO: Tune this once the exact textbook PDF layout is reviewed. Some scans omit
    # machine-readable chapter headings, so this safe first version falls back to
    # page-level chunking instead of guessing.
    return(tibble(chapter = integer(), first_page = integer()))
  }

  matches
}

coerce_text_pages <- function(text) {
  if (is.data.frame(text) && all(c("page_number", "text") %in% names(text))) {
    return(text %>% mutate(page_number = as.integer(page_number), text = as.character(text)))
  }
  if (is.character(text) && length(text) > 1) {
    return(tibble(page_number = seq_along(text), text = text))
  }
  tibble(page_number = 1L, text = paste(text %||% "", collapse = "\n\n"))
}

infer_page_chapter <- function(pages, chapter_starts) {
  if (nrow(chapter_starts) == 0) {
    return(rep(NA_integer_, nrow(pages)))
  }
  vapply(pages$page_number, function(page) {
    prior <- chapter_starts %>% filter(first_page <= !!page) %>% arrange(desc(first_page)) %>% slice_head(n = 1)
    if (nrow(prior) == 0) NA_integer_ else prior$chapter[[1]]
  }, FUN.VALUE = integer(1))
}

extract_section_heading <- function(text) {
  lines <- str_split(text %||% "", "\n")[[1]]
  lines <- str_squish(lines)
  lines <- lines[nzchar(lines)]
  section_line <- lines[str_detect(lines, regex("^(section\\s+)?[0-9]{1,2}(\\.[0-9]+)+\\s+", ignore_case = TRUE))]
  if (length(section_line) > 0) {
    return(section_line[[1]])
  }
  heading <- lines[str_detect(lines, regex("^[A-Z][A-Za-z0-9 ,:;\\-]{8,80}$"))]
  if (length(heading) > 0) heading[[1]] else NA_character_
}

split_textbook_page <- function(text, max_chars = 1800L) {
  clean <- str_replace_all(text %||% "", "\\r\\n?", "\n")
  paragraphs <- str_split(clean, "\\n\\s*\\n+")[[1]] %>%
    str_squish()
  paragraphs <- paragraphs[nzchar(paragraphs)]
  if (length(paragraphs) == 0) {
    return(character())
  }

  chunks <- character()
  current <- ""
  for (paragraph in paragraphs) {
    proposed <- str_squish(paste(current, paragraph))
    if (nchar(proposed) > max_chars && nzchar(current)) {
      chunks <- c(chunks, current)
      current <- paragraph
    } else {
      current <- proposed
    }
  }
  if (nzchar(current)) {
    chunks <- c(chunks, current)
  }
  chunks
}

chunk_textbook_sections <- function(text) {
  pages <- coerce_text_pages(text)
  chapter_starts <- detect_textbook_chapters(pages)
  pages$chapter <- infer_page_chapter(pages, chapter_starts)
  chapter_map <- get_textbook_chapter_module_map()

  page_chunks <- pages %>%
    mutate(section = map_chr(text, extract_section_heading)) %>%
    mutate(chunks = map(text, split_textbook_page)) %>%
    tidyr::unnest_longer(chunks, values_to = "text") %>%
    group_by(page_number) %>%
    mutate(page_chunk_number = row_number()) %>%
    ungroup() %>%
    left_join(chapter_map, by = "chapter") %>%
    mutate(
      module_id = module_id %||% NA_character_,
      source_name = "course_textbook",
      source_type = "textbook",
      source_scope = "universal_core",
      source_priority = default_source_priority("textbook", "universal_core"),
      professor_id = NA_character_,
      topic_id = module_id,
      parent_id = if_else(!is.na(chapter), paste0("chapter_", chapter), NA_character_),
      display_permission_status = "local_only",
      chunk_id = make_chunk_id(source_name, module_id %||% "unknown_module", topic_id %||% "unknown_topic", parent_id %||% paste0("page_", page_number), page_chunk_number)
    )

  coerce_chunk_schema(page_chunks)
}

infer_textbook_content_type <- function(text) {
  clean <- str_to_lower(text %||% "")
  case_when(
    str_detect(clean, "\\bdefinition\\b|\\bdefined as\\b|\\bis called\\b") ~ "definition",
    str_detect(clean, "\\bexample\\b|\\bworked example\\b") ~ "worked_example",
    str_detect(clean, "\\bapply your knowledge\\b") ~ "apply_your_knowledge",
    str_detect(clean, "\\bcheck your skills\\b") ~ "check_your_skills",
    str_detect(clean, "\\bsummary\\b|\\bchapter review\\b") ~ "chapter_summary",
    str_detect(clean, "\\blink it\\b") ~ "link_it",
    str_detect(clean, "\\bformula\\b|\\bequation\\b") ~ "formula",
    str_detect(clean, "\\bfigure\\s+[0-9]") ~ "figure_caption",
    str_detect(clean, "\\bexercise\\b|\\bproblem\\b") ~ "practice_question",
    TRUE ~ "concept_explanation"
  )
}

infer_textbook_concept_tag <- function(text, module_id = NULL) {
  clean <- normalize_chunk_text(text %||% "")
  case_when(
    str_detect(clean, "p_hat|sample proportion|population proportion") ~ "sample_proportion",
    str_detect(clean, "p_0|null proportion|hypothesized proportion") ~ "null_proportion",
    str_detect(clean, "x_bar|sample mean") ~ "sample_mean",
    str_detect(clean, "mu_0|population mean|hypothesized mean") ~ "population_mean",
    str_detect(clean, "p_value|reject|null hypothesis|alternative hypothesis") ~ "hypothesis_test_decision",
    str_detect(clean, "confidence interval|margin of error") ~ "confidence_interval",
    str_detect(clean, "standard error") ~ "standard_error",
    str_detect(clean, "normal distribution|z_score|z star|z_star") ~ "normal_distribution",
    str_detect(clean, "binomial") ~ "binomial_distribution",
    TRUE ~ module_id %||% "general_concept"
  )
}

tag_textbook_chunks <- function(chunks) {
  coerce_chunk_schema(chunks) %>%
    mutate(
      normalized_text = normalize_chunk_text(text),
      content_type = map_chr(text, infer_textbook_content_type),
      concept_tag = map2_chr(text, module_id, infer_textbook_concept_tag),
      topic_id = if_else(is.na(topic_id) | !nzchar(topic_id), concept_tag, topic_id),
      aliases_added = map_chr(text, ~ apply_alias_replacements(.x, return_aliases = TRUE)$aliases_added)
    ) %>%
    coerce_chunk_schema()
}

create_textbook_concept_pages <- function(chunks) {
  chunks <- coerce_chunk_schema(chunks)
  if (nrow(chunks) == 0) {
    return(empty_chunk_table())
  }

  concepts <- chunks %>%
    filter(source_type == "textbook", !is.na(module_id), nzchar(module_id)) %>%
    group_by(module_id, topic_id, concept_tag) %>%
    summarise(
      text = paste(head(text, 4), collapse = "\n\n"),
      chapter = suppressWarnings(min(chapter, na.rm = TRUE)),
      section = first(na.omit(section)) %||% NA_character_,
      parent_id = first(parent_id) %||% NA_character_,
      .groups = "drop"
    ) %>%
    mutate(
      source_name = "textbook_concept_page",
      source_type = "concept_page",
      source_scope = "universal_core",
      source_priority = default_source_priority("concept_page", "universal_core"),
      professor_id = NA_character_,
      content_type = "concept_explanation",
      page_number = NA_integer_,
      slide_number = NA_integer_,
      normalized_text = normalize_chunk_text(text),
      aliases_added = map_chr(text, ~ apply_alias_replacements(.x, return_aliases = TRUE)$aliases_added),
      image_refs = "",
      display_permission_status = "local_only",
      chunk_id = make_chunk_id(source_name, module_id, topic_id, concept_tag)
    )

  concepts$chapter[is.infinite(concepts$chapter)] <- NA_integer_
  coerce_chunk_schema(concepts)
}
