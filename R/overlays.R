library(dplyr)
library(fs)
library(purrr)
library(readr)
library(stringr)
library(tibble)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

if (!exists("coerce_chunk_schema", mode = "function") && file.exists("R/chunk_schema.R")) {
  source("R/chunk_schema.R")
}
if (!exists("normalize_chunk_text", mode = "function") && file.exists("R/aliases.R")) {
  source("R/aliases.R")
}

source_name_to_scope <- function(source_name) {
  case_when(
    source_name == "current_professor" ~ "professor_specific",
    source_name == "dr_south" ~ "supplemental",
    source_name == "exam_materials" ~ "supplemental",
    TRUE ~ "supplemental"
  )
}

doc_type_to_source_type <- function(doc_type, source_name = NULL) {
  case_when(
    doc_type == "formula_sheet" ~ "formula_sheet",
    doc_type %in% c("practice_problem", "review") ~ "practice_problem",
    doc_type == "lecture_note" ~ "professor_notes",
    source_name == "exam_materials" ~ "exam_material",
    TRUE ~ "professor_notes"
  )
}

doc_type_to_content_type <- function(doc_type, text = NULL) {
  clean <- str_to_lower(text %||% "")
  case_when(
    doc_type == "formula_sheet" ~ "formula",
    str_detect(clean, "\\bexample\\b") ~ "worked_example",
    doc_type == "practice_problem" ~ "practice_question",
    doc_type == "review" ~ "chapter_summary",
    TRUE ~ "concept_explanation"
  )
}

read_overlay_text <- function(evidence_file = NULL, extracted_text_path = NULL) {
  path <- if (!is.na(evidence_file %||% NA_character_) && nzchar(evidence_file %||% "")) evidence_file else extracted_text_path
  if (is.null(path) || is.na(path) || !nzchar(path) || !fs::file_exists(path)) {
    return("")
  }
  paste(readr::read_lines(path, progress = FALSE), collapse = "\n")
}

professor_id_from_source <- function(source_name) {
  case_when(
    source_name == "current_professor" ~ "current_professor",
    source_name == "dr_south" ~ "supplemental_overlay",
    TRUE ~ NA_character_
  )
}

ingest_professor_materials <- function(index_path = "data/processed/topic_evidence_index.csv") {
  if (!fs::file_exists(index_path)) {
    return(empty_chunk_table())
  }

  index <- suppressMessages(readr::read_csv(index_path, show_col_types = FALSE))
  required <- c("topic_id", "source_name", "doc_type")
  if (!all(required %in% names(index))) {
    warning("Topic evidence index exists but is missing required columns.", call. = FALSE)
    return(empty_chunk_table())
  }

  overlays <- index %>%
    mutate(
      text = pmap_chr(
        list(evidence_file %||% NA_character_, extracted_text_path %||% NA_character_),
        read_overlay_text
      ),
      module_id = topic_to_rag_module(topic_id),
      concept_tag = topic_id,
      source_type = map2_chr(doc_type, source_name, doc_type_to_source_type),
      source_scope = map_chr(source_name, source_name_to_scope),
      professor_id = map_chr(source_name, professor_id_from_source),
      source_priority = default_source_priority(source_type, source_scope, professor_id = professor_id),
      content_type = map2_chr(doc_type, text, doc_type_to_content_type),
      chapter = NA_integer_,
      section = NA_character_,
      page_number = NA_integer_,
      slide_number = NA_integer_,
      parent_id = topic_id,
      normalized_text = normalize_chunk_text(text),
      aliases_added = map_chr(text, ~ apply_alias_replacements(.x, return_aliases = TRUE)$aliases_added),
      image_refs = "",
      display_permission_status = "local_only",
      chunk_id = pmap_chr(
        list(source_name, module_id, topic_id, file_name %||% source_name, row_number()),
        make_chunk_id
      )
    )

  coerce_chunk_schema(overlays)
}

attach_professor_overlay <- function(textbook_chunks, overlays = NULL, professor_id = NULL) {
  textbook_chunks <- coerce_chunk_schema(textbook_chunks)
  overlays <- overlays %||% ingest_professor_materials()
  overlays <- coerce_chunk_schema(overlays)

  if (!is.null(professor_id) && nzchar(professor_id)) {
    overlays <- overlays %>%
      filter(is.na(.data$professor_id) | .data$professor_id == professor_id | .data$source_scope == "supplemental")
  }

  bind_rows(
    textbook_chunks,
    overlays %>%
      mutate(
        parent_id = if_else(
          is.na(parent_id) | !nzchar(parent_id),
          paste(module_id, concept_tag, sep = "__"),
          parent_id
        )
      )
  ) %>%
    distinct(chunk_id, .keep_all = TRUE) %>%
    coerce_chunk_schema()
}

get_active_source_policy <- function(mode = c("general", "professor"), professor_id = NULL) {
  mode <- match.arg(mode)
  professor_id <- professor_id %||% "current_professor"

  if (identical(mode, "professor")) {
    return(list(
      mode = mode,
      professor_id = professor_id,
      hierarchy = c(
        "selected_professor_overlay_active_module",
        "textbook_active_module",
        "selected_professor_examples_active_module",
        "related_textbook_modules",
        "supplemental_professor_notes"
      ),
      source_boosts = tibble::tribble(
        ~source_type, ~source_scope, ~boost,
        "professor_notes", "professor_specific", 4.0,
        "textbook", "universal_core", 1.7,
        "concept_page", "universal_core", 1.5,
        "practice_problem", "professor_specific", 2.5,
        "formula_sheet", "supplemental", 0.8,
        "professor_notes", "supplemental", -0.3
      )
    ))
  }

  list(
    mode = mode,
    professor_id = professor_id,
    hierarchy = c(
      "textbook_active_module",
      "textbook_concept_pages_active_module",
      "textbook_examples_figures_practice_active_module",
      "related_textbook_modules",
      "professor_notes_secondary"
    ),
    source_boosts = tibble::tribble(
      ~source_type, ~source_scope, ~boost,
      "textbook", "universal_core", 2.0,
      "concept_page", "universal_core", 1.8,
      "practice_problem", "universal_core", 1.2,
      "image_caption", "universal_core", 1.0,
      "formula_sheet", "supplemental", 0.7,
      "professor_notes", "professor_specific", 0.2,
      "professor_notes", "supplemental", -0.4
    )
  )
}
