library(dplyr)
library(purrr)
library(stringr)
library(tibble)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

rag_coalesce <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

rag_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(default)
  }
  value <- as.character(x[[1]])
  if (!nzchar(str_squish(value))) {
    return(default)
  }
  value
}

rag_slug <- function(x, default = "unknown") {
  value <- rag_scalar(x, default)
  value %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "") %>%
    {\(z) ifelse(nzchar(z), z, default)}()
}

get_textbook_chapter_module_map <- function() {
  tibble::tribble(
    ~chapter, ~module_id,
    1L, "data_graphs",
    2L, "descriptive_stats",
    3L, "normal_distribution",
    4L, "scatterplots_correlation",
    5L, "regression",
    8L, "sampling",
    9L, "experiments",
    10L, "probability",
    11L, "sampling_distributions",
    13L, "binomial_distribution",
    14L, "confidence_intervals",
    15L, "hypothesis_testing",
    16L, "inference_in_practice",
    18L, "inference_mean",
    20L, "inference_proportion",
    21L, "two_proportions",
    23L, "chi_square",
    24L, "regression_inference"
  )
}

get_rag_module_table <- function() {
  tibble::tribble(
    ~module_id, ~module_label, ~module_order,
    "data_graphs", "Data and Graphs", 1L,
    "descriptive_stats", "Descriptive Statistics", 2L,
    "normal_distribution", "Normal Distribution", 3L,
    "scatterplots_correlation", "Scatterplots and Correlation", 4L,
    "regression", "Regression", 5L,
    "sampling", "Sampling", 6L,
    "experiments", "Experiments", 7L,
    "probability", "Probability", 8L,
    "sampling_distributions", "Sampling Distributions", 9L,
    "binomial_distribution", "Binomial Distribution", 10L,
    "confidence_intervals", "Confidence Intervals", 11L,
    "hypothesis_testing", "Hypothesis Testing", 12L,
    "inference_in_practice", "Inference in Practice", 13L,
    "inference_mean", "Inference for Means", 14L,
    "inference_proportion", "Inference for Proportions", 15L,
    "two_proportions", "Two Proportions", 16L,
    "chi_square", "Chi-Square", 17L,
    "regression_inference", "Regression Inference", 18L,
    "final_review", "Cumulative Review", 99L
  )
}

get_rag_module_choices <- function(include_auto = TRUE) {
  modules <- get_rag_module_table() %>%
    arrange(module_order)
  choices <- stats::setNames(modules$module_id, modules$module_label)
  if (isTRUE(include_auto)) {
    c("Auto from question" = "", choices)
  } else {
    choices
  }
}

topic_to_rag_module <- function(topic_id) {
  topic_id <- as.character(topic_id %||% NA_character_)
  recode(
    topic_id,
    data_graphs = "data_graphs",
    descriptive_stats = "descriptive_stats",
    relationships_regression = "regression",
    producing_data = "sampling",
    probability_basics = "probability",
    normal_dist = "normal_distribution",
    binomial_dist = "binomial_distribution",
    sampling_dist = "sampling_distributions",
    ci_prop = "confidence_intervals",
    ci_mean = "confidence_intervals",
    ht_foundations = "hypothesis_testing",
    ht_prop = "hypothesis_testing",
    ht_mean = "hypothesis_testing",
    uses_abuses_tests = "inference_in_practice",
    final_review = "final_review",
    .default = topic_id
  )
}

legacy_app_module_to_rag_module <- function(module_id, query = NULL) {
  module_id <- rag_scalar(module_id, default = NA_character_)
  inferred <- if (exists("route_question_to_module", mode = "function")) {
    tryCatch(route_question_to_module(query %||% ""), error = function(e) NA_character_)
  } else {
    NA_character_
  }
  if (!is.na(inferred) && nzchar(inferred)) {
    if (identical(module_id, "module_1") && inferred %in% c("data_graphs", "descriptive_stats")) return(inferred)
    if (identical(module_id, "module_2") && inferred %in% c("scatterplots_correlation", "regression")) return(inferred)
    if (identical(module_id, "module_3") && inferred %in% c("sampling", "experiments")) return(inferred)
    if (identical(module_id, "module_5") && inferred %in% c("normal_distribution", "binomial_distribution")) return(inferred)
    if (identical(module_id, "module_7") && inferred %in% c("confidence_intervals", "inference_mean", "inference_proportion")) return("confidence_intervals")
  }
  recode(
    module_id,
    module_1 = "data_graphs",
    module_2 = "regression",
    module_3 = "sampling",
    module_4 = "probability",
    module_5 = "normal_distribution",
    module_6 = "sampling_distributions",
    module_7 = "confidence_intervals",
    module_8 = "hypothesis_testing",
    module_9 = "inference_in_practice",
    cumulative_review = "final_review",
    .default = module_id
  )
}

normalize_rag_module_id <- function(module_id, query = NULL) {
  module_id <- rag_scalar(module_id, default = NA_character_)
  if (is.na(module_id) || !nzchar(module_id)) {
    return(NULL)
  }
  if (module_id %in% get_rag_module_table()$module_id) {
    return(module_id)
  }
  converted <- legacy_app_module_to_rag_module(module_id, query = query)
  if (!is.na(converted) && nzchar(converted)) converted else NULL
}

chunk_schema_fields <- function() {
  c(
    "chunk_id",
    "source_name",
    "source_type",
    "source_scope",
    "source_priority",
    "professor_id",
    "chapter",
    "section",
    "module_id",
    "topic_id",
    "concept_tag",
    "content_type",
    "page_number",
    "slide_number",
    "parent_id",
    "text",
    "normalized_text",
    "aliases_added",
    "image_refs",
    "display_permission_status"
  )
}

empty_chunk_table <- function() {
  tibble(
    chunk_id = character(),
    source_name = character(),
    source_type = character(),
    source_scope = character(),
    source_priority = numeric(),
    professor_id = character(),
    chapter = integer(),
    section = character(),
    module_id = character(),
    topic_id = character(),
    concept_tag = character(),
    content_type = character(),
    page_number = integer(),
    slide_number = integer(),
    parent_id = character(),
    text = character(),
    normalized_text = character(),
    aliases_added = character(),
    image_refs = character(),
    display_permission_status = character()
  )
}

default_source_priority <- function(source_type, source_scope = NULL, mode = "general", professor_id = NULL, selected_professor_id = NULL) {
  source_type <- as.character(source_type %||% "")
  source_scope <- as.character(source_scope %||% "")
  professor_id <- as.character(professor_id %||% "")
  selected_professor_id <- as.character(selected_professor_id %||% "")
  mode <- match.arg(mode, c("general", "professor"))

  case_when(
    mode == "professor" &&
      nzchar(selected_professor_id) &&
      source_type == "professor_notes" &&
      professor_id == selected_professor_id ~ 5,
    source_type == "textbook" ~ 10,
    source_type == "concept_page" ~ 15,
    source_type == "formula_sheet" ~ 35,
    source_type %in% c("image_caption", "practice_problem") ~ 40,
    source_type == "exam_material" ~ 45,
    source_type == "professor_notes" && source_scope == "professor_specific" ~ 60,
    source_scope == "supplemental" ~ 75,
    source_scope == "local_only_image" ~ 85,
    TRUE ~ 90
  )
}

make_chunk_id <- function(source_name, module_id, topic_id = NULL, parent_id = NULL, n = NULL) {
  base <- paste(
    rag_slug(source_name),
    rag_slug(module_id),
    rag_slug(topic_id %||% "general"),
    rag_slug(parent_id %||% "chunk"),
    sep = "__"
  )
  if (is.null(n)) {
    base
  } else {
    paste0(base, "__", sprintf("%04d", as.integer(n)))
  }
}

coerce_chunk_schema <- function(chunks) {
  if (is.null(chunks) || !is.data.frame(chunks) || nrow(chunks) == 0) {
    return(empty_chunk_table())
  }

  out <- tibble::as_tibble(chunks)
  for (field in chunk_schema_fields()) {
    if (!field %in% names(out)) {
      out[[field]] <- NA
    }
  }

  out <- out %>%
    mutate(
      text = as.character(text %||% ""),
      source_name = as.character(source_name %||% "unknown_source"),
      source_type = as.character(source_type %||% "textbook"),
      source_scope = as.character(source_scope %||% "universal_core"),
      professor_id = as.character(professor_id %||% NA_character_),
      chapter = suppressWarnings(as.integer(chapter)),
      section = as.character(section %||% NA_character_),
      module_id = as.character(module_id %||% NA_character_),
      topic_id = as.character(topic_id %||% module_id),
      concept_tag = as.character(concept_tag %||% topic_id),
      content_type = as.character(content_type %||% "concept_explanation"),
      page_number = suppressWarnings(as.integer(page_number)),
      slide_number = suppressWarnings(as.integer(slide_number)),
      parent_id = as.character(parent_id %||% NA_character_),
      image_refs = as.character(image_refs %||% ""),
      aliases_added = as.character(aliases_added %||% ""),
      display_permission_status = as.character(display_permission_status %||% "unknown"),
      source_priority = suppressWarnings(as.numeric(source_priority))
    )

  if (!"normalized_text" %in% names(chunks) || all(is.na(out$normalized_text))) {
    out$normalized_text <- if (exists("normalize_chunk_text", mode = "function")) {
      normalize_chunk_text(out$text)
    } else {
      str_to_lower(str_squish(out$text))
    }
  } else {
    out$normalized_text <- as.character(out$normalized_text)
  }

  missing_priority <- is.na(out$source_priority)
  if (any(missing_priority)) {
    out$source_priority[missing_priority] <- default_source_priority(
      out$source_type[missing_priority],
      out$source_scope[missing_priority],
      professor_id = out$professor_id[missing_priority]
    )
  }

  missing_id <- is.na(out$chunk_id) | !nzchar(as.character(out$chunk_id))
  if (any(missing_id)) {
    out$chunk_id[missing_id] <- pmap_chr(
      list(out$source_name[missing_id], out$module_id[missing_id], out$topic_id[missing_id], out$parent_id[missing_id], seq_len(sum(missing_id))),
      make_chunk_id
    )
  }

  out %>%
    select(all_of(chunk_schema_fields())) %>%
    distinct(chunk_id, .keep_all = TRUE)
}
