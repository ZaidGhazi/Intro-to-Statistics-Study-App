# Question-bank audit helpers for the Introduction to Statistics Study App.
# These functions summarize coverage, metadata completeness, visuals, and answer-option quality.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

load_question_bank_for_audit <- function(path = "data/processed/question_bank.csv") {
  if (!file.exists(path)) {
    stop("Question bank not found at ", path, call. = FALSE)
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("The readr package is required for audit_question_bank.R", call. = FALSE)
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

has_text_value <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

canonical_option_text <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  gsub("\\s+", " ", x)
}

parse_choice_objects_for_audit <- function(choices_json) {
  if (is.null(choices_json) || length(choices_json) == 0 || is.na(choices_json) || !nzchar(trimws(as.character(choices_json)))) {
    return(list(error = "missing_choices", choices = list()))
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(error = "jsonlite_unavailable", choices = list()))
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(as.character(choices_json), simplifyVector = FALSE),
    error = function(e) e
  )
  if (inherits(parsed, "error") || !is.list(parsed)) {
    return(list(error = "malformed_choices_json", choices = list()))
  }
  list(error = NA_character_, choices = parsed)
}

choice_texts_for_audit <- function(choices) {
  if (length(choices) == 0) return(character())
  vapply(choices, function(choice) as.character(choice$text %||% ""), character(1))
}

choice_ids_for_audit <- function(choices) {
  if (length(choices) == 0) return(character())
  vapply(choices, function(choice) as.character(choice$id %||% ""), character(1))
}

has_any_visual_field <- function(question_bank) {
  visual_cols <- intersect(c("visual_id", "visual_ids", "visual_template_id", "tutor_visual_ids"), names(question_bank))
  if (length(visual_cols) == 0) {
    return(rep(FALSE, nrow(question_bank)))
  }
  Reduce(`|`, lapply(visual_cols, function(col) has_text_value(question_bank[[col]])))
}

audit_answer_options <- function(question_bank = load_question_bank_for_audit()) {
  rows <- lapply(seq_len(nrow(question_bank)), function(i) {
    row <- question_bank[i, , drop = FALSE]
    parsed <- parse_choice_objects_for_audit(row$choices[[1]] %||% NA_character_)
    choices <- parsed$choices
    ids <- choice_ids_for_audit(choices)
    texts <- choice_texts_for_audit(choices)
    clean_texts <- canonical_option_text(texts)
    correct_id <- as.character(row$correct_choice_id[[1]] %||% "")
    correct_answer <- as.character(row$correct_answer[[1]] %||% "")
    correct_index <- match(correct_id, ids)
    correct_option_text <- if (!is.na(correct_index) && length(texts) >= correct_index) texts[[correct_index]] else NA_character_
    issues <- character()

    if (!is.na(parsed$error)) issues <- c(issues, parsed$error)
    if (length(choices) == 0) issues <- c(issues, "no_answer_options")
    if (any(!nzchar(trimws(texts)))) issues <- c(issues, "blank_option_text")
    if (any(!nzchar(trimws(ids)))) issues <- c(issues, "blank_option_id")
    if (length(ids) != length(unique(ids))) issues <- c(issues, "duplicate_option_id")
    if (length(clean_texts) != length(unique(clean_texts))) issues <- c(issues, "duplicate_option_text")
    if (!nzchar(correct_id) || !correct_id %in% ids) issues <- c(issues, "correct_choice_id_not_in_options")
    if (length(choices) < 4) issues <- c(issues, "fewer_than_four_options")
    if (length(choices) > 6) issues <- c(issues, "more_than_six_options")
    if (has_text_value(correct_answer) && !any(canonical_option_text(correct_answer) == clean_texts)) {
      issues <- c(issues, "correct_answer_text_not_in_options")
    }
    if (any(grepl("take the same core idea|scenario|new setting|developer|debug|metadata|concept page", texts, ignore.case = TRUE))) {
      issues <- c(issues, "student_facing_placeholder_or_internal_text")
    }
    if (any(nchar(texts) > 160)) issues <- c(issues, "very_long_option_text")

    tibble::tibble(
      question_id = as.character(row$question_id[[1]] %||% NA_character_),
      module_id = as.character(row$module_id[[1]] %||% NA_character_),
      topic_id = as.character(row$topic_id[[1]] %||% NA_character_),
      concept_tag = as.character(row$concept_tag[[1]] %||% NA_character_),
      n_options = length(choices),
      correct_choice_id = correct_id,
      correct_option_text = correct_option_text,
      duplicate_option_text = length(clean_texts) != length(unique(clean_texts)),
      duplicate_option_id = length(ids) != length(unique(ids)),
      correct_choice_id_not_in_options = !nzchar(correct_id) || !correct_id %in% ids,
      correct_answer_text_not_in_options = has_text_value(correct_answer) && !any(canonical_option_text(correct_answer) == clean_texts),
      issue_count = length(unique(issues)),
      issues = paste(unique(issues), collapse = "; "),
      option_texts = paste(texts, collapse = " | ")
    )
  })
  dplyr::bind_rows(rows)
}


feedback_explanation_issues <- function(question_bank = load_question_bank_for_audit()) {
  rows <- lapply(seq_len(nrow(question_bank)), function(i) {
    row <- question_bank[i, , drop = FALSE]
    explanation <- as.character(row$explanation[[1]] %||% "")
    solution_explanation <- as.character(row$solution_explanation[[1]] %||% "")
    correct_answer <- as.character(row$correct_answer[[1]] %||% "")
    explanation_clean <- canonical_option_text(explanation)
    solution_clean <- canonical_option_text(solution_explanation)
    correct_clean <- canonical_option_text(correct_answer)
    issues <- character()

    if (!has_text_value(explanation)) issues <- c(issues, "missing_explanation")
    if (has_text_value(explanation) && nchar(trimws(explanation)) < 80) issues <- c(issues, "explanation_too_short")
    if (has_text_value(correct_answer) && explanation_clean == correct_clean) issues <- c(issues, "explanation_repeats_correct_answer")
    if (!has_text_value(solution_explanation)) issues <- c(issues, "missing_solution_explanation")
    if (has_text_value(solution_explanation) && nchar(trimws(solution_explanation)) < 80) issues <- c(issues, "solution_explanation_too_short")
    if (has_text_value(correct_answer) && solution_clean == correct_clean) issues <- c(issues, "solution_explanation_repeats_correct_answer")
    if (grepl("take the same core idea|scenario|new setting|developer|debug|metadata|concept page", explanation, ignore.case = TRUE)) {
      issues <- c(issues, "student_facing_placeholder_or_internal_text")
    }

    tibble::tibble(
      question_id = as.character(row$question_id[[1]] %||% NA_character_),
      module_id = as.character(row$module_id[[1]] %||% NA_character_),
      topic_id = as.character(row$topic_id[[1]] %||% NA_character_),
      concept_tag = as.character(row$concept_tag[[1]] %||% NA_character_),
      explanation_chars = nchar(explanation),
      solution_explanation_chars = nchar(solution_explanation),
      issue_count = length(unique(issues)),
      issues = paste(unique(issues), collapse = "; ")
    )
  })
  dplyr::bind_rows(rows)
}

audit_question_bank <- function(question_bank = load_question_bank_for_audit()) {
  required <- c(
    "question_id", "module_id", "topic_id", "concept_tag", "question_text",
    "format", "choices", "correct_choice_id", "correct_answer", "hint", "explanation"
  )
  missing_required <- setdiff(required, names(question_bank))
  question_bank$has_visual <- has_any_visual_field(question_bank)
  question_bank$has_hint_1 <- if ("hint_1" %in% names(question_bank)) has_text_value(question_bank$hint_1) else FALSE
  question_bank$has_concept_explanation <- if ("concept_explanation" %in% names(question_bank)) has_text_value(question_bank$concept_explanation) else FALSE
  question_bank$has_solution_explanation <- if ("solution_explanation" %in% names(question_bank)) has_text_value(question_bank$solution_explanation) else FALSE
  question_bank$question_family_safe <- if ("question_family" %in% names(question_bank)) {
    ifelse(has_text_value(question_bank$question_family), as.character(question_bank$question_family), if ("concept_tag" %in% names(question_bank)) as.character(question_bank$concept_tag) else rep("missing_family", nrow(question_bank)))
  } else {
    if ("concept_tag" %in% names(question_bank)) as.character(question_bank$concept_tag) else rep("missing_family", nrow(question_bank))
  }

  option_audit <- audit_answer_options(question_bank)
  explanation_audit <- feedback_explanation_issues(question_bank)

  by_module <- question_bank |>
    dplyr::group_by(.data$module_id) |>
    dplyr::summarise(
      n_questions = dplyr::n(),
      n_question_families = dplyr::n_distinct(.data$question_family_safe, na.rm = TRUE),
      n_with_visuals = sum(.data$has_visual, na.rm = TRUE),
      n_without_visuals = dplyr::n() - sum(.data$has_visual, na.rm = TRUE),
      pct_with_visuals = round(100 * mean(.data$has_visual, na.rm = TRUE), 1),
      n_with_hint_1 = sum(.data$has_hint_1, na.rm = TRUE),
      n_with_concept_explanation = sum(.data$has_concept_explanation, na.rm = TRUE),
      n_with_solution_explanation = sum(.data$has_solution_explanation, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      option_audit |>
        dplyr::group_by(.data$module_id) |>
        dplyr::summarise(n_option_issues = sum(.data$issue_count > 0, na.rm = TRUE), .groups = "drop"),
      by = "module_id"
    ) |>
    dplyr::mutate(n_option_issues = tidyr::replace_na(.data$n_option_issues, 0L)) |>
    dplyr::left_join(
      explanation_audit |>
        dplyr::group_by(.data$module_id) |>
        dplyr::summarise(n_feedback_explanation_issues = sum(.data$issue_count > 0, na.rm = TRUE), .groups = "drop"),
      by = "module_id"
    ) |>
    dplyr::mutate(n_feedback_explanation_issues = tidyr::replace_na(.data$n_feedback_explanation_issues, 0L)) |>
    dplyr::arrange(.data$module_id)

  by_concept <- question_bank |>
    dplyr::count(.data$module_id, .data$topic_id, .data$concept_tag, name = "n_questions") |>
    dplyr::arrange(.data$module_id, dplyr::desc(.data$n_questions), .data$concept_tag)

  duplicate_questions <- question_bank |>
    dplyr::count(.data$question_text, name = "n") |>
    dplyr::filter(.data$n > 1) |>
    dplyr::arrange(dplyr::desc(.data$n))

  list(
    total_questions = nrow(question_bank),
    missing_required_columns = missing_required,
    by_module = by_module,
    by_concept = by_concept,
    duplicate_questions = duplicate_questions,
    option_audit = option_audit,
    option_issues = option_audit |> dplyr::filter(.data$issue_count > 0),
    explanation_audit = explanation_audit,
    explanation_issues = explanation_audit |> dplyr::filter(.data$issue_count > 0),
    generation_methods = if ("generation_method" %in% names(question_bank)) dplyr::count(question_bank, .data$generation_method, name = "n") else tibble::tibble(),
    source_basis = if ("source_basis" %in% names(question_bank)) dplyr::count(question_bank, .data$source_basis, name = "n") else tibble::tibble()
  )
}

audit_visual_coverage <- function(question_bank = load_question_bank_for_audit()) {
  question_bank$has_visual <- has_any_visual_field(question_bank)
  visual_id_col <- if ("visual_id" %in% names(question_bank)) question_bank$visual_id else rep(NA_character_, nrow(question_bank))
  question_bank |>
    dplyr::mutate(visual_id = visual_id_col) |>
    dplyr::group_by(.data$module_id, .data$has_visual, .data$visual_id) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(.data$module_id, dplyr::desc(.data$has_visual), dplyr::desc(.data$n))
}

audit_question_families <- function(question_bank = load_question_bank_for_audit()) {
  family <- if ("question_family" %in% names(question_bank)) question_bank$question_family else question_bank$concept_tag
  question_bank |>
    dplyr::mutate(question_family = ifelse(has_text_value(family), as.character(family), as.character(.data$concept_tag))) |>
    dplyr::count(.data$module_id, .data$topic_id, .data$concept_tag, .data$question_family, name = "n_questions") |>
    dplyr::arrange(.data$module_id, .data$topic_id, dplyr::desc(.data$n_questions))
}

audit_question_metadata <- function(question_bank = load_question_bank_for_audit()) {
  metadata_cols <- c(
    "question_id", "module_id", "topic_id", "concept_tag", "question_family",
    "format", "choices", "correct_choice_id", "answer_type", "correct_answer", "accepted_answers", "hint_1",
    "concept_explanation", "solution_explanation", "visual_id", "visual_template_id",
    "generation_method", "source_basis", "reviewed_status"
  )
  tibble::tibble(
    field = metadata_cols,
    present = metadata_cols %in% names(question_bank),
    missing_or_blank = vapply(metadata_cols, function(col) {
      if (!col %in% names(question_bank)) return(nrow(question_bank))
      sum(!has_text_value(question_bank[[col]]))
    }, integer(1))
  )
}

write_question_bank_audit_report <- function(audit, path = "data/processed/question_bank_audit_report.md") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    "# Introduction to Statistics Question Bank Audit",
    "",
    paste0("Total questions: ", audit$total_questions),
    "",
    "## Missing required columns",
    if (length(audit$missing_required_columns) == 0) "None" else paste0("- ", audit$missing_required_columns),
    "",
    "## Questions by module",
    paste(capture.output(print(audit$by_module, n = Inf)), collapse = "\n"),
    "",
    "## Answer-option issues",
    if (nrow(audit$option_issues) == 0) "None" else paste(capture.output(print(audit$option_issues, n = Inf)), collapse = "\n"),
    "",
    "## Duplicate exact question text",
    if (nrow(audit$duplicate_questions) == 0) "None" else paste(capture.output(print(audit$duplicate_questions, n = Inf)), collapse = "\n")
  )
  writeLines(lines, path)
  invisible(path)
}

run_question_bank_audit <- function(path = "data/processed/question_bank.csv") {
  qb <- load_question_bank_for_audit(path)
  audit <- audit_question_bank(qb)
  visual <- audit_visual_coverage(qb)
  family <- audit_question_families(qb)
  metadata <- audit_question_metadata(qb)
  option_audit <- audit$option_audit

  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(audit$by_module, "data/processed/question_bank_audit.csv")
  readr::write_csv(visual, "data/processed/question_bank_visual_coverage.csv")
  readr::write_csv(family, "data/processed/question_bank_family_audit.csv")
  readr::write_csv(metadata, "data/processed/question_bank_metadata_audit.csv")
  readr::write_csv(option_audit, "data/processed/question_bank_answer_option_audit.csv")
  readr::write_csv(audit$option_issues, "data/processed/question_bank_answer_option_issues.csv")
  readr::write_csv(audit$explanation_audit, "data/processed/question_bank_feedback_explanation_audit.csv")
  readr::write_csv(audit$explanation_issues, "data/processed/question_bank_feedback_explanation_issues.csv")
  write_question_bank_audit_report(audit)

  cat("Question bank audit\n")
  cat("===================\n")
  cat("Total questions:", audit$total_questions, "\n")
  cat("Answer-option issue rows:", nrow(audit$option_issues), "\n\n")
  print(audit$by_module, n = Inf)
  cat("\nSaved audit files under data/processed/.\n")
  invisible(audit)
}
