if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

normalize_practice_module_ids <- function(module_ids = NULL, valid_module_ids = NULL) {
  selected <- module_ids %||% character()
  selected <- unique(as.character(unlist(selected, use.names = FALSE)))
  selected <- selected[!is.na(selected) & nzchar(selected)]
  if (!is.null(valid_module_ids)) {
    selected <- intersect(selected, valid_module_ids)
  }
  selected
}

empty_question_selection <- function(reason, active_module_ids = character(), pool_size = 0L) {
  out <- tibble::tibble()
  attr(out, "selection_debug") <- list(
    active_module_ids = active_module_ids,
    pool_size = pool_size,
    candidate_count = 0L,
    seen_count = 0L,
    seen_reset = FALSE,
    selected_question_id = NA_character_,
    reason = reason
  )
  out
}

choose_next_question <- function(question_bank,
                                 active_module_ids,
                                 seen_question_ids = character(),
                                 current_question_id = NULL,
                                 valid_module_ids = NULL) {
  if (!is.data.frame(question_bank) || nrow(question_bank) == 0) {
    return(empty_question_selection("missing_question_bank"))
  }
  if (!all(c("question_id", "module_id") %in% names(question_bank))) {
    return(empty_question_selection("missing_question_columns"))
  }

  module_ids <- normalize_practice_module_ids(active_module_ids, valid_module_ids)
  if (length(module_ids) == 0) {
    return(empty_question_selection("no_modules_selected"))
  }

  pool <- question_bank |>
    dplyr::filter(.data$module_id %in% module_ids)
  pool_size <- nrow(pool)
  if (pool_size == 0) {
    return(empty_question_selection("no_questions_for_selected_modules", module_ids, pool_size))
  }

  seen <- unique(as.character(c(seen_question_ids %||% character(), current_question_id %||% character())))
  seen <- seen[!is.na(seen) & nzchar(seen)]
  candidates <- pool |>
    dplyr::filter(!.data$question_id %in% seen)
  seen_reset <- FALSE

  if (nrow(candidates) == 0) {
    seen_reset <- TRUE
    candidates <- pool |>
      dplyr::filter(.data$question_id != (current_question_id %||% ""))
  }
  if (nrow(candidates) == 0) {
    candidates <- pool
  }

  picked <- candidates |>
    dplyr::slice_sample(n = 1)

  attr(picked, "selection_debug") <- list(
    active_module_ids = module_ids,
    pool_size = pool_size,
    candidate_count = nrow(candidates),
    seen_count = length(seen),
    seen_reset = seen_reset,
    selected_question_id = picked$question_id[[1]] %||% NA_character_,
    reason = "sampled"
  )
  picked
}
