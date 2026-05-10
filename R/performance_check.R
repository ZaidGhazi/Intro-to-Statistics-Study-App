if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

perf_elapsed <- function(start_time) {
  round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 3)
}

perf_source_app <- function() {
  if (!exists("load_question_bank_cached", mode = "function") ||
      !exists("get_questions_for_module", mode = "function") ||
      !exists("generate_practice_question", mode = "function")) {
    source("app.R", local = .GlobalEnv)
  }
  invisible(TRUE)
}

scan_start_practice_for_expensive_calls <- function(path = "app.R") {
  if (!file.exists(path)) {
    return(tibble::tibble(pattern = character(), found = logical()))
  }
  app_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  start_block <- stringr::str_match(
    app_text,
    "start_practice_session <- function[\\s\\S]*?reset_practice_session <- function"
  )[, 1] %||% ""
  checks <- c(
    "LLM generation" = "call_grounded_llm|chat\\$chat|chat\\$stream|ellmer|ANTHROPIC|Claude",
    "PDF ingestion" = "pdf_text|ingest_textbook|pdftools",
    "embedding/index rebuild" = "embed|embedding|vector_index|rebuild",
    "retrieval prefetch" = "retrieve_evidence|get_cached_question_evidence\\(question, force = TRUE\\)"
  )
  tibble::tibble(
    pattern = names(checks),
    found = vapply(checks, function(pattern) stringr::str_detect(start_block, pattern), logical(1))
  )
}

run_performance_check <- function(module_ids = NULL) {
  cat("introductory statistics performance check\n")
  cat("===========================\n")

  perf_source_app()

  processed_files <- tibble::tibble(
    file = c(
      "data/processed/question_bank.csv",
      "data/processed/question_bank.rds",
      "data/processed/retrieval_index.rds",
      "data/processed/image_metadata.rds"
    ),
    exists = file.exists(file)
  )
  print(processed_files)

  t_bank <- Sys.time()
  bank <- load_question_bank_cached(refresh = TRUE)
  bank_time <- perf_elapsed(t_bank)
  cat("Question bank rows:", nrow(bank), "\n")
  cat("Question bank load time:", bank_time, "sec\n")

  module_counts <- bank %>%
    dplyr::count(module_id, name = "questions") %>%
    dplyr::left_join(MODULES, by = "module_id") %>%
    dplyr::arrange(module_order, module_id)
  print(module_counts)

  selected_modules <- module_ids %||% module_counts$module_id[module_counts$questions > 0][1]
  selected_modules <- selected_modules[!is.na(selected_modules) & nzchar(selected_modules)]
  if (length(selected_modules) == 0) {
    cat("No module with available questions was found.\n")
    return(invisible(list(ok = FALSE, processed_files = processed_files, module_counts = module_counts)))
  }

  t_start <- Sys.time()
  pool <- get_questions_for_module(selected_modules)
  selected_row <- choose_next_question(
    question_bank = pool,
    active_module_ids = selected_modules,
    seen_question_ids = character(),
    current_question_id = NULL,
    valid_module_ids = MODULES$module_id
  )
  question <- if (is.data.frame(selected_row) && nrow(selected_row) > 0) make_practice_question_from_row(selected_row) else NULL
  start_time <- perf_elapsed(t_start)
  cat("Practice initialization module(s):", paste(selected_modules, collapse = ", "), "\n")
  cat("Practice pool rows:", nrow(pool), "\n")
  cat("Practice initialization time without tutor retrieval:", start_time, "sec\n")
  cat("Generated/selected question:", if (is.null(question)) "none" else question$question_id, "\n")

  expensive <- scan_start_practice_for_expensive_calls()
  print(expensive)
  if (any(expensive$found)) {
    cat("Warning: Start Practice still appears to reference expensive work. Inspect the rows above.\n")
  } else {
    cat("Start Practice scan: no direct LLM, PDF ingestion, embedding rebuild, or retrieval prefetch detected.\n")
  }

  api_required <- FALSE
  cat("API key required for Start Practice:", api_required, "\n")
  if (!file.exists("data/processed/question_bank.csv") && !file.exists("data/processed/question_bank.rds")) {
    cat("The processed question bank could not be found. Run preprocessing first, for example:\n")
    cat('source("R/build_question_bank.R")\n')
    cat("build_question_bank()\n")
  }

  invisible(list(
    ok = !any(expensive$found) && !is.null(question),
    processed_files = processed_files,
    question_bank_rows = nrow(bank),
    module_counts = module_counts,
    practice_pool_rows = nrow(pool),
    selected_question_id = if (is.null(question)) NA_character_ else question$question_id,
    bank_load_time = bank_time,
    practice_init_time = start_time,
    expensive_scan = expensive,
    api_required_for_start = api_required
  ))
}
