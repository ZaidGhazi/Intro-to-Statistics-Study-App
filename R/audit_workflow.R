# Lightweight workflow audit for where the app uses LLM/API calls.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

audit_llm_usage <- function(paths = c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  patterns <- c(
    "ellmer", "chat_anthropic", "chat_openai", "chat\\$chat", "chat\\$stream",
    "ANTHROPIC_API_KEY", "OPENAI_API_KEY", "generate_similar_question", "verify_faithfulness",
    "rerank", "embedding", "Claude", "OpenAI"
  )
  hits <- purrr::map_dfr(paths[file.exists(paths)], function(path) {
    lines <- readLines(path, warn = FALSE)
    purrr::map_dfr(patterns, function(pattern) {
      idx <- grep(pattern, lines, ignore.case = TRUE)
      if (length(idx) == 0) return(tibble::tibble())
      tibble::tibble(
        file = path,
        line = idx,
        pattern = pattern,
        code = trimws(lines[idx])
      )
    })
  })
  hits
}

audit_practice_workflow <- function() {
  llm_hits <- audit_llm_usage()
  start_practice_hits <- llm_hits |>
    dplyr::filter(grepl("start_practice|Start Practice", .data$code, ignore.case = TRUE))
  tibble::tibble(
    workflow_step = c(
      "App startup", "Start Practice", "Next Question", "Submit Answer",
      "Give me a hint", "Explain concept", "Follow-up tutor message",
      "Similar question generation", "Evaluation/tests"
    ),
    expected_llm_use = c(
      "No", "No", "No", "No for local grading", "Usually no; stored hints first",
      "Optional", "Optional for explanation only", "Yes, if custom follow-up needs it",
      "Optional fallback", "Optional"
    ),
    notes = c(
      "Loads packages, processed banks, and metadata.",
      "Should filter and randomly sample stored questions only.",
      "Should use stored/random questions, with same-concept preference after wrong answers.",
      "Should grade locally when accepted answers/rubrics exist.",
      "Uses hint ladder before model calls.",
      "Can use stored concept explanations or grounded LLM polish.",
      "Visual is rendered by R/ggplot or metadata; model may explain it.",
      "Conversational tutor can call LLM when needed.",
      "Uses stored similar questions first; LLM only if enabled and needed.",
      "Vitals/edge tests may call model depending on dry_run."
    )
  ) |>
    dplyr::mutate(
      code_flag = dplyr::case_when(
        workflow_step == "Start Practice" & nrow(start_practice_hits) > 0 ~ "Review: possible LLM-related code near Start Practice",
        TRUE ~ "OK / inspect detailed hits"
      )
    )
}

run_workflow_audit <- function() {
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  hits <- audit_llm_usage()
  workflow <- audit_practice_workflow()
  readr::write_csv(hits, "data/processed/workflow_llm_usage_hits.csv")
  readr::write_csv(workflow, "data/processed/workflow_audit_summary.csv")
  report_path <- "data/processed/workflow_audit_report.md"
  writeLines(c(
    "# introductory statistics Workflow Audit",
    "",
    "## Intended live-app LLM use",
    paste(capture.output(print(workflow, n = Inf)), collapse = "\n"),
    "",
    "## Code search hits",
    paste(capture.output(print(hits, n = Inf)), collapse = "\n")
  ), report_path)
  cat("Workflow audit\n")
  cat("==============\n")
  print(workflow, n = Inf)
  cat("\nSaved workflow audit files under data/processed/.\n")
  invisible(list(workflow = workflow, llm_hits = hits))
}
