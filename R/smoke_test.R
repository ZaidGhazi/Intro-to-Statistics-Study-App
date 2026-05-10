if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

source_if_needed <- function(path) {
  if (!file.exists(path)) {
    stop("Required source file is missing: ", path, call. = FALSE)
  }
  source(path)
}

smoke_record <- function(results, name, expr) {
  outcome <- tryCatch(
    {
      value <- force(expr)
      list(name = name, pass = TRUE, value = value, error = NA_character_)
    },
    error = function(e) list(name = name, pass = FALSE, value = NULL, error = conditionMessage(e))
  )
  status <- if (isTRUE(outcome$pass)) "PASS" else "FAIL"
  cat(sprintf("[%s] %s", status, name), "\n")
  if (!isTRUE(outcome$pass)) {
    cat("      ", outcome$error, "\n", sep = "")
  }
  c(results, list(outcome))
}

run_smoke_test <- function(run_vitals = requireNamespace("vitals", quietly = TRUE)) {
  cat("introductory statistics smoke test\n")
  cat("====================\n")

  if (!exists("check_setup", mode = "function")) {
    source_if_needed("R/check_setup.R")
  }
  setup <- check_setup(verbose = TRUE)

  results <- list()
  launch_missing <- setup$launch_packages_missing %||% character()
  if (length(launch_missing) > 0) {
    results <- smoke_record(results, "launch package availability", {
      stop("Missing launch packages: ", paste(launch_missing, collapse = ", "), call. = FALSE)
    })
    cat("Smoke test stopped before sourcing app modules because launch packages are missing.\n")
    return(invisible(list(pass = FALSE, results = results, setup = setup)))
  }

  results <- smoke_record(results, "source core RAG files", {
    source_if_needed("R/chunk_schema.R")
    source_if_needed("R/aliases.R")
    source_if_needed("R/overlays.R")
    source_if_needed("R/retrieval.R")
    source_if_needed("R/images.R")
    source_if_needed("R/visual_helpers.R")
    source_if_needed("R/tutor.R")
    TRUE
  })

  if (!isTRUE(results[[length(results)]]$pass)) {
    return(invisible(list(pass = FALSE, results = results, setup = setup)))
  }

  results <- smoke_record(results, "alias normalization", {
    normalized <- normalize_student_query("what does p^ mean?")
    if (!grepl("p_hat", normalized, fixed = TRUE)) {
      stop("Expected p^ to normalize to p_hat; got: ", normalized, call. = FALSE)
    }
    normalized
  })

  results <- smoke_record(results, "rerank missing-score regression", {
    if (exists("test_rerank_missing_score_cols", mode = "function")) {
      test_rerank_missing_score_cols()
    } else {
      test_candidates <- tibble::tibble(
        chunk_id = "c1",
        text = "p_hat is the sample proportion.",
        semantic_score = 0.8,
        keyword_score = 0.2
      )
      rerank_chunks(test_candidates, query = "what does p^ mean?", active_module_id = "hypothesis_testing")
    }
  })

  results <- smoke_record(results, "retrieve_evidence p_hat query", {
    retrieval <- retrieve_evidence(
      "what does p^ mean?",
      active_module_id = "hypothesis_testing",
      mode = "general"
    )
    if (!is.list(retrieval) || !"evidence" %in% names(retrieval) || !is.data.frame(retrieval$evidence)) {
      stop("retrieve_evidence() did not return a structured object with an evidence tibble.", call. = FALSE)
    }
    retrieval
  })

  results <- smoke_record(results, "public demo corpus retrieval", {
    demo_chunks <- load_public_demo_chunks()
    if (!is.data.frame(demo_chunks) || nrow(demo_chunks) < 8) {
      stop("Expected at least 8 public-safe demo chunks.", call. = FALSE)
    }
    demo_hits <- keyword_retrieve("what is a p-value?", chunks = demo_chunks, top_k = 5)
    if (!is.data.frame(demo_hits) || nrow(demo_hits) == 0) {
      stop("Expected the public demo corpus to retrieve evidence for a p-value query.", call. = FALSE)
    }
    demo_hits
  })

  results <- smoke_record(results, "retrieve_evidence multi-module routing", {
    retrieval <- retrieve_evidence(
      "what is a p-value?",
      active_module_ids = c("confidence_intervals", "hypothesis_testing"),
      current_module_id = "confidence_intervals",
      mode = "general",
      top_k = 4
    )
    if (!is.list(retrieval) || !"active_module_ids" %in% names(retrieval)) {
      stop("retrieve_evidence() did not preserve active_module_ids.", call. = FALSE)
    }
    if (!"hypothesis_testing" %in% retrieval$active_module_ids) {
      stop("Expected selected module pool to include hypothesis_testing.", call. = FALSE)
    }
    retrieval
  })

  results <- smoke_record(results, "grounded tutor fallback without API key", {
    feedback <- generate_grounded_feedback(
      "what does p^ mean?",
      active_module_id = "hypothesis_testing",
      mode = "general",
      use_llm = FALSE
    )
    if (!is.list(feedback) || !"answer" %in% names(feedback)) {
      stop("generate_grounded_feedback() did not return an answer object.", call. = FALSE)
    }
    feedback
  })

  results <- smoke_record(results, "contextual practice help with vague missing context", {
    help <- generate_contextual_practice_help(
      help_mode = "diagnose",
      practice_context = list(active_module_id = "hypothesis_testing", mode = "general"),
      help_question = "why is this wrong?",
      use_llm = FALSE
    )
    if (!isTRUE(help$needs_clarification)) {
      stop("Expected vague practice help without current question context to ask for clarification.", call. = FALSE)
    }
    help
  })

  results <- smoke_record(results, "contextual practice hint with current question", {
    help <- generate_contextual_practice_help(
      help_mode = "hint",
      practice_context = list(
        active_module_id = "hypothesis_testing",
        current_question_id = "smoke_p_hat",
        question_text = "In a one-proportion hypothesis test, what does p-hat represent?",
        student_answer = "",
        answer_submitted = FALSE,
        attempt_count = 0L,
        expected_concept_tag = "sample_proportion",
        weak_concept_tag = "sample_proportion",
        mode = "general"
      ),
      help_question = "Give me a hint",
      use_llm = FALSE
    )
    if (!isTRUE(help$answer_withheld)) {
      stop("Expected hint mode to withhold the final answer.", call. = FALSE)
    }
    if (stringr::str_detect(stringr::str_to_lower(help$answer %||% ""), "good try|your answer|you chose|correct|incorrect")) {
      stop("Hint mode used answer-assessment language before an answer was submitted.", call. = FALSE)
    }
    help
  })

  resistant_context <- list(
    active_module_id = "descriptive_stats",
    current_module_id = "descriptive_stats",
    active_module_ids = "descriptive_stats",
    question_id = "smoke_resistant_measures_001",
    current_question_id = "ds_fib_003",
    question_text = "When a distribution is strongly right-skewed with several large outliers, the preferred measure of center is the ______ because it is resistant to extreme values.",
    topic_id = "descriptive_stats",
    module_id = "module_1",
    correct_answer = "median",
    student_answer = "",
    answer_submitted = FALSE,
    attempt_count = 0L,
    expected_concept_tag = "resistant_measures",
    weak_concept_tag = "resistant_measures",
    hint_1 = "Focus on the phrase 'resistant to extreme values.' Which measure of center is based on the middle position instead of using every value?",
    concept_explanation = "When a distribution has extreme outliers, the median is usually preferred because it depends on position, not the size of every value. The mean uses every value, so large outliers pull it toward the tail.",
    solution_explanation = "The median is the preferred center for strongly skewed data with large outliers because it stays near the middle position. The mean is nonresistant and is pulled toward the large values.",
    mode = "general"
  )
  resistant_empty_evidence <- list(
    evidence = tibble::tibble(),
    retrieval_trace = tibble::tibble(),
    normalized_query = "",
    expanded_queries = character(),
    active_module_id = "descriptive_stats",
    current_module_id = "descriptive_stats",
    active_module_ids = "descriptive_stats",
    inferred_module_id = "descriptive_stats",
    expanded_outside_active = FALSE,
    expanded_outside_selected = FALSE,
    rerank_time = 0
  )

  results <- smoke_record(results, "resistant measures concept mode anchored", {
    help <- generate_contextual_practice_help(
      help_mode = "concept",
      practice_context = resistant_context,
      help_question = "Explain this concept.",
      use_llm = FALSE,
      evidence_result = resistant_empty_evidence,
      visual_metadata = tibble::tibble()
    )
    answer <- stringr::str_to_lower(help$answer %||% "")
    if (!stringr::str_detect(answer, "resistant|resistance") ||
        !stringr::str_detect(answer, "median|middle position") ||
        !stringr::str_detect(answer, "mean") ||
        !stringr::str_detect(answer, "outlier|tail")) {
      stop("Expected concept mode to explain resistant median/mean/outlier ideas.", call. = FALSE)
    }
    if (stringr::str_detect(answer, "categorical|bar chart|graph-to-variable|concept page")) {
      stop("Concept mode drifted into unrelated graph/variable-type or internal wording.", call. = FALSE)
    }
    help
  })

  results <- smoke_record(results, "resistant measures hint mode anchored", {
    help <- generate_contextual_practice_help(
      help_mode = "hint",
      practice_context = resistant_context,
      help_question = "Give me a hint.",
      use_llm = FALSE,
      evidence_result = resistant_empty_evidence,
      visual_metadata = tibble::tibble()
    )
    answer <- stringr::str_to_lower(help$answer %||% "")
    if (!stringr::str_detect(answer, "hint|resistant|extreme values|outlier")) {
      stop("Expected hint mode to reference resistance/extreme values.", call. = FALSE)
    }
    if (stringr::str_detect(answer, "concept page|categorical|bar chart|graph")) {
      stop("Hint mode exposed internal wording or unrelated graph/type content.", call. = FALSE)
    }
    if (stringr::str_count(answer, "median") > 0) {
      stop("First hint revealed the final answer too directly.", call. = FALSE)
    }
    help
  })

  results <- smoke_record(results, "resistant measures diagnose without submission anchored", {
    help <- generate_contextual_practice_help(
      help_mode = "diagnose",
      practice_context = resistant_context,
      help_question = "Why was my answer wrong?",
      use_llm = FALSE,
      evidence_result = resistant_empty_evidence,
      visual_metadata = tibble::tibble()
    )
    answer <- stringr::str_to_lower(help$answer %||% "")
    if (!stringr::str_detect(answer, "after you submit|submit one")) {
      stop("Expected diagnose mode without a submitted answer to ask for submission first.", call. = FALSE)
    }
    if (!stringr::str_detect(answer, "resistant|extreme values|outlier|middle")) {
      stop("Expected diagnose-without-submission to provide a relevant resistant-measures hint.", call. = FALSE)
    }
    if (stringr::str_detect(answer, "categorical|bar chart|graph-to-variable|correct|incorrect")) {
      stop("Diagnose-without-submission drifted or evaluated correctness too early.", call. = FALSE)
    }
    help
  })

  results <- smoke_record(results, "resistant measures visual mode anchored", {
    visual_type <- choose_visual_type("Can you show this visually?", resistant_context)
    if (!identical(visual_type, "mean_vs_median_skew")) {
      stop("Expected resistant/outlier question to choose mean_vs_median_skew visual.", call. = FALSE)
    }
    response <- visual_response_for_type(visual_type)
    if (!stringr::str_detect(stringr::str_to_lower(response), "mean|median|tail|outlier|skew")) {
      stop("Expected visual response to discuss mean/median/tail/outlier ideas.", call. = FALSE)
    }
    turn <- create_tutor_message(
      role = "assistant",
      text = response,
      help_mode = "followup",
      visuals = list(deterministic_visual_message(visual_type, message_id = "resistant_visual")),
      message_id = "resistant_visual"
    )
    if (length(turn$visuals) != 1L) {
      stop("Expected visual to be attached to the tutor message.", call. = FALSE)
    }
    if (!identical(turn$visuals[[1]]$visual_id, "mean_vs_median_skew")) {
      stop("Expected attached visual_id to be mean_vs_median_skew.", call. = FALSE)
    }
    turn
  })

  results <- smoke_record(results, "visual metadata retrieval", {
    visuals <- retrieve_relevant_visuals(
      "Can you show p-value visually?",
      module_id = "hypothesis_testing",
      top_k = 3
    )
    if (!is.data.frame(visuals)) {
      stop("retrieve_relevant_visuals() did not return a data frame.", call. = FALSE)
    }
    if (nrow(visuals) == 0) {
      stop("Expected at least one recreated or metadata-backed visual candidate.", call. = FALSE)
    }
    visuals
  })

  results <- smoke_record(results, "deterministic visual routing", {
    q <- list(
      concept_tag = "resistant_statistics",
      question_text = "Why is the median more resistant than the mean for a right-skewed income distribution with outliers?"
    )
    if (!isTRUE(is_visual_request("Can you show this visually?"))) {
      stop("Expected visual request detection to return TRUE.", call. = FALSE)
    }
    visual_type <- choose_visual_type("Can you show this visually?", q)
    if (!identical(visual_type, "mean_vs_median_skew")) {
      stop("Expected mean_vs_median_skew visual; got: ", visual_type %||% "NULL", call. = FALSE)
    }
    visual_response_for_type(visual_type)
  })

  results <- smoke_record(results, "tutor message visual attachment", {
    visual <- deterministic_visual_message(
      visual_type = "mean_vs_median_skew",
      message_id = "assistant_visual_1",
      file_path = "www/session_visuals/example.png",
      src = "session_visuals/example.png",
      module_id = "descriptive_stats",
      concept_tag = "resistant_statistics"
    )
    turn <- create_tutor_message(
      role = "assistant",
      text = "### Visual aid\n\nI added a visual below.",
      help_mode = "followup",
      visuals = list(visual),
      message_id = "assistant_visual_1"
    )
    if (length(turn$visuals) != 1L || !identical(turn$visuals[[1]]$visual_id, "mean_vs_median_skew")) {
      stop("Expected the assistant turn to retain its attached visual metadata.", call. = FALSE)
    }
    if (!identical(turn$message_id, turn$visuals[[1]]$message_id)) {
      stop("Expected the visual to be tied to the same message_id as the tutor turn.", call. = FALSE)
    }
    turn
  })

  results <- smoke_record(results, "multiple tutor visuals remain message-scoped", {
    turn_one <- create_tutor_message(
      role = "assistant",
      text = "First visual.",
      visuals = list(deterministic_visual_message("mean_vs_median_skew", message_id = "m1")),
      message_id = "m1"
    )
    turn_two <- create_tutor_message(
      role = "assistant",
      text = "Second visual.",
      visuals = list(deterministic_visual_message("bar_vs_histogram", message_id = "m2")),
      message_id = "m2"
    )
    history <- list(turn_one, turn_two)
    visual_ids <- purrr::map_chr(history, ~ .x$visuals[[1]]$visual_id)
    if (!identical(visual_ids, c("mean_vs_median_skew", "bar_vs_histogram"))) {
      stop("Expected each tutor turn to keep its own visual instead of replacing the prior visual.", call. = FALSE)
    }
    history
  })

  results <- smoke_record(results, "no visual fallback has no empty visual", {
    turn <- create_tutor_message(
      role = "assistant",
      text = format_visual_fallback_response("unknown_concept"),
      help_mode = "followup",
      visuals = list(),
      message_id = "no_visual"
    )
    if (length(turn$visuals) != 0L) {
      stop("Expected no-visual fallback tutor turn to have no attached visual metadata.", call. = FALSE)
    }
    if (!stringr::str_detect(turn$text, stringr::fixed("### Picture it this way"))) {
      stop("Expected no-visual fallback to remain formatted Markdown.", call. = FALSE)
    }
    turn
  })

  results <- smoke_record(results, "visual debug metadata surfaced", {
    app_text <- if (file.exists("app.R")) paste(readLines("app.R", warn = FALSE), collapse = "\n") else ""
    if (!stringr::str_detect(app_text, "message_visual_debug") ||
        !stringr::str_detect(app_text, "message_id") ||
        !stringr::str_detect(app_text, "visual_id")) {
      stop("Expected debug UI to include message-level visual metadata fields.", call. = FALSE)
    }
    TRUE
  })

  results <- smoke_record(results, "tutor markdown cleanup", {
    dirty <- paste(
      "Following up on variable_classification:",
      "concept_tag: variable_classification",
      "### Picture it this way",
      "",
      "- **Categorical variables** use labels.",
      sep = "\n"
    )
    cleaned <- clean_tutor_markdown(dirty)
    if (stringr::str_detect(cleaned, "Following up on|concept_tag:")) {
      stop("Internal tutor metadata was not removed from student-facing Markdown.", call. = FALSE)
    }
    if (!stringr::str_detect(cleaned, stringr::fixed("### Picture it this way"))) {
      stop("Markdown heading was not preserved during cleanup.", call. = FALSE)
    }
    cleaned
  })

  results <- smoke_record(results, "visual fallback markdown formatting", {
    formatted <- format_visual_fallback_response(
      concept_label = "variable_classification",
      explanation = "Imagine a bar chart where each bar is one category.",
      bullets = c(
        "A **categorical variable** uses group labels.",
        "A **histogram** is for quantitative measurements."
      ),
      guiding_question = "Are the values group names or measured numbers?"
    )
    if (!stringr::str_detect(formatted, stringr::fixed("### Picture it this way"))) {
      stop("Expected visual fallback to include a Markdown heading.", call. = FALSE)
    }
    if (!stringr::str_detect(formatted, stringr::fixed("- A **categorical variable**"))) {
      stop("Expected visual fallback to include Markdown bullets and bold terms.", call. = FALSE)
    }
    if (stringr::str_detect(formatted, "Following up on|concept_tag:")) {
      stop("Visual fallback leaked internal metadata.", call. = FALSE)
    }
    formatted
  })

  results <- smoke_record(results, "edge case dataset build", {
    source_if_needed("R/edge_case_tests.R")
    edge_cases <- build_edge_case_dataset()
    if (!is.data.frame(edge_cases) || nrow(edge_cases) < 20) {
      stop("Expected at least 20 edge-case rows.", call. = FALSE)
    }
    edge_cases
  })

  if (isTRUE(run_vitals)) {
    results <- smoke_record(results, "vitals dry run", {
      source_if_needed("R/evals_vitals.R")
      run_vitals_eval(dry_run = TRUE, view = FALSE)
    })
  } else {
    if (requireNamespace("vitals", quietly = TRUE)) {
      cat("[SKIP] vitals dry run (run_vitals = FALSE)\n")
    } else {
      cat("[SKIP] vitals dry run (vitals package is not installed in this R library)\n")
    }
  }

  pass <- all(vapply(results, function(x) isTRUE(x$pass), logical(1)))
  cat("Overall smoke result:", if (pass) "PASS" else "FAIL", "\n")
  invisible(list(pass = pass, results = results, setup = setup))
}

if (tolower(Sys.getenv("STAT2331_AUTO_SMOKE", unset = "false")) %in% c("true", "1", "yes")) {
  run_smoke_test()
}
