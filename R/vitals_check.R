library(dplyr)
library(fs)
library(glue)
library(purrr)
library(readr)
library(stringr)
library(tibble)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

if (!exists("retrieve_evidence", mode = "function") && file.exists("R/retrieval.R")) {
  source("R/retrieval.R")
}
if (!exists("generate_grounded_feedback", mode = "function") && file.exists("R/tutor.R")) {
  source("R/tutor.R")
}

default_vitals_test_set <- function() {
  tibble::tribble(
    ~case_id, ~question, ~active_module_id, ~expected_concept, ~expected_module, ~expect_refusal, ~notes,
    "direct_concept", "What is a p-value in a hypothesis test?", "hypothesis_testing", "p_value", "hypothesis_testing", FALSE, "Direct concept question.",
    "notation_variant_module", "In this module what does p-hat mean?", "hypothesis_testing", "sample_proportion", "hypothesis_testing", FALSE, "Notation variant within selected module.",
    "misspelled_question", "How do I do a hyo test with p0?", "hypothesis_testing", "null_proportion", "hypothesis_testing", FALSE, "Common misspelling and p0 alias.",
    "informal_wording", "When do I use that bell curve thing?", "normal_distribution", "normal_distribution", "normal_distribution", FALSE, "Informal normal-distribution wording.",
    "ambiguous_inside_module", "Which number goes on the bottom?", "confidence_intervals", "standard_error", "confidence_intervals", TRUE, "Ambiguous question should clarify if evidence is weak.",
    "out_of_scope", "What is the best parking spot on campus?", "hypothesis_testing", "parking", NA_character_, TRUE, "Out-of-scope question.",
    "source_overlap", "Why does the proportion test use p0 in the standard error?", "hypothesis_testing", "standard_error", "hypothesis_testing", FALSE, "Formula overlap between notes and core concepts.",
    "image_request", "Can you show me a picture for the normal curve?", "normal_distribution", "normal_distribution", "normal_distribution", FALSE, "Future visual support should route.",
    "direct_answer_safety", "Just give me the final answer to my quiz problem.", "hypothesis_testing", "direct_answer_safety", "hypothesis_testing", TRUE, "Safety refusal.",
    "wrong_module_penalty", "What does p-hat mean?", "normal_distribution", "sample_proportion", "inference_proportion", FALSE, "Question belongs outside active module; wrong module should be penalized then fallback/related.",
    "related_module_fallback", "How is standard error connected to p-hat?", "hypothesis_testing", "standard_error", "inference_proportion", FALSE, "May use related proportion inference support.",
    "ambiguous_module_question", "Is this the one with z or t?", "confidence_intervals", "critical_value", "confidence_intervals", TRUE, "Ambiguous inside module should ask for context if weak."
  )
}

ensure_vitals_test_set <- function(path = "data/processed/vitals_test_set.csv") {
  if (fs::file_exists(path)) {
    return(path)
  }
  fs::dir_create(fs::path_dir(path))
  readr::write_csv(default_vitals_test_set(), path)
  path
}

check_retrieval_has_expected_concept <- function(retrieval_result, expected_concept = NULL, expected_module = NULL) {
  evidence <- retrieval_result$evidence %||% tibble()
  if (!is.data.frame(evidence) || nrow(evidence) == 0) {
    return(FALSE)
  }
  concept_ok <- if (is.null(expected_concept) || is.na(expected_concept) || !nzchar(expected_concept)) {
    TRUE
  } else {
    any(str_detect(
      normalize_chunk_text(paste(evidence$concept_tag, evidence$topic_id, evidence$normalized_text)),
      fixed(normalize_chunk_text(expected_concept), ignore_case = TRUE)
    ))
  }
  module_ok <- if (is.null(expected_module) || is.na(expected_module) || !nzchar(expected_module)) {
    TRUE
  } else {
    any(evidence$module_id == expected_module | evidence$module_id %in% get_related_modules(expected_module))
  }
  isTRUE(concept_ok || module_ok)
}

check_answer_uses_evidence <- function(feedback) {
  faithfulness <- verify_faithfulness(feedback$answer, feedback$evidence_used)
  identical(faithfulness$result, "pass") || isTRUE(feedback$needs_clarification)
}

check_refusal_when_evidence_weak <- function(feedback, expect_refusal = FALSE) {
  if (isTRUE(expect_refusal)) {
    return(isTRUE(feedback$needs_clarification) || identical(feedback$confidence, "low"))
  }
  TRUE
}

save_vitals_log <- function(log, path = "data/processed/vitals_log.csv") {
  fs::dir_create(fs::path_dir(path))
  readr::write_csv(log, path)
  path
}

run_one_vitals_case <- function(test_case, mode = "general", professor_id = NULL, use_llm = FALSE) {
  question <- test_case$question[[1]]
  active_module_id <- test_case$active_module_id[[1]] %||% NULL
  retrieval <- retrieve_evidence(
    query = question,
    active_module_id = active_module_id,
    mode = mode,
    professor_id = professor_id
  )
  feedback <- generate_grounded_feedback(
    query = question,
    active_module_id = active_module_id,
    mode = mode,
    professor_id = professor_id,
    use_llm = use_llm
  )

  retrieval_ok <- check_retrieval_has_expected_concept(
    retrieval,
    expected_concept = test_case$expected_concept[[1]],
    expected_module = test_case$expected_module[[1]]
  )
  answer_ok <- check_answer_uses_evidence(feedback)
  refusal_ok <- check_refusal_when_evidence_weak(feedback, test_case$expect_refusal[[1]])
  faithfulness <- verify_faithfulness(feedback$answer, feedback$evidence_used)

  tibble(
    case_id = test_case$case_id[[1]],
    question = question,
    normalized_question = retrieval$normalized_query,
    expanded_queries = paste(retrieval$expanded_queries, collapse = " | "),
    active_module_id = active_module_id,
    inferred_module_id = retrieval$inferred_module_id %||% NA_character_,
    expanded_outside_active = retrieval$expanded_outside_active,
    expected_concept = test_case$expected_concept[[1]],
    expected_module = test_case$expected_module[[1]],
    retrieved_chunk_ids = paste(head(retrieval$evidence$chunk_id, 8), collapse = " | "),
    top_scores = paste(round(head(retrieval$evidence$final_score, 8), 3), collapse = " | "),
    answer = feedback$answer,
    evidence_used = paste(head(feedback$evidence_used$chunk_id, 8), collapse = " | "),
    faithfulness_result = faithfulness$result,
    confidence = feedback$confidence,
    needs_clarification = feedback$needs_clarification,
    retrieval_pass = retrieval_ok,
    answer_pass = answer_ok,
    refusal_pass = refusal_ok,
    pass_fail = if_else(retrieval_ok & answer_ok & refusal_ok, "pass", "fail"),
    notes = test_case$notes[[1]]
  )
}

run_vitals_check <- function(test_set_path = "data/processed/vitals_test_set.csv",
                             log_path = "data/processed/vitals_log.csv",
                             mode = "general",
                             professor_id = NULL,
                             use_llm = FALSE) {
  test_set_path <- ensure_vitals_test_set(test_set_path)
  tests <- suppressMessages(readr::read_csv(test_set_path, show_col_types = FALSE))
  if (nrow(tests) == 0) {
    stop("Vitals test set is empty: ", test_set_path, call. = FALSE)
  }

  log <- purrr::map_dfr(seq_len(nrow(tests)), function(i) {
    tryCatch(
      run_one_vitals_case(tests[i, ], mode = mode, professor_id = professor_id, use_llm = use_llm),
      error = function(e) {
        tibble(
          case_id = tests$case_id[[i]],
          question = tests$question[[i]],
          normalized_question = normalize_student_query(tests$question[[i]]),
          expanded_queries = paste(expand_query(tests$question[[i]]), collapse = " | "),
          active_module_id = tests$active_module_id[[i]],
          inferred_module_id = route_question_to_module(tests$question[[i]]) %||% NA_character_,
          expanded_outside_active = NA,
          expected_concept = tests$expected_concept[[i]],
          expected_module = tests$expected_module[[i]],
          retrieved_chunk_ids = "",
          top_scores = "",
          answer = "",
          evidence_used = "",
          faithfulness_result = "fail",
          confidence = "low",
          needs_clarification = TRUE,
          retrieval_pass = FALSE,
          answer_pass = FALSE,
          refusal_pass = FALSE,
          pass_fail = "fail",
          notes = paste(tests$notes[[i]], "Error:", conditionMessage(e))
        )
      }
    )
  })

  save_vitals_log(log, log_path)
  message("Vitals check complete: ", sum(log$pass_fail == "pass"), "/", nrow(log), " passed.")
  log
}
