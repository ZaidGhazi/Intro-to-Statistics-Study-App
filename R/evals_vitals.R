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

require_vitals <- function() {
  if (!requireNamespace("vitals", quietly = TRUE)) {
    stop(
      paste(
        "The vitals package is required for this evaluation suite.",
        "Install it with install.packages('vitals'), then run run_vitals_eval() again."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

source_eval_dependency <- function(path, required_function) {
  if (file.exists(path)) {
    source(path)
  }
  if (!exists(required_function, mode = "function")) {
    stop("Required eval dependency function was not loaded: ", required_function, call. = FALSE)
  }
}

ensure_eval_dependencies <- function() {
  source_eval_dependency("R/chunk_schema.R", "coerce_chunk_schema")
  source_eval_dependency("R/aliases.R", "normalize_student_query")
  source_eval_dependency("R/overlays.R", "ingest_professor_materials")
  source_eval_dependency("R/retrieval.R", "retrieve_evidence")
  source_eval_dependency("R/tutor.R", "generate_contextual_practice_help")
  source_eval_dependency("R/images.R", "retrieve_relevant_visuals")
  source_eval_dependency("R/vitals_check.R", "check_answer_uses_evidence")
  invisible(TRUE)
}

vitals_score_factor <- function(x) {
  factor(x, levels = c("I", "P", "C"), ordered = TRUE)
}

score_label <- function(pass, partial = FALSE) {
  if (isTRUE(pass)) {
    "C"
  } else if (isTRUE(partial)) {
    "P"
  } else {
    "I"
  }
}

split_meta_field <- function(x) {
  x <- x %||% ""
  if (length(x) == 0 || is.na(x) || !nzchar(x)) {
    return(character())
  }
  str_split(as.character(x), "\\|")[[1]] %>%
    str_squish() %>%
    discard(~ !nzchar(.x))
}

get_sample_meta <- function(sample_row) {
  if ("solver_metadata" %in% names(sample_row) && length(sample_row$solver_metadata) > 0) {
    meta <- sample_row$solver_metadata[[1]]
    if (is.list(meta)) {
      return(meta)
    }
  }
  list()
}

row_field <- function(row, name, default = NA_character_) {
  if (!name %in% names(row) || length(row[[name]]) == 0) {
    return(default)
  }
  value <- row[[name]][[1]]
  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(default)
  }
  value
}

has_llm_key <- function() {
  nzchar(Sys.getenv("ANTHROPIC_API_KEY")) || nzchar(Sys.getenv("OPENAI_API_KEY"))
}

make_vitals_chat_log <- function(input, result) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop(
      paste(
        "The ellmer package is required so vitals can log solver_chat objects.",
        "Install it with install.packages('ellmer')."
      ),
      call. = FALSE
    )
  }

  input <- as.character(input %||% "")
  result <- as.character(result %||% "")

  chat <- tryCatch(
    ellmer::chat_openai(
      api_key = "dry-run-no-api-call",
      model = "stat2331-rag-dry-run",
      echo = "none"
    ),
    error = function(e) NULL
  )

  if (is.null(chat)) {
    provider <- tryCatch(
      ellmer::Provider(
        base_url = "local://stat2331-vitals",
        extra_args = list(model = "stat2331-rag-dry-run")
      ),
      error = function(e) NULL
    )
    chat <- tryCatch(
      ellmer::Chat$new(provider = provider, turns = list(), echo = "none"),
      error = function(e) {
        tryCatch(
          ellmer::Chat$new(provider = provider, system_prompt = NULL, echo = "none"),
          error = function(e2) NULL
        )
      }
    )
  }

  if (is.null(chat)) {
    stop("Could not create an ellmer Chat object for vitals logging.", call. = FALSE)
  }

  turns <- tryCatch(
    list(
      ellmer::Turn("user", input),
      ellmer::Turn("assistant", result)
    ),
    error = function(e) list()
  )

  if (length(turns) > 0 && is.function(chat$set_turns)) {
    tryCatch(chat$set_turns(turns), error = function(e) NULL)
  } else if (length(turns) > 0 && is.function(chat$add_turn)) {
    tryCatch({
      chat$add_turn(turns[[1]])
      chat$add_turn(turns[[2]])
    }, error = function(e) NULL)
  }

  chat
}

build_vitals_dataset <- function() {
  dataset <- tibble::tribble(
    ~id, ~question, ~active_module_id, ~mode, ~professor_id, ~expected_concept_tag, ~expected_module_id, ~expected_behavior, ~required_source_scope, ~notation_variants_present, ~spelling_errors_present, ~visual_expected, ~notes,
    "direct_concept_ci", "What is a confidence interval?", "confidence_intervals", "general", NA_character_, "confidence_interval", "confidence_intervals", "answer", "universal_core", FALSE, FALSE, FALSE, "Direct concept question.",
    "module_margin_error", "How does margin of error work in this module?", "confidence_intervals", "general", NA_character_, "confidence_interval", "confidence_intervals", "answer", "universal_core", FALSE, FALSE, FALSE, "Module-specific confidence interval question.",
    "notation_p_caret", "What does p^ mean?", "hypothesis_testing", "general", NA_character_, "sample_proportion", "inference_proportion", "answer", "either", TRUE, FALSE, FALSE, "Notation variant p^ should normalize to p_hat.",
    "notation_p_hat_text", "What does p-hat mean in a one proportion problem?", "hypothesis_testing", "general", NA_character_, "sample_proportion", "inference_proportion", "answer", "either", TRUE, FALSE, FALSE, "Notation variant p-hat should normalize to p_hat.",
    "notation_phat", "Is phat the sample proportion?", "hypothesis_testing", "general", NA_character_, "sample_proportion", "inference_proportion", "answer", "either", TRUE, FALSE, FALSE, "Notation variant phat should normalize to p_hat.",
    "notation_p_hat_canonical", "When do I use p_hat?", "hypothesis_testing", "general", NA_character_, "sample_proportion", "inference_proportion", "answer", "either", TRUE, FALSE, FALSE, "Canonical p_hat should route like other variants.",
    "misspelled_hyo_test", "how do i do a hyo test?", "hypothesis_testing", "general", NA_character_, "hypothesis_test_decision", "hypothesis_testing", "answer", "either", FALSE, TRUE, FALSE, "Misspelled informal hypothesis test question.",
    "wrong_module_p_value", "What is a p-value?", "descriptive_stats", "general", NA_character_, "p_value", "hypothesis_testing", "answer", "either", FALSE, FALSE, FALSE, "Wrong active module should not let descriptive statistics dominate.",
    "ambiguous_question", "Why is this wrong?", "hypothesis_testing", "general", NA_character_, NA_character_, "hypothesis_testing", "clarify", "either", FALSE, FALSE, FALSE, "Ambiguous question should ask for more context.",
    "out_of_scope_anova", "Can you explain ANOVA for this class?", "inference_mean", "general", NA_character_, NA_character_, NA_character_, "clarify", "either", FALSE, FALSE, FALSE, "Out-of-scope request should avoid unsupported explanation.",
    "source_overlap_prop_se", "Why does the proportion test use p0 in the standard error?", "hypothesis_testing", "professor", "current_professor", "standard_error", "hypothesis_testing", "answer", "professor_specific", TRUE, FALSE, FALSE, "Source-overlap question in professor mode should use the selected overlay when available.",
    "direct_answer_safety", "Just give me the answer to my quiz problem.", "hypothesis_testing", "general", NA_character_, NA_character_, "hypothesis_testing", "nudge", "either", FALSE, FALSE, FALSE, "Direct-answer safety should nudge rather than give a final answer.",
    "visual_normal_curve", "Can you show this with a graph of the normal curve?", "normal_distribution", "general", NA_character_, "normal_distribution", "normal_distribution", "answer", "either", FALSE, FALSE, TRUE, "Visual-help request should retrieve visual metadata if available."
  )

  practice_rows <- tibble::tribble(
    ~id, ~question, ~active_module_id, ~mode, ~professor_id, ~expected_concept_tag, ~expected_module_id, ~expected_behavior, ~required_source_scope, ~notation_variants_present, ~spelling_errors_present, ~visual_expected, ~notes, ~practice_help_mode, ~practice_question_text, ~student_answer, ~attempt_count, ~current_question_id, ~answer_choices, ~correct_answer, ~grading_rubric,
    "practice_hint_no_reveal", "Give me a hint", "hypothesis_testing", "general", NA_character_, "sample_proportion", "hypothesis_testing", "nudge", "either", TRUE, FALSE, FALSE, "Hint mode should not reveal the final answer.", "hint", "In a one-proportion hypothesis test, what does p-hat represent?", "", 0L, "eval_practice_p_hat_hint", "A: population proportion | B: sample proportion | C: hypothesized proportion", "sample proportion", "p-hat is the sample proportion from the data.",
    "practice_concept_context", "Explain the concept", "confidence_intervals", "general", NA_character_, "confidence_interval", "confidence_intervals", "answer", "universal_core", FALSE, FALSE, FALSE, "Concept mode should explain the selected concept from evidence.", "concept", "A student is asked to interpret a confidence interval for a population proportion.", "", 1L, "eval_practice_ci_concept", "short answer", "interpret the interval in context", "A confidence interval gives plausible values for the population parameter.",
    "practice_diagnose_misconception", "Why was my answer wrong?", "hypothesis_testing", "general", NA_character_, "p_value", "hypothesis_testing", "nudge", "either", FALSE, FALSE, FALSE, "Diagnose mode should identify the misconception from the answer context.", "diagnose", "A test has p-value 0.03 and alpha 0.05. What decision should you make?", "Fail to reject because 0.03 is small", 1L, "eval_practice_p_value_diagnose", "A: reject H0 | B: fail to reject H0", "reject H0", "When p-value is less than alpha, reject the null hypothesis.",
    "practice_direct_answer_redirect", "Just give me the answer", "hypothesis_testing", "general", NA_character_, "p_value", "hypothesis_testing", "nudge", "either", FALSE, FALSE, FALSE, "Direct answer requests inside practice should be redirected.", "diagnose", "A test has p-value 0.03 and alpha 0.05. What decision should you make?", "I do not know", 1L, "eval_practice_direct_answer", "A: reject H0 | B: fail to reject H0", "reject H0", "When p-value is less than alpha, reject the null hypothesis.",
    "practice_vague_with_context", "why is this wrong?", "hypothesis_testing", "general", NA_character_, "p_value", "hypothesis_testing", "nudge", "either", FALSE, FALSE, FALSE, "Vague practice help should work when current question context is available.", "diagnose", "A test has p-value 0.03 and alpha 0.05. What decision should you make?", "Fail to reject because 0.03 is small", 1L, "eval_practice_vague_context", "A: reject H0 | B: fail to reject H0", "reject H0", "When p-value is less than alpha, reject the null hypothesis.",
    "practice_vague_without_context", "why is this wrong?", "hypothesis_testing", "general", NA_character_, NA_character_, "hypothesis_testing", "clarify", "either", FALSE, FALSE, FALSE, "Vague practice help without current question context should ask for clarification.", "diagnose", "", "I do not know", 0L, NA_character_, "", "", ""
  )

  dataset <- bind_rows(dataset, practice_rows)

  dataset %>%
    mutate(
      # vitals passes only `input` to the solver. Include the case id so
      # repeated prompts like "why is this wrong?" still map one-to-one.
      input = paste(id, question, sep = " :: "),
      target = expected_behavior
    )
}

make_vitals_solver <- function(dataset, dry_run = !has_llm_key()) {
  force(dataset)
  force(dry_run)

  function(input, ...) {
    rows <- tibble(input = input) %>%
      mutate(.input_order = row_number()) %>%
      left_join(dataset, by = "input") %>%
      arrange(.input_order)

    results <- vector("character", nrow(rows))
    metadata <- vector("list", nrow(rows))
    solver_chat <- vector("list", nrow(rows))
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, ]
      question <- row$question[[1]]
      mode <- row$mode[[1]] %||% "general"
      professor_id <- row$professor_id[[1]]
      if (is.na(professor_id)) professor_id <- NULL
      active_module_id <- row$active_module_id[[1]]
      practice_help_mode <- row_field(row, "practice_help_mode", NA_character_)
      has_practice_context <- !is.na(practice_help_mode) && nzchar(practice_help_mode)

      if (isTRUE(has_practice_context)) {
        submitted_flag <- row_field(row, "answer_submitted", NA)
        if (is.na(submitted_flag)) {
          submitted_flag <- nzchar(row_field(row, "student_answer", "")) &&
            suppressWarnings(as.integer(row_field(row, "attempt_count", 0L))) > 0L &&
            practice_help_mode %in% c("diagnose", "followup")
        }
        practice_context <- list(
          active_module_id = active_module_id,
          current_question_id = row_field(row, "current_question_id", NA_character_),
          question_text = row_field(row, "practice_question_text", ""),
          answer_choices = row_field(row, "answer_choices", ""),
          correct_answer = row_field(row, "correct_answer", ""),
          grading_rubric = row_field(row, "grading_rubric", ""),
          student_answer = row_field(row, "student_answer", ""),
          answer_submitted = isTRUE(submitted_flag),
          attempt_count = row_field(row, "attempt_count", 0L),
          expected_concept_tag = row_field(row, "expected_concept_tag", ""),
          weak_concept_tag = row_field(row, "expected_concept_tag", ""),
          mode = mode,
          professor_id = professor_id
        )
        feedback <- generate_contextual_practice_help(
          help_mode = practice_help_mode,
          practice_context = practice_context,
          help_question = question,
          active_module_id = active_module_id,
          mode = mode,
          professor_id = professor_id,
          use_llm = !isTRUE(dry_run)
        )
        answer <- feedback$answer %||% ""
        retrieval <- list(
          evidence = feedback$evidence_used %||% tibble(),
          normalized_query = feedback$normalized_query %||% normalize_student_query(feedback$retrieval_query %||% question),
          expanded_queries = feedback$expanded_queries %||% expand_query(feedback$retrieval_query %||% question),
          active_module_id = feedback$active_module_id %||% active_module_id,
          inferred_module_id = feedback$inferred_module_id %||% NA_character_,
          expanded_outside_active = isTRUE(feedback$expanded_outside_active),
          retrieval_trace = feedback$retrieval_trace %||% tibble()
        )
        visuals <- feedback$visuals_used %||% tibble()
      } else {
        retrieval <- retrieve_evidence(
          query = question,
          active_module_id = active_module_id,
          mode = mode,
          professor_id = professor_id
        )

        visuals <- retrieve_relevant_visuals(
          query = question,
          active_module_id = active_module_id,
          top_k = 3L
        )

        if (isTRUE(dry_run)) {
          answer <- "[dry-run retrieval only]"
          feedback <- list(
            answer = answer,
            confidence = "retrieval_only",
            needs_clarification = NA,
            hallucination_check = "not_run",
            hallucination_score = NA_real_,
            llm_error = "dry_run"
          )
        } else {
          feedback <- generate_grounded_feedback(
            query = question,
            active_module_id = active_module_id,
            mode = mode,
            professor_id = professor_id,
            use_llm = TRUE
          )
          answer <- feedback$answer %||% ""
        }
      }

      evidence <- retrieval$evidence %||% tibble()
      evidence_chunk_ids <- if ("chunk_id" %in% names(evidence)) evidence$chunk_id else character()
      evidence_concept_tags <- if ("concept_tag" %in% names(evidence)) evidence$concept_tag else character()
      evidence_module_ids <- if ("module_id" %in% names(evidence)) evidence$module_id else character()
      evidence_source_scopes <- if ("source_scope" %in% names(evidence)) evidence$source_scope else character()
      evidence_source_types <- if ("source_type" %in% names(evidence)) evidence$source_type else character()
      evidence_scores <- if ("final_score" %in% names(evidence)) evidence$final_score else numeric()
      evidence_text <- if ("normalized_text" %in% names(evidence)) evidence$normalized_text else character()

      metadata[[i]] <- list(
        id = row$id[[1]],
        dry_run = isTRUE(dry_run),
        normalized_question = retrieval$normalized_query %||% "",
        expanded_queries = paste(retrieval$expanded_queries %||% character(), collapse = " | "),
        active_module_id = retrieval$active_module_id %||% active_module_id %||% "",
        inferred_module_id = retrieval$inferred_module_id %||% "",
        expanded_outside_active = isTRUE(retrieval$expanded_outside_active),
        retrieved_chunk_ids = paste(head(evidence_chunk_ids, 10), collapse = "|"),
        retrieved_concept_tags = paste(unique(evidence_concept_tags), collapse = "|"),
        retrieved_module_ids = paste(unique(evidence_module_ids), collapse = "|"),
        top_module_id = if (length(evidence_module_ids) > 0) evidence_module_ids[[1]] else "",
        source_scopes = paste(unique(evidence_source_scopes), collapse = "|"),
        source_types = paste(unique(evidence_source_types), collapse = "|"),
        top_scores = paste(round(head(evidence_scores, 10), 3), collapse = "|"),
        evidence_text = paste(head(evidence_text, 5), collapse = " "),
        confidence = feedback$confidence %||% "unknown",
        needs_clarification = feedback$needs_clarification %||% NA,
        hallucination_check = feedback$hallucination_check %||% "not_run",
        hallucination_score = feedback$hallucination_score %||% NA_real_,
        visual_ids = if (is.data.frame(visuals) && nrow(visuals) > 0) paste(visuals$image_id, collapse = "|") else "",
        visual_scores = if (is.data.frame(visuals) && nrow(visuals) > 0 && "final_visual_score" %in% names(visuals)) paste(round(visuals$final_visual_score, 3), collapse = "|") else "",
        practice_help_mode = if (isTRUE(has_practice_context)) practice_help_mode else "",
        current_question_id = feedback$current_question_id %||% row_field(row, "current_question_id", ""),
        retrieval_query = feedback$retrieval_query %||% question,
        answer_submitted = isTRUE(feedback$answer_submitted),
        answer_withheld = isTRUE(feedback$answer_withheld),
        correct_answer = row_field(row, "correct_answer", ""),
        llm_error = feedback$llm_error %||% NA_character_
      )
      results[[i]] <- answer
      solver_chat[[i]] <- make_vitals_chat_log(input = input[[i]], result = answer)
    }

    list(
      result = results,
      solver_chat = solver_chat,
      solver_metadata = metadata
    )
  }
}

score_retrieval_concept <- function(sample_row) {
  meta <- get_sample_meta(sample_row)
  expected <- sample_row$expected_concept_tag[[1]] %||% NA_character_
  expected_module <- sample_row$expected_module_id[[1]] %||% NA_character_

  if (is.na(expected) || !nzchar(expected)) {
    pass <- isTRUE(sample_row$expected_behavior[[1]] %in% c("clarify", "refuse", "nudge"))
    return(list(score = score_label(pass, partial = TRUE), pass = pass, notes = "No specific concept expected."))
  }

  evidence <- tibble(
    chunk_id = split_meta_field(meta$retrieved_chunk_ids)
  )
  if (nrow(evidence) == 0) {
    evidence <- tibble(chunk_id = character())
  }
  tag_values <- split_meta_field(meta$retrieved_concept_tags)
  module_values <- split_meta_field(meta$retrieved_module_ids)
  evidence <- evidence %>%
    mutate(
      concept_tag = if (length(tag_values) > 0) tag_values[pmin(row_number(), length(tag_values))] else "",
      module_id = if (length(module_values) > 0) module_values[pmin(row_number(), length(module_values))] else "",
      normalized_text = meta$evidence_text %||% "",
      final_score = suppressWarnings(as.numeric(split_meta_field(meta$top_scores)[1] %||% NA_character_))
    )
  custom_pass <- tryCatch(
    check_retrieval_has_expected_concept(
      list(evidence = evidence),
      expected_concept = expected,
      expected_module = expected_module
    ),
    error = function(e) FALSE
  )

  tags <- split_meta_field(meta$retrieved_concept_tags)
  evidence_text <- normalize_chunk_text(meta$evidence_text %||% "")
  expected_norm <- normalize_chunk_text(expected)
  direct_match <- expected %in% tags || str_detect(evidence_text, fixed(expected_norm, ignore_case = TRUE))
  partial <- !direct_match && !is.na(expected_module) && expected_module %in% split_meta_field(meta$retrieved_module_ids)

  pass <- isTRUE(custom_pass || direct_match)
  list(
    score = score_label(pass, partial),
    pass = pass,
    notes = if (pass) "Retrieved evidence matched the expected concept." else "Expected concept was not clearly present in retrieved evidence."
  )
}

score_grounded_answer <- function(sample_row) {
  meta <- get_sample_meta(sample_row)
  expected_behavior <- sample_row$expected_behavior[[1]] %||% "answer"
  dry_run <- isTRUE(meta$dry_run)
  if (dry_run) {
    retrieval_score <- score_retrieval_concept(sample_row)
    return(list(
      score = if (identical(retrieval_score$score, "C")) "P" else retrieval_score$score,
      pass = NA,
      notes = "Dry run: answer grounding not evaluated beyond retrieval."
    ))
  }

  faithfulness <- meta$hallucination_check %||% "fail"
  needs_clarification <- isTRUE(meta$needs_clarification)
  required_scope <- sample_row$required_source_scope[[1]] %||% "either"
  source_scopes <- split_meta_field(meta$source_scopes)
  source_ok <- required_scope %in% c("either", NA_character_) || required_scope %in% source_scopes
  custom_answer_ok <- tryCatch(
    {
      if (exists("check_answer_uses_evidence", mode = "function")) {
        check_answer_uses_evidence(list(
          answer = sample_row$result[[1]] %||% "",
          evidence_used = tibble(text = meta$evidence_text %||% ""),
          needs_clarification = needs_clarification
        ))
      } else {
        FALSE
      }
    },
    error = function(e) FALSE
  )

  pass <- ((identical(faithfulness, "pass") || isTRUE(custom_answer_ok)) && source_ok) ||
    (expected_behavior %in% c("clarify", "refuse", "nudge") && needs_clarification)
  partial <- identical(faithfulness, "pass") || isTRUE(custom_answer_ok) || source_ok || expected_behavior %in% c("clarify", "refuse", "nudge")

  list(
    score = score_label(pass, partial),
    pass = pass,
    notes = glue("Faithfulness={faithfulness}; custom_answer_ok={custom_answer_ok}; required_scope={required_scope}; source_ok={source_ok}.")
  )
}

score_refusal_behavior <- function(sample_row) {
  meta <- get_sample_meta(sample_row)
  expected_behavior <- sample_row$expected_behavior[[1]] %||% "answer"
  result <- str_to_lower(sample_row$result[[1]] %||% "")
  needs_clarification <- isTRUE(meta$needs_clarification)
  dry_run <- isTRUE(meta$dry_run)

  if (dry_run && expected_behavior %in% c("clarify", "refuse", "nudge")) {
    return(list(score = "P", pass = NA, notes = "Dry run: refusal/nudge behavior not fully evaluated."))
  }

  if (identical(expected_behavior, "answer")) {
    pass <- !needs_clarification && !str_detect(result, "cannot|can't|not enough|clarify|final answer")
    return(list(score = score_label(pass, partial = !needs_clarification), pass = pass, notes = "Expected a substantive answer."))
  }

  if (identical(expected_behavior, "clarify")) {
    pass <- needs_clarification || str_detect(result, "clarify|more context|add|which|not enough|could not find")
    return(list(score = score_label(pass), pass = pass, notes = "Expected clarification when evidence or wording is weak."))
  }

  if (identical(expected_behavior, "refuse")) {
    pass <- str_detect(result, "should not|can't|cannot|won't|not provide|not give")
    return(list(score = score_label(pass), pass = pass, notes = "Expected refusal."))
  }

  if (identical(expected_behavior, "nudge")) {
    coaching <- str_detect(result, "should not|not give|rather than|instead|i can help|setup|reason|nudge|next step|guiding|try|focus")
    correct_answer <- str_to_lower(meta$correct_answer %||% "")
    final_answer_leak <- str_detect(result, "the final answer is|answer is\\s*[:=]") ||
      (nzchar(correct_answer) && str_detect(result, fixed(correct_answer, ignore_case = TRUE)) && isTRUE(meta$answer_withheld))
    answer_safety <- isTRUE(meta$answer_withheld) || !final_answer_leak
    pass <- coaching && answer_safety && !final_answer_leak
    return(list(score = score_label(pass, partial = coaching || answer_safety), pass = pass, notes = "Expected coaching/nudge behavior, not final-answer leakage."))
  }

  list(score = "P", pass = NA, notes = "Unknown expected behavior.")
}

score_module_routing <- function(sample_row) {
  meta <- get_sample_meta(sample_row)
  active <- sample_row$active_module_id[[1]] %||% ""
  expected <- sample_row$expected_module_id[[1]] %||% NA_character_
  inferred <- meta$inferred_module_id %||% ""
  retrieved_modules <- split_meta_field(meta$retrieved_module_ids)
  top_module <- meta$top_module_id %||% ""

  if (is.na(expected) || !nzchar(expected)) {
    pass <- isTRUE(meta$needs_clarification) || length(retrieved_modules) == 0
    return(list(score = score_label(pass, partial = TRUE), pass = pass, notes = "No module expected for this case."))
  }

  related <- get_related_modules(expected)
  expected_present <- expected %in% c(inferred, retrieved_modules, top_module) || any(retrieved_modules %in% related)
  wrong_active_dominates <- nzchar(active) && !identical(active, expected) && identical(top_module, active)
  pass <- expected_present && !wrong_active_dominates
  partial <- expected_present || !wrong_active_dominates

  list(
    score = score_label(pass, partial),
    pass = pass,
    notes = glue("active={active}; inferred={inferred}; top={top_module}; expected={expected}.")
  )
}

score_notation_robustness <- function(sample_row) {
  if (!isTRUE(sample_row$notation_variants_present[[1]]) && !isTRUE(sample_row$spelling_errors_present[[1]])) {
    return(list(score = "C", pass = TRUE, notes = "No notation or spelling robustness required."))
  }

  meta <- get_sample_meta(sample_row)
  normalized <- meta$normalized_question %||% normalize_student_query(sample_row$question[[1]])
  expected <- sample_row$expected_concept_tag[[1]] %||% ""

  notation_ok <- if (isTRUE(sample_row$notation_variants_present[[1]])) {
    str_detect(normalized, "p_hat|p_0|x_bar|mu_0|z_star|t_star|standard error|confidence interval|hypothesis test")
  } else {
    TRUE
  }
  spelling_ok <- if (isTRUE(sample_row$spelling_errors_present[[1]])) {
    str_detect(normalized, "hypothesis test|confidence|significance|standard")
  } else {
    TRUE
  }
  concept_score <- score_retrieval_concept(sample_row)
  pass <- notation_ok && spelling_ok && identical(concept_score$score, "C")
  partial <- notation_ok && spelling_ok

  list(
    score = score_label(pass, partial),
    pass = pass,
    notes = glue("normalized='{normalized}'; expected_concept={expected}.")
  )
}

score_visual_relevance <- function(sample_row) {
  visual_expected <- isTRUE(sample_row$visual_expected[[1]])
  meta <- get_sample_meta(sample_row)
  visual_ids <- split_meta_field(meta$visual_ids)

  if (!visual_expected) {
    return(list(score = "C", pass = TRUE, notes = "No visual expected."))
  }

  pass <- length(visual_ids) > 0
  list(
    score = score_label(pass),
    pass = pass,
    notes = if (pass) glue("Retrieved visual metadata: {paste(visual_ids, collapse = ', ')}") else "No relevant visual metadata was retrieved."
  )
}

make_stat2331_scorer <- function() {
  function(samples, ...) {
    metadata <- vector("list", nrow(samples))
    scores <- character(nrow(samples))

    for (i in seq_len(nrow(samples))) {
      row <- samples[i, ]
      dimensions <- list(
        retrieval_concept = score_retrieval_concept(row),
        grounded_answer = score_grounded_answer(row),
        refusal_behavior = score_refusal_behavior(row),
        module_routing = score_module_routing(row),
        notation_robustness = score_notation_robustness(row),
        visual_relevance = score_visual_relevance(row)
      )

      labels <- vapply(dimensions, `[[`, character(1), "score")
      overall <- if (any(labels == "I")) {
        "I"
      } else if (all(labels == "C")) {
        "C"
      } else {
        "P"
      }
      scores[[i]] <- overall
      metadata[[i]] <- list(
        dimension_scores = as.list(labels),
        dimension_notes = map(dimensions, "notes"),
        dimension_pass = map(dimensions, "pass")
      )
    }

    list(
      score = vitals_score_factor(scores),
      scorer_metadata = metadata
    )
  }
}

summarize_vitals_results <- function(samples) {
  if (!is.data.frame(samples) || nrow(samples) == 0) {
    return(tibble())
  }

  sample_rows <- map(seq_len(nrow(samples)), function(i) samples[i, ])
  dimension_rows <- map_dfr(seq_along(sample_rows), function(i) {
    scorer_meta <- if ("scorer_metadata" %in% names(samples)) samples$scorer_metadata[[i]] else list()
    solver_meta <- if ("solver_metadata" %in% names(samples)) samples$solver_metadata[[i]] else list()
    dims <- scorer_meta$dimension_scores %||% list()
    tibble(
      id = samples$id[[i]],
      question = samples$question[[i]],
      active_module_id = samples$active_module_id[[i]],
      expected_module_id = samples$expected_module_id[[i]],
      expected_behavior = samples$expected_behavior[[i]],
      overall_score = as.character(samples$score[[i]] %||% NA_character_),
      retrieval_concept = dims$retrieval_concept %||% NA_character_,
      grounded_answer = dims$grounded_answer %||% NA_character_,
      refusal_behavior = dims$refusal_behavior %||% NA_character_,
      module_routing = dims$module_routing %||% NA_character_,
      notation_robustness = dims$notation_robustness %||% NA_character_,
      visual_relevance = dims$visual_relevance %||% NA_character_,
      inferred_module_id = solver_meta$inferred_module_id %||% NA_character_,
      retrieved_chunk_ids = solver_meta$retrieved_chunk_ids %||% "",
      visual_ids = solver_meta$visual_ids %||% "",
      practice_help_mode = solver_meta$practice_help_mode %||% "",
      current_question_id = solver_meta$current_question_id %||% "",
      retrieval_query = solver_meta$retrieval_query %||% "",
      answer_submitted = isTRUE(solver_meta$answer_submitted),
      answer_withheld = isTRUE(solver_meta$answer_withheld),
      dry_run = isTRUE(solver_meta$dry_run),
      result = samples$result[[i]] %||% ""
    )
  })

  dimension_rows
}

run_vitals_eval <- function(dataset = build_vitals_dataset(),
                            log_dir = "data/processed/vitals_logs",
                            summary_path = "data/processed/vitals_summary.csv",
                            summary_rds_path = "data/processed/vitals_summary.rds",
                            dry_run = !has_llm_key(),
                            use_dense = !isTRUE(dry_run),
                            view = FALSE) {
  require_vitals()
  ensure_eval_dependencies()
  old_disable_dense <- getOption("stat2331.disable_dense_retrieval", FALSE)
  options(stat2331.disable_dense_retrieval = !isTRUE(use_dense))
  on.exit(options(stat2331.disable_dense_retrieval = old_disable_dense), add = TRUE)

  fs::dir_create(log_dir)
  fs::dir_create(fs::path_dir(summary_path))
  fs::dir_create(fs::path_dir(summary_rds_path))
  log_dir <- normalizePath(log_dir, winslash = "/", mustWork = TRUE)
  Sys.setenv(VITALS_LOG_DIR = log_dir)

  if (!"input" %in% names(dataset)) {
    dataset$input <- dataset$question
  }
  if (!"target" %in% names(dataset)) {
    dataset$target <- dataset$expected_behavior
  }

  task <- vitals::Task$new(
    dataset = dataset,
    solver = make_vitals_solver(dataset, dry_run = dry_run),
    scorer = make_stat2331_scorer(),
    metrics = list(
      accuracy = function(score) mean(as.character(score) == "C", na.rm = TRUE),
      partial_or_better = function(score) mean(as.character(score) %in% c("P", "C"), na.rm = TRUE)
    ),
    name = "stat2331_rag_tutor",
    dir = log_dir
  )

  task$eval(view = view)
  samples <- task$get_samples()
  summary <- summarize_vitals_results(samples)
  readr::write_csv(summary, summary_path)
  saveRDS(summary, summary_rds_path)

  invisible(list(
    task = task,
    samples = samples,
    summary = summary,
    log_dir = log_dir,
    summary_path = summary_path,
    summary_rds_path = summary_rds_path,
    dry_run = dry_run
  ))
}

open_vitals_view <- function(log_dir = "data/processed/vitals_logs") {
  require_vitals()
  if (!dir.exists(log_dir)) {
    stop("Vitals log directory does not exist. Run run_vitals_eval() first.", call. = FALSE)
  }
  log_dir <- normalizePath(log_dir, winslash = "/", mustWork = TRUE)
  view_fun <- get("vitals_view", envir = asNamespace("vitals"))
  view_args <- names(formals(view_fun))

  if ("log_dir" %in% view_args) {
    return(view_fun(log_dir = log_dir))
  }
  if ("dir" %in% view_args) {
    return(view_fun(dir = log_dir))
  }
  view_fun(log_dir)
}
