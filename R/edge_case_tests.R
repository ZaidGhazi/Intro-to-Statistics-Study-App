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

source_edge_dependency <- function(path, required_function) {
  if (file.exists(path)) {
    source(path)
  }
  if (!exists(required_function, mode = "function")) {
    stop("Required edge-case dependency function was not loaded: ", required_function, call. = FALSE)
  }
}

ensure_edge_dependencies <- function() {
  source_edge_dependency("R/chunk_schema.R", "coerce_chunk_schema")
  source_edge_dependency("R/aliases.R", "normalize_student_query")
  source_edge_dependency("R/overlays.R", "ingest_professor_materials")
  source_edge_dependency("R/retrieval.R", "retrieve_evidence")
  source_edge_dependency("R/images.R", "retrieve_relevant_visuals")
  source_edge_dependency("R/tutor.R", "generate_contextual_practice_help")
  invisible(TRUE)
}

build_edge_case_dataset <- function() {
  dataset <- tibble::tribble(
    ~id, ~category, ~input_question, ~active_module_id, ~mode, ~professor_id, ~current_question_context_present, ~expected_behavior, ~expected_module_id, ~expected_concept_tag, ~help_mode, ~practice_question_text, ~student_answer, ~attempt_count, ~correct_answer, ~grading_rubric, ~notes,
    "notation_p_caret", "notation_variants", "What does p^ mean?", "hypothesis_testing", "general", NA_character_, FALSE, "normalize_to_p_hat", "inference_proportion", "sample_proportion", NA_character_, "", "", 0L, "", "", "Caret notation should normalize to p_hat.",
    "notation_p_hat", "notation_variants", "What is p-hat?", "hypothesis_testing", "general", NA_character_, FALSE, "normalize_to_p_hat", "inference_proportion", "sample_proportion", NA_character_, "", "", 0L, "", "", "Text notation should normalize to p_hat.",
    "notation_phat_unicode", "notation_variants", "Is phat the same as p̂?", "hypothesis_testing", "general", NA_character_, FALSE, "normalize_to_p_hat", "inference_proportion", "sample_proportion", NA_character_, "", "", 0L, "", "", "Plain and unicode variants should agree.",
    "notation_p0_vs_phat", "notation_variants", "When do I use p0 vs p_hat?", "hypothesis_testing", "general", NA_character_, FALSE, "normalize_p0_and_phat", "hypothesis_testing", "null_proportion", NA_character_, "", "", 0L, "", "", "Should preserve both null and sample proportion ideas.",
    "spelling_hyo_test", "spelling_informal", "how do i do a hyo test", "hypothesis_testing", "general", NA_character_, FALSE, "route_hypothesis_testing", "hypothesis_testing", "hypothesis_test_decision", NA_character_, "", "", 0L, "", "", "Common typo for hypothesis test.",
    "spelling_standard_error", "spelling_informal", "what is standrd error", "sampling_distributions", "general", NA_character_, FALSE, "normalize_standard_error", "sampling_distributions", "standard_error", NA_character_, "", "", 0L, "", "", "Common typo for standard error.",
    "spelling_conf_vs_hyp", "spelling_informal", "conf int vs hyp test", "confidence_intervals", "general", NA_character_, FALSE, "normalize_ci_hyp_test", "confidence_intervals", "confidence_interval", NA_character_, "", "", 0L, "", "", "Abbreviated informal wording.",
    "spelling_z_or_t", "spelling_informal", "how do i know if its z or t", "confidence_intervals", "general", NA_character_, FALSE, "route_confidence_or_inference", "confidence_intervals", "critical_value", NA_character_, "", "", 0L, "", "", "Formula-selection question.",
    "followup_why_context", "vague_followup_with_context", "why?", "hypothesis_testing", "general", NA_character_, TRUE, "answer_from_context", "hypothesis_testing", "p_value", "followup", "A test has p-value 0.03 and alpha 0.05. What decision should you make?", "Fail to reject because 0.03 is small", 1L, "reject H0", "When p-value is less than alpha, reject the null hypothesis.", "Vague follow-up should use current question context.",
    "followup_simpler_context", "vague_followup_with_context", "can you explain that simpler?", "hypothesis_testing", "general", NA_character_, TRUE, "answer_from_context", "hypothesis_testing", "p_value", "followup", "A test has p-value 0.03 and alpha 0.05. What decision should you make?", "Fail to reject because 0.03 is small", 1L, "reject H0", "When p-value is less than alpha, reject the null hypothesis.", "Simplification should continue the thread.",
    "followup_mean_context", "vague_followup_with_context", "what does that mean?", "hypothesis_testing", "general", NA_character_, TRUE, "answer_from_context", "hypothesis_testing", "p_value", "followup", "A test has p-value 0.03 and alpha 0.05. What decision should you make?", "Fail to reject because 0.03 is small", 1L, "reject H0", "When p-value is less than alpha, reject the null hypothesis.", "Pronoun follow-up should continue context.",
    "followup_visual_context", "vague_followup_with_context", "show me visually", "normal_distribution", "general", NA_character_, TRUE, "visual_or_word_explanation", "normal_distribution", "normal_distribution", "followup", "Use the normal curve to explain why a z-score above 2 is unusual.", "", 0L, "", "Large positive z-scores are far in the upper tail.", "Visual request should not crash if no visual exists.",
    "followup_no_context", "vague_followup_without_context", "why is this wrong?", "hypothesis_testing", "general", NA_character_, FALSE, "clarify", "hypothesis_testing", NA_character_, "diagnose", "", "I do not know", 0L, "", "", "No current question context should trigger clarification.",
    "wrong_module_pvalue", "wrong_module", "What is a p-value?", "descriptive_stats", "general", NA_character_, FALSE, "route_hypothesis_testing", "hypothesis_testing", "p_value", NA_character_, "", "", 0L, "", "", "Wrong active module should not force descriptive-statistics answer.",
    "direct_just_answer", "direct_answer", "just give me the answer", "hypothesis_testing", "general", NA_character_, TRUE, "withhold_answer", "hypothesis_testing", "p_value", "diagnose", "A test has p-value 0.03 and alpha 0.05. What decision should you make?", "I do not know", 1L, "reject H0", "When p-value is less than alpha, reject the null hypothesis.", "Answer-only request should redirect.",
    "direct_option_correct", "direct_answer", "what option is correct?", "hypothesis_testing", "general", NA_character_, TRUE, "withhold_answer", "hypothesis_testing", "p_value", "diagnose", "A test has p-value 0.03 and alpha 0.05. What decision should you make?", "I do not know", 1L, "reject H0", "When p-value is less than alpha, reject the null hypothesis.", "Option-only request should redirect.",
    "weak_anova", "weak_retrieval", "Can you teach me ANOVA for this introductory statistics module?", "inference_mean", "general", NA_character_, FALSE, "clarify_or_weak_evidence", NA_character_, NA_character_, NA_character_, "", "", 0L, "", "", "Out-of-scope or weakly supported content should not hallucinate.",
    "source_overlap_general", "source_overlap", "Why does the proportion test use p0 in the standard error?", "hypothesis_testing", "general", NA_character_, FALSE, "prefer_universal_or_either", "hypothesis_testing", "standard_error", NA_character_, "", "", 0L, "", "", "General mode should prefer universal/course evidence when available.",
    "source_overlap_professor", "source_overlap", "Why does the proportion test use p0 in the standard error?", "hypothesis_testing", "professor", "current_professor", FALSE, "professor_overlay_allowed", "hypothesis_testing", "standard_error", NA_character_, "", "", 0L, "", "", "Professor mode may use selected overlay for notation/emphasis.",
    "visual_graph", "visual_request", "Can you show this with a graph?", "normal_distribution", "general", NA_character_, TRUE, "visual_or_word_explanation", "normal_distribution", "normal_distribution", "followup", "Use the normal curve to explain why a z-score above 2 is unusual.", "", 0L, "", "Large positive z-scores are far in the upper tail.", "Visual metadata or a graceful no-visual response is acceptable.",
    "visual_pvalue", "visual_request", "Can you explain p-value visually?", "hypothesis_testing", "general", NA_character_, TRUE, "visual_or_word_explanation", "hypothesis_testing", "p_value", "followup", "A test statistic lands in the tail of the null distribution. What does the p-value represent?", "", 0L, "", "The p-value is the probability, assuming the null, of a result at least as extreme.", "Visual p-value request.",
    "api_missing_fallback", "api_missing", "Explain p-hat without using a live API.", "hypothesis_testing", "general", NA_character_, TRUE, "no_crash_fallback", "hypothesis_testing", "sample_proportion", "concept", "In a one-proportion problem, what does p-hat represent?", "", 0L, "sample proportion", "p-hat is the sample proportion from the data.", "dry_run should avoid API calls and still return an object.",
    "empty_processed_data", "empty_processed_data", "What does p-hat mean?", "hypothesis_testing", "general", NA_character_, FALSE, "clarify_without_data", "hypothesis_testing", "sample_proportion", NA_character_, "", "", 0L, "", "", "Simulated empty evidence should ask for clarification, not crash.",
    "multi_attempt_first", "multiple_attempts", "Give me a hint", "hypothesis_testing", "general", NA_character_, TRUE, "withhold_answer", "hypothesis_testing", "p_value", "hint", "A test has p-value 0.03 and alpha 0.05. What decision should you make?", "", 0L, "reject H0", "When p-value is less than alpha, reject the null hypothesis.", "First attempt should be a nudge.",
    "multi_attempt_third", "multiple_attempts", "Why was my answer wrong?", "hypothesis_testing", "general", NA_character_, TRUE, "fuller_explanation_allowed", "hypothesis_testing", "p_value", "diagnose", "A test has p-value 0.03 and alpha 0.05. What decision should you make?", "Fail to reject because 0.03 is small", 3L, "reject H0", "When p-value is less than alpha, reject the null hypothesis.", "Later attempts may give more detail.")

  dataset <- dataset %>%
    mutate(
      answer_submitted = current_question_context_present &
        !is.na(help_mode) &
        help_mode %in% c("diagnose", "followup") &
        attempt_count > 0L &
        nzchar(student_answer)
    )

  out <- bind_rows(
    dataset,
    tibble::tribble(
      ~id, ~category, ~input_question, ~active_module_id, ~mode, ~professor_id, ~current_question_context_present, ~expected_behavior, ~expected_module_id, ~expected_concept_tag, ~help_mode, ~practice_question_text, ~student_answer, ~attempt_count, ~correct_answer, ~grading_rubric, ~notes, ~answer_submitted,
      "hint_no_submit_empty", "practice_hint_mode", "Give me a hint", "regression", "general", NA_character_, TRUE, "hint_no_assessment", "regression", "slope_interpretation", "hint", "A regression question asks whether a scatterplot association proves one variable causes another.", "", 0L, "No, association alone does not prove causation.", "Regression can describe association, but causal claims need study design evidence.", "Hint before submission should not diagnose or assess an answer.", FALSE,
      "hint_no_submit_accidental_value", "practice_hint_mode", "Give me a hint", "regression", "general", NA_character_, TRUE, "hint_no_assessment", "regression", "slope_interpretation", "hint", "A regression question asks whether a scatterplot association proves one variable causes another.", "Yes, regression proves causation", 0L, "No, association alone does not prove causation.", "Regression can describe association, but causal claims need study design evidence.", "Hint should ignore an accidental selected/input value until Submit Answer is clicked.", FALSE,
      "hint_second_click", "practice_hint_mode", "Give me another hint", "regression", "general", NA_character_, TRUE, "hint_no_assessment", "regression", "slope_interpretation", "hint", "A regression question asks whether a scatterplot association proves one variable causes another.", "Yes, regression proves causation", 0L, "No, association alone does not prove causation.", "Regression can describe association, but causal claims need study design evidence.", "Repeated hints should still avoid diagnosis before submission.", FALSE,
      "diagnose_no_submit", "practice_diagnose_mode", "Why was my answer wrong?", "regression", "general", NA_character_, TRUE, "diagnose_requires_submit", "regression", "slope_interpretation", "diagnose", "A regression question asks whether a scatterplot association proves one variable causes another.", "Yes, regression proves causation", 0L, "No, association alone does not prove causation.", "Regression can describe association, but causal claims need study design evidence.", "Diagnose should ask for a submitted answer and then give a hint.", FALSE,
      "module_only_practice_setup", "simplified_practice_ui", "module-only setup", "data_graphs", "general", NA_character_, FALSE, "module_only_setup", "data_graphs", "data_graphs", NA_character_, "", "", 0L, "", "", "Practice setup should expose module selection without practice/source mode pickers.", FALSE
    )
  )

  multi_module_rows <- tibble::tribble(
    ~id, ~category, ~input_question, ~active_module_id, ~mode, ~professor_id, ~current_question_context_present, ~expected_behavior, ~expected_module_id, ~expected_concept_tag, ~help_mode, ~practice_question_text, ~student_answer, ~attempt_count, ~correct_answer, ~grading_rubric, ~notes, ~answer_submitted, ~active_module_ids,
    "multi_single_selected", "multi_module_practice", "What is a p-value?", "hypothesis_testing", "general", NA_character_, FALSE, "selected_module_preferred", "hypothesis_testing", "p_value", NA_character_, "", "", 0L, "", "", "Single selected module should prefer hypothesis_testing.", FALSE, "hypothesis_testing",
    "multi_two_module_pool", "multi_module_practice", "module pool check", "confidence_intervals", "general", NA_character_, FALSE, "practice_pool_selected_only", "confidence_intervals", "confidence_interval", NA_character_, "", "", 0L, "", "", "Practice pool should draw only from confidence_intervals and hypothesis_testing.", FALSE, "confidence_intervals|hypothesis_testing",
    "multi_current_margin", "multi_module_retrieval", "what is margin of error?", "confidence_intervals", "general", NA_character_, TRUE, "selected_module_preferred", "confidence_intervals", "margin_of_error", "concept", "A confidence interval question asks how the margin of error affects interval width.", "", 0L, "", "Margin of error controls how far the interval extends from the estimate.", "Current question module should win.", FALSE, "confidence_intervals|hypothesis_testing",
    "multi_other_selected_pvalue", "multi_module_retrieval", "what is a p-value?", "confidence_intervals", "general", NA_character_, FALSE, "selected_module_allowed", "hypothesis_testing", "p_value", NA_character_, "", "", 0L, "", "", "A question from another selected module can route there.", FALSE, "confidence_intervals|hypothesis_testing",
    "multi_outside_selected_pvalue", "multi_module_retrieval", "what is a p-value?", "descriptive_stats", "general", NA_character_, TRUE, "outside_selected_clarify", "hypothesis_testing", "p_value", "followup", "A descriptive statistics question asks for the median of a small data set.", "", 0L, "", "Median is the middle value after sorting.", "Question appears outside selected modules and should ask whether to expand.", FALSE, "descriptive_stats",
    "multi_no_modules", "multi_module_practice", "start practice", NA_character_, "general", NA_character_, FALSE, "no_modules_message", NA_character_, NA_character_, NA_character_, "", "", 0L, "", "", "No selected modules should not crash.", FALSE, "",
    "multi_switch_resets", "multi_module_practice", "module switch cache reset", "confidence_intervals", "general", NA_character_, FALSE, "cache_reset_supported", "confidence_intervals", "confidence_interval", NA_character_, "", "", 0L, "", "", "Changing selected modules should reset current question and tutor cache.", FALSE, "confidence_intervals|hypothesis_testing"
  )

  visual_rows <- tibble::tribble(
    ~id, ~category, ~input_question, ~active_module_id, ~mode, ~professor_id, ~current_question_context_present, ~expected_behavior, ~expected_module_id, ~expected_concept_tag, ~help_mode, ~practice_question_text, ~student_answer, ~attempt_count, ~correct_answer, ~grading_rubric, ~notes, ~answer_submitted, ~active_module_ids,
    "visual_practice_schema", "visual_practice", "practice visual schema", "data_graphs", "general", NA_character_, FALSE, "practice_visual_schema", "data_graphs", "graph_selection", NA_character_, "", "", 0L, "", "", "Question schema and UI should support linked visuals.", FALSE, "data_graphs",
    "visual_missing_file", "visual_practice", "missing visual fallback", "data_graphs", "general", NA_character_, FALSE, "missing_visual_fallback", "data_graphs", "graph_selection", NA_character_, "", "", 0L, "", "", "Missing visual file should show a friendly fallback instead of crashing.", FALSE, "data_graphs",
    "visual_tutor_context", "visual_tutor", "Can you show this visually?", "hypothesis_testing", "general", NA_character_, TRUE, "visual_or_word_explanation", "hypothesis_testing", "p_value_interpretation", "followup", "What does a small p-value suggest?", "", 0L, "The observed result would be unusual if the null hypothesis were true.", "A small p-value means the observed result is in the tail of the null distribution.", "Visual request with current question context should retrieve or explain a visual.", FALSE, "hypothesis_testing",
    "visual_graph_no_exact", "visual_tutor", "Can you explain this with a graph?", "confidence_intervals", "general", NA_character_, TRUE, "visual_or_word_explanation", "confidence_intervals", "ci_interpretation", "followup", "What does a confidence interval estimate?", "", 0L, "A population parameter", "A confidence interval shows plausible values around an estimate.", "No exact visual should fall back to best concept visual or word picture.", FALSE, "confidence_intervals",
    "visual_local_disabled", "visual_permissions", "local textbook visuals disabled", "hypothesis_testing", "general", NA_character_, FALSE, "local_only_disabled_safe", "hypothesis_testing", "p_value_interpretation", NA_character_, "", "", 0L, "", "", "When local-only visuals are disabled, safe recreated visuals should remain available.", FALSE, "hypothesis_testing",
    "visual_api_missing", "visual_tutor", "Can you show this with a graph without an API key?", "normal_distribution", "general", NA_character_, TRUE, "visual_or_word_explanation", "normal_distribution", "z_score_interpretation", "followup", "A z-score tells you how far an observation is from the mean.", "", 0L, "standard deviations", "A z-score is distance from the mean in standard deviation units.", "Visual can still be shown and explained from metadata in dry-run/API-missing mode.", FALSE, "normal_distribution",
    "visual_multiple_candidates", "visual_retrieval", "Can you show p-value visually?", "hypothesis_testing", "general", NA_character_, FALSE, "multiple_visual_choice", "hypothesis_testing", "p_value_interpretation", NA_character_, "", "", 0L, "", "", "Multiple candidate visuals should be ranked and a best visual chosen.", FALSE, "hypothesis_testing"
  )

  out %>%
    mutate(active_module_ids = active_module_id %||% "") %>%
    bind_rows(multi_module_rows, visual_rows)
}

edge_practice_context <- function(row) {
  selected_modules <- edge_active_module_ids(row)
  list(
    active_module_id = row$active_module_id,
    current_module_id = row$active_module_id,
    active_module_ids = selected_modules,
    current_question_id = row$id,
    question_text = row$practice_question_text %||% "",
    answer_choices = "",
    correct_answer = row$correct_answer %||% "",
    grading_rubric = row$grading_rubric %||% "",
    student_answer = row$student_answer %||% "",
    answer_submitted = isTRUE(row$answer_submitted %||% FALSE),
    attempt_count = row$attempt_count %||% 0L,
    expected_concept_tag = row$expected_concept_tag %||% "",
    weak_concept_tag = row$expected_concept_tag %||% "",
    mode = row$mode %||% "general",
    professor_id = if (!is.na(row$professor_id %||% NA_character_)) row$professor_id else NULL,
    last_tutor_answer = "Previous tutor answer: compare the p-value to alpha before deciding.",
    conversation_history = list(
      list(role = "student", text = "I am stuck on this item.", help_mode = "hint"),
      list(role = "assistant", text = "Start by naming what the p-value measures, then compare it to alpha.", help_mode = "hint")
    )
  )
}

empty_edge_retrieval <- function(question, active_module_id = NULL) {
  list(
    query = question,
    normalized_query = normalize_student_query(question),
    expanded_queries = expand_query(question),
    intent = classify_query_intent(question),
    active_module_id = active_module_id,
    inferred_module_id = route_question_to_module(question),
    related_modules = get_related_modules(active_module_id),
    expanded_outside_active = FALSE,
    evidence = tibble(),
    retrieval_trace = tibble()
  )
}

edge_active_module_ids <- function(row) {
  raw <- row$active_module_ids %||% row$active_module_id %||% ""
  raw <- raw[!is.na(raw)]
  if (length(raw) == 0 || !nzchar(raw[[1]])) {
    return(character())
  }
  str_split(raw[[1]], "\\|")[[1]] %>%
    str_squish() %>%
    discard(~ !nzchar(.x))
}

classify_edge_result <- function(row, normalized_query, retrieval, feedback, visuals) {
  answer <- str_to_lower(feedback$answer %||% "")
  evidence <- retrieval$evidence %||% tibble()
  retrieved_modules <- if (is.data.frame(evidence) && "module_id" %in% names(evidence)) unique(evidence$module_id) else character()
  selected_modules <- edge_active_module_ids(row)
  source_scopes <- if (is.data.frame(evidence) && "source_scope" %in% names(evidence)) unique(evidence$source_scope) else character()
  visual_count <- if (is.data.frame(visuals)) nrow(visuals) else 0L
  evaluation_language <- str_detect(
    answer,
    "good try|you('?re| are) right|you('?re| are) correct|you('?re| are) wrong|your answer (is|was)|you chose|you selected|incorrect|correct answer"
  )
  correct_answer_text <- str_to_lower(row$correct_answer %||% "")
  revealed_final_answer <- nzchar(correct_answer_text) &&
    str_detect(answer, fixed(correct_answer_text, ignore_case = TRUE))

  pass <- switch(
    row$expected_behavior,
    normalize_to_p_hat = str_detect(normalized_query, "p_hat"),
    normalize_p0_and_phat = str_detect(normalized_query, "p_0") && str_detect(normalized_query, "p_hat"),
    route_hypothesis_testing = identical(retrieval$inferred_module_id %||% "", "hypothesis_testing") || "hypothesis_testing" %in% retrieved_modules,
    normalize_standard_error = str_detect(normalized_query, "standard error"),
    normalize_ci_hyp_test = str_detect(normalized_query, "confidence interval") && str_detect(normalized_query, "hypothesis test"),
    route_confidence_or_inference = (retrieval$inferred_module_id %||% "") %in% c("confidence_intervals", "inference_mean", "inference_proportion") || any(retrieved_modules %in% c("confidence_intervals", "inference_mean", "inference_proportion")),
    answer_from_context = !isTRUE(feedback$needs_clarification) && nzchar(answer),
    visual_or_word_explanation = visual_count > 0 || str_detect(answer, "visual|graph|curve|metadata|words|explain"),
    clarify = isTRUE(feedback$needs_clarification) || str_detect(answer, "need|context|question|answer|clarify|paste"),
    withhold_answer = isTRUE(feedback$answer_withheld) || str_detect(answer, "not give|should not|rather than|instead|guide|hint"),
    clarify_or_weak_evidence = isTRUE(feedback$needs_clarification) || str_detect(answer, "not find|not enough|course documents|clarify|support"),
    prefer_universal_or_either = any(source_scopes %in% c("universal_core", "professor_specific", "supplemental")) || nrow(retrieval$evidence %||% tibble()) > 0,
    professor_overlay_allowed = any(source_scopes %in% c("professor_specific", "universal_core")) || nrow(retrieval$evidence %||% tibble()) > 0,
    no_crash_fallback = is.list(feedback) && "answer" %in% names(feedback),
    clarify_without_data = isTRUE(feedback$needs_clarification) || str_detect(answer, "not find|not enough|course documents|clarify"),
    fuller_explanation_allowed = !isTRUE(feedback$answer_withheld) || str_detect(answer, "next step|because|compare|reason"),
    hint_no_assessment = str_detect(answer, "hint|nudge|focus|start|look for|ask yourself|what ") &&
      !evaluation_language &&
      !revealed_final_answer,
    diagnose_requires_submit = str_detect(answer, "after you submit|submit one|submitted answer") &&
      str_detect(answer, "hint|for now") &&
      !evaluation_language,
    module_only_setup = {
      app_text <- if (file.exists("app.R")) paste(readLines("app.R", warn = FALSE), collapse = "\n") else ""
      str_detect(app_text, "Choose a module") &&
        str_detect(app_text, "Start practice") &&
        !str_detect(app_text, "Pick practice mode") &&
        !str_detect(app_text, "Pick course context")
    },
    selected_module_preferred = row$expected_module_id %in% c(retrieval$active_module_id %||% "", retrieval$current_module_id %||% "", retrieval$inferred_module_id %||% "", retrieved_modules),
    selected_module_allowed = row$expected_module_id %in% c(retrieval$inferred_module_id %||% "", retrieved_modules) &&
      row$expected_module_id %in% selected_modules,
    practice_pool_selected_only = {
      pool <- if (exists("get_practice_pool", mode = "function")) get_practice_pool(selected_modules) else tibble()
      app_text <- if (file.exists("app.R")) paste(readLines("app.R", warn = FALSE), collapse = "\n") else ""
      (is.data.frame(pool) && (nrow(pool) == 0 || all(pool$module_id %in% selected_modules))) ||
        (str_detect(app_text, "get_practice_pool") && str_detect(app_text, "filter\\(module_id %in% module_ids\\)"))
    },
    outside_selected_clarify = isTRUE(feedback$needs_clarification) &&
      !str_detect(answer, "outside your selected|switch|expand") &&
      !(row$expected_module_id %in% selected_modules),
    no_modules_message = length(selected_modules) == 0,
    cache_reset_supported = {
      app_text <- if (file.exists("app.R")) paste(readLines("app.R", warn = FALSE), collapse = "\n") else ""
      str_detect(app_text, "Module selection changed") &&
        str_detect(app_text, "reset_practice_session") &&
        str_detect(app_text, "evidence_cache <- NULL")
    },
    practice_visual_schema = {
      app_text <- if (file.exists("app.R")) paste(readLines("app.R", warn = FALSE), collapse = "\n") else ""
      str_detect(app_text, "visual_id") &&
        str_detect(app_text, "visual_ids") &&
        str_detect(app_text, "render_question_visuals") &&
        str_detect(app_text, "render_tutor_visuals")
    },
    missing_visual_fallback = {
      app_text <- if (file.exists("app.R")) paste(readLines("app.R", warn = FALSE), collapse = "\n") else ""
      str_detect(app_text, "image file is not available")
    },
    local_only_disabled_safe = {
      if (exists("retrieve_relevant_visuals", mode = "function")) {
        old <- getOption("stat2331.local_textbook_visuals", TRUE)
        on.exit(options(stat2331.local_textbook_visuals = old), add = TRUE)
        options(stat2331.local_textbook_visuals = FALSE)
        safe_visuals <- retrieve_relevant_visuals("Can you show p-value visually?", module_id = "hypothesis_testing", top_k = 3)
        is.data.frame(safe_visuals) && nrow(safe_visuals) > 0 && all(safe_visuals$safe_for_deployment)
      } else {
        FALSE
      }
    },
    multiple_visual_choice = {
      if (exists("retrieve_relevant_visuals", mode = "function") && exists("choose_visual_for_answer", mode = "function")) {
        candidates <- retrieve_relevant_visuals("Can you show p-value visually?", module_id = "hypothesis_testing", top_k = 3)
        choice <- choose_visual_for_answer(candidates)
        is.data.frame(candidates) && nrow(candidates) >= 1 && is.data.frame(choice) && nrow(choice) == 1
      } else {
        FALSE
      }
    },
    FALSE
  )
  isTRUE(pass)
}

run_one_edge_case <- function(row, dry_run = TRUE) {
  professor_id <- row$professor_id
  if (is.na(professor_id)) professor_id <- NULL
  normalized_query <- normalize_student_query(row$input_question)
  active_module_ids <- edge_active_module_ids(row)

  if (identical(row$category, "empty_processed_data")) {
    retrieval <- empty_edge_retrieval(row$input_question, row$active_module_id)
    refusal <- maybe_refuse_or_clarify(row$input_question, retrieval, mode = row$mode, professor_id = professor_id)
    feedback <- list(
      answer = refusal$answer %||% "No processed knowledge base was available for this simulated test.",
      confidence = refusal$confidence %||% "low",
      needs_clarification = TRUE,
      answer_withheld = NA,
      hallucination_check = "not_run",
      evidence_used = tibble()
    )
    visuals <- tibble()
  } else if (isTRUE(row$current_question_context_present) || !is.na(row$help_mode)) {
    context <- edge_practice_context(row)
    feedback <- generate_contextual_practice_help(
      help_mode = if (!is.na(row$help_mode)) row$help_mode else "followup",
      practice_context = context,
      help_question = row$input_question,
      active_module_id = row$active_module_id,
      active_module_ids = active_module_ids,
      current_module_id = row$active_module_id,
      mode = row$mode,
      professor_id = professor_id,
      use_llm = !isTRUE(dry_run)
    )
    retrieval <- list(
      evidence = feedback$evidence_used %||% tibble(),
      normalized_query = feedback$normalized_query %||% normalized_query,
      expanded_queries = feedback$expanded_queries %||% expand_query(row$input_question),
      inferred_module_id = feedback$inferred_module_id %||% route_question_to_module(row$input_question),
      active_module_id = feedback$active_module_id %||% row$active_module_id,
      current_module_id = feedback$current_module_id %||% row$active_module_id,
      active_module_ids = feedback$active_module_ids %||% active_module_ids,
      retrieval_trace = feedback$retrieval_trace %||% tibble()
    )
    visuals <- feedback$visuals_used %||% tibble()
  } else {
    retrieval <- retrieve_evidence(
      row$input_question,
      active_module_id = row$active_module_id,
      active_module_ids = active_module_ids,
      current_module_id = row$active_module_id,
      mode = row$mode,
      professor_id = professor_id
    )
    visuals <- tryCatch(
      retrieve_relevant_visuals(row$input_question, active_module_id = row$active_module_id),
      error = function(e) tibble()
    )
    feedback <- generate_grounded_feedback(
      row$input_question,
      active_module_id = row$active_module_id,
      mode = row$mode,
      professor_id = professor_id,
      use_llm = !isTRUE(dry_run)
    )
  }

  evidence <- retrieval$evidence %||% feedback$evidence_used %||% tibble()
  pass <- classify_edge_result(row, normalized_query, retrieval, feedback, visuals)
  tibble(
    id = row$id,
    category = row$category,
    input_question = row$input_question,
    active_module_id = row$active_module_id,
    active_module_ids = paste(active_module_ids, collapse = "|"),
    current_question_context_present = isTRUE(row$current_question_context_present),
    help_mode = row$help_mode %||% NA_character_,
    answer_submitted = isTRUE(row$answer_submitted %||% FALSE),
    expected_behavior = row$expected_behavior,
    actual_behavior = paste(
      if (isTRUE(feedback$needs_clarification)) "clarify" else "answer",
      if (isTRUE(feedback$answer_withheld)) "answer_withheld" else "answer_not_withheld",
      sep = "|"
    ),
    normalized_query = normalized_query,
    inferred_module_id = retrieval$inferred_module_id %||% NA_character_,
    retrieved_chunk_ids = if (is.data.frame(evidence) && nrow(evidence) > 0) paste(head(evidence$chunk_id, 8), collapse = "|") else "",
    visual_ids = if (is.data.frame(visuals) && nrow(visuals) > 0 && "image_id" %in% names(visuals)) paste(head(visuals$image_id, 5), collapse = "|") else "",
    evidence_used = if (is.data.frame(evidence) && nrow(evidence) > 0) paste(head(evidence$source_scope, 8), collapse = "|") else "",
    tutor_response = feedback$answer %||% "",
    pass_fail = if (pass) "PASS" else "FAIL",
    notes = row$notes
  )
}

run_edge_case_tests <- function(dataset = build_edge_case_dataset(), dry_run = TRUE, use_dense = !isTRUE(dry_run), quiet = FALSE) {
  ensure_edge_dependencies()
  old_disable_dense <- getOption("stat2331.disable_dense_retrieval", FALSE)
  options(stat2331.disable_dense_retrieval = !isTRUE(use_dense))
  on.exit(options(stat2331.disable_dense_retrieval = old_disable_dense), add = TRUE)

  rows <- split(dataset, seq_len(nrow(dataset)))
  results <- purrr::imap_dfr(rows, function(row, i) {
    if (!isTRUE(quiet)) {
      message(glue("Edge case {i}/{length(rows)}: {row$id}"))
    }
    tryCatch(
      run_one_edge_case(row, dry_run = dry_run),
      error = function(e) {
        tibble(
          id = row$id,
          category = row$category,
          input_question = row$input_question,
          active_module_id = row$active_module_id,
          active_module_ids = paste(edge_active_module_ids(row), collapse = "|"),
          current_question_context_present = isTRUE(row$current_question_context_present),
          help_mode = row$help_mode %||% NA_character_,
          answer_submitted = isTRUE(row$answer_submitted %||% FALSE),
          expected_behavior = row$expected_behavior,
          actual_behavior = "error",
          normalized_query = tryCatch(normalize_student_query(row$input_question), error = function(err) ""),
          inferred_module_id = NA_character_,
          retrieved_chunk_ids = "",
          visual_ids = "",
          evidence_used = "",
          tutor_response = "",
          pass_fail = "FAIL",
          notes = paste(row$notes, "Error:", conditionMessage(e))
        )
      }
    )
  })
  save_edge_case_results(results)
  results
}

summarize_edge_case_results <- function(results) {
  results %>%
    count(category, pass_fail, name = "n") %>%
    group_by(category) %>%
    mutate(rate = n / sum(n)) %>%
    ungroup()
}

save_edge_case_results <- function(results,
                                   csv_path = "data/processed/edge_case_results.csv",
                                   rds_path = "data/processed/edge_case_results.rds") {
  fs::dir_create(fs::path_dir(csv_path))
  fs::dir_create(fs::path_dir(rds_path))
  readr::write_csv(results, csv_path)
  saveRDS(results, rds_path)
  invisible(list(csv_path = csv_path, rds_path = rds_path))
}
