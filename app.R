library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(dplyr)
library(tibble)
library(purrr)
library(stringr)
library(glue)
library(htmltools)

source("R/wiki.R")
source("R/chunk_schema.R")
source("R/aliases.R")
source("R/ingest_textbook.R")
source("R/overlays.R")
source("R/retrieval.R")
source("R/tutor.R")
source("R/images.R")
source("R/visual_helpers.R")
source("R/practice_selection.R")
source("R/vitals_check.R")
source("R/evals_vitals.R")

# =========================
# CONFIG
# =========================
APP_TITLE <- "Introduction to Statistics Study App"
DB_PATH <- "intro_stats_study_app.sqlite"
QUESTION_BANK_PATH <- "data/processed/question_bank.csv"

.app_cache <- new.env(parent = emptyenv())

anonymous_demo_user <- function() {
  list(
    user_id = "demo_user",
    display_name = "Demo user",
    role = "student",
    mode = "practice"
  )
}

student_accounts <- tibble::tribble(
  ~user_id, ~password, ~display_name, ~role,
  "student1", "demo123", "Student One", "student",
  "student2", "demo123", "Student Two", "student",
  "student3", "demo123", "Student Three", "student",
  "student4", "demo123", "Student Four", "student",
  "student5", "demo123", "Student Five", "student",
  "instructor1", "demo123", "Instructor Demo", "instructor"
)

MODULES <- tibble::tribble(
  ~module_id, ~module_label, ~module_order,
  "module_1", "Module 1: Data Types, Graphs, and Descriptive Statistics", 1L,
  "module_2", "Module 2: Relationships, Association, and Regression", 2L,
  "module_3", "Module 3: Producing Data: Sampling and Experiments", 3L,
  "module_4", "Module 4: Probability Basics", 4L,
  "module_5", "Module 5: Normal and Binomial Distributions", 5L,
  "module_6", "Module 6: Sampling Distributions", 6L,
  "module_7", "Module 7: Confidence Intervals", 7L,
  "module_8", "Module 8: Hypothesis Testing", 8L,
  "module_9", "Module 9: Uses and Abuses of Tests", 9L,
  "cumulative_review", "Cumulative Review", 10L
)

TOPIC_META <- tibble::tribble(
  ~topic_id, ~student_label, ~module_id, ~concept_tag, ~topic_order,
  "data_graphs", "Data Types and Graphs", "module_1", "data_graphs", 1L,
  "descriptive_stats", "Descriptive Statistics", "module_1", "descriptive_stats", 2L,
  "relationships_regression", "Relationships and Regression", "module_2", "relationships_regression", 3L,
  "producing_data", "Producing Data", "module_3", "producing_data", 4L,
  "probability_basics", "Probability Basics", "module_4", "probability_basics", 5L,
  "normal_dist", "Normal Distribution", "module_5", "normal_dist", 6L,
  "binomial_dist", "Binomial Distribution", "module_5", "binomial_dist", 7L,
  "sampling_dist", "Sampling Distributions", "module_6", "sampling_dist", 8L,
  "ci_prop", "Confidence Intervals for Proportions", "module_7", "ci_prop", 9L,
  "ci_mean", "Confidence Intervals for Means", "module_7", "ci_mean", 10L,
  "ht_foundations", "Hypothesis Testing Foundations", "module_8", "ht_foundations", 11L,
  "ht_prop", "Hypothesis Tests for Proportions", "module_8", "ht_prop", 12L,
  "ht_mean", "Hypothesis Tests for Means", "module_8", "ht_mean", 13L,
  "uses_abuses_tests", "Uses and Abuses of Tests", "module_9", "uses_abuses_tests", 14L,
  "final_review", "Cumulative Review", "cumulative_review", "final_review", 15L
) %>%
  left_join(MODULES, by = "module_id") %>%
  arrange(module_order, topic_order)

PRACTICE_MODES <- c(
  "Recommended practice" = "recommended",
  "Weak areas" = "weak_areas",
  "Quick review" = "quick_review",
  "Challenge mode" = "challenge"
)

is_development_mode <- function() {
  value <- tolower(Sys.getenv("STAT2331_DEV_MODE", unset = "false"))
  value %in% c("1", "true", "yes", "dev", "development")
}

# =========================
# UTILITIES
# =========================
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

safe_html <- function(x) {
  HTML(x %||% "")
}

first_or_default <- function(x, default = "") {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(default)
  }
  
  x[[1]]
}

first_row_value <- function(data, column, default = NA_character_) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 || !column %in% names(data)) {
    return(default)
  }
  
  values <- data[[column]]
  if (is.null(values) || length(values) == 0 || all(is.na(values))) {
    return(default)
  }
  
  values[[1]]
}

normalize_scalar_string <- function(x) {
  if (is.null(x) || length(x) != 1 || all(is.na(x))) {
    return(NULL)
  }
  
  value <- str_squish(as.character(x[[1]]))
  if (!nzchar(value)) {
    return(NULL)
  }
  
  value
}

current_utc_timestamp <- function(time = Sys.time()) {
  format(as.POSIXct(time, tz = "UTC"), "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

normalize_ts_column <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(df)
  }
  
  if ("ts" %in% names(df)) {
    df$ts <- as.character(df$ts)
  }
  
  df
}

parse_ts_utc <- function(x) {
  suppressWarnings(as.POSIXct(as.character(x), tz = "UTC"))
}

is_valid_topic_id <- function(topic_id, allow_special = FALSE, require_known = FALSE) {
  topic_value <- normalize_scalar_string(topic_id)
  if (is.null(topic_value)) {
    return(FALSE)
  }
  
  if (!allow_special && topic_value %in% c("__auto__", "__weak__")) {
    return(FALSE)
  }
  
  if (isTRUE(require_known) && !topic_value %in% get_topic_meta()$topic_id) {
    return(FALSE)
  }
  
  TRUE
}

sanitize_topic_id <- function(topic_id, allow_special = FALSE, require_known = FALSE) {
  topic_value <- normalize_scalar_string(topic_id)
  if (is.null(topic_value)) {
    return(NULL)
  }
  
  if (!allow_special && topic_value %in% c("__auto__", "__weak__")) {
    return(NULL)
  }
  
  if (isTRUE(require_known) && !topic_value %in% get_topic_meta()$topic_id) {
    return(NULL)
  }
  
  topic_value
}

get_topic_row <- function(topic_id) {
  topic_value <- sanitize_topic_id(topic_id, require_known = TRUE)
  if (is.null(topic_value)) {
    return(get_topic_meta()[0, , drop = FALSE])
  }
  
  get_topic_meta() %>%
    filter(topic_id == !!topic_value) %>%
    slice_head(n = 1)
}

get_topics_for_help_module <- function(module_id = NULL) {
  topics <- get_topic_meta()
  module_value <- normalize_scalar_string(module_id)
  
  if (!is.null(module_value) && module_value %in% MODULES$module_id) {
    module_topics <- topics %>% filter(module_id == !!module_value)
    if (nrow(module_topics) > 0) {
      return(module_topics)
    }
  }
  
  topics
}

get_default_help_topic <- function(module_id = NULL) {
  fallback_row <- get_topics_for_help_module(module_id) %>%
    arrange(module_order, topic_order) %>%
    slice_head(n = 1)
  
  first_row_value(fallback_row, "topic_id", "data_graphs")
}

get_help_failure_message <- function() {
  "I had trouble generating a response, but I saved your question. Try rephrasing it or ask again."
}

compact_help_text <- function(text, default = "") {
  cleaned <- text %||% "" %>%
    as.character() %>%
    str_replace_all("\\r\\n?", "\n") %>%
    str_replace_all("[*_`>#]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
  
  if (!nzchar(cleaned)) {
    cleaned <- default %||% ""
  }
  
  cleaned
}

normalize_help_bullets <- function(x, max_n = 5L) {
  bullets <- unlist(x %||% character(), use.names = FALSE) %>%
    as.character() %>%
    vapply(compact_help_text, default = "", FUN.VALUE = character(1)) %>%
    unique()
  bullets <- bullets[nzchar(bullets)]
  if (length(bullets) == 0) {
    return(character())
  }
  bullets[seq_len(min(length(bullets), max_n))]
}

empty_help_response_object <- function(routed_topic_label = "General course help") {
  list(
    routed_topic_label = compact_help_text(routed_topic_label, "General course help"),
    direct_answer = "",
    remember_bullets = character(),
    analogy = "",
    common_mistake = "",
    next_step = ""
  )
}

normalize_help_response_object <- function(response,
                                           routed_topic_label = NULL,
                                           fallback_topic_label = "General course help") {
  response <- response %||% list()
  topic_label <- compact_help_text(
    routed_topic_label %||% response$routed_topic_label %||% fallback_topic_label,
    fallback_topic_label
  )
  direct_answer <- compact_help_text(
    response$direct_answer,
    "Start by identifying what the question is asking for, then match the notation and method to that goal."
  )
  remember_bullets <- normalize_help_bullets(response$remember_bullets)
  if (length(remember_bullets) == 0) {
    remember_bullets <- c("Match the notation and the question goal before you calculate anything.")
  }
  
  list(
    routed_topic_label = topic_label,
    direct_answer = direct_answer,
    remember_bullets = remember_bullets,
    analogy = compact_help_text(response$analogy, "Think of this as matching the right statistical story to the question before doing the arithmetic."),
    common_mistake = compact_help_text(response$common_mistake, "Do not mix up the notation or method before you start calculating."),
    next_step = compact_help_text(response$next_step, "")
  )
}

serialize_help_response_object <- function(response) {
  normalized <- normalize_help_response_object(response)
  jsonlite::toJSON(normalized, auto_unbox = TRUE, null = "null")
}

parse_help_response_json <- function(text) {
  if (!is.character(text) || length(text) != 1 || !nzchar(str_squish(text))) {
    return(NULL)
  }
  
  trimmed <- str_trim(text)
  parsed <- parse_json_safely(trimmed)
  if (!is.null(parsed)) {
    return(parsed)
  }
  
  stripped <- trimmed %>%
    str_replace("^\\s*```json\\s*", "") %>%
    str_replace("^\\s*```\\s*", "") %>%
    str_replace("\\s*```\\s*$", "") %>%
    str_trim()
  parsed <- parse_json_safely(stripped)
  if (!is.null(parsed)) {
    return(parsed)
  }
  
  start_pos <- str_locate(stripped, fixed("{"))[1]
  end_positions <- str_locate_all(stripped, fixed("}"))[[1]]
  if (is.na(start_pos) || nrow(end_positions) == 0) {
    return(NULL)
  }
  
  end_pos <- end_positions[nrow(end_positions), 1]
  if (is.na(end_pos) || end_pos < start_pos) {
    return(NULL)
  }
  
  parse_json_safely(str_sub(stripped, start_pos, end_pos))
}

normalize_help_markdown <- function(text) {
  safe_text <- as.character(text %||% "") %>%
    str_replace_all("\\r\\n?", "\n") %>%
    str_replace_all("\t", "  ") %>%
    str_replace_all("\n{3,}", "\n\n") %>%
    str_trim()
  
  if (!nzchar(safe_text)) {
    return("")
  }

  safe_text %>%
    str_replace_all("\n{3,}", "\n\n") %>%
    str_trim()
}

build_help_markdown <- function(direct_answer,
                                remember_bullets,
                                analogy,
                                common_mistake,
                                next_step = NULL) {
  bullet_lines <- remember_bullets %||% character()
  bullet_lines <- bullet_lines[nzchar(str_squish(bullet_lines))]
  if (length(bullet_lines) == 0) {
    bullet_lines <- "Match the notation and the question goal before you calculate anything."
  }
  
  sections <- c(
    glue("**Direct answer**\n{compact_help_text(direct_answer, 'Start by identifying what the question is asking for and what notation belongs to it.')}"),
    paste(c("**Remember**", paste0("- ", bullet_lines)), collapse = "\n"),
    glue("**Analogy**\n{compact_help_text(analogy, 'Think of this as checking which story about the data best fits before you compute.')}"),
    glue("**Common mistake**\n{compact_help_text(common_mistake, 'Do not mix up the notation or method before you start calculating.')}")
  )
  
  next_step_text <- compact_help_text(next_step, "")
  if (nzchar(next_step_text)) {
    sections <- c(sections, glue("**Next step**\n{next_step_text}"))
  }
  
  normalize_help_markdown(paste(sections, collapse = "\n\n"))
}

is_p_value_help_query <- function(query_text, topic_id = NULL, concept_tag = NULL) {
  query <- str_to_lower(query_text %||% "")
  concept_tag <- normalize_review_concept_tag(concept_tag, topic_id)
  
  identical(concept_tag, "p_value_interpretation") ||
    (identical(topic_id, "ht_foundations") &&
      str_detect(query, "p value|p-value|null hypothesis|alternative hypothesis|fail to reject|reject the null|alpha|significance"))
}

get_help_analogy <- function(topic_id, concept_tag, query_text) {
  if (is_p_value_help_query(query_text, topic_id, concept_tag)) {
    return("A p-value is like asking how surprising your sample would look if the null model were the true story.")
  }
  
  switch(
    concept_tag %||% "",
    fail_to_reject = "Failing to reject is like saying the evidence was too weak to overturn the default story, not proving the default story is true.",
    binomial_conditions = "The BINS check is like a pre-flight list: if the conditions fail, the binomial model should not take off.",
    binomial_at_least_at_most = "Translate binomial wording the way you would shade a number line before choosing the calculator command.",
    ci_interpretation = "A confidence interval is like a plausible range of values for the population parameter, not a guarantee about one exact number.",
    slope_interpretation = "Slope is like the tilt of a ramp: it tells how much the prediction changes when x moves by one unit.",
    variable_type_identification = "Classifying a variable is like choosing a drawer: labels go in the categorical drawer, measurements go in the quantitative one.",
    graph_selection = "Choosing a graph is like choosing the right container: categorical data fit bar or pie charts, while quantitative data need distribution plots.",
    "Think of this as matching the right statistical story to the question before doing the arithmetic."
  )
}

get_help_next_step <- function(topic_id, concept_tag) {
  label <- get_concept_label(concept_tag %||% topic_id)
  glue("Practice one or two {label} questions next so you can apply this idea right away.")
}

build_unexpected_help_response <- function(topic_label = "General course help") {
  normalize_help_response_object(list(
    routed_topic_label = topic_label,
    direct_answer = get_help_failure_message(),
    remember_bullets = c(
      "I kept the routed topic so your review sheet can still use the signal from this question.",
      "Try asking again with one key phrase like p-value, alpha, z-score, or margin of error.",
      "If you want, ask for a shorter explanation focused on one step."
    ),
    analogy = "This is like reloading a study card after the first copy failed to open.",
    common_mistake = "Do not assume the concept was lost just because the first response failed.",
    next_step = "Ask again with the specific term or notation that feels confusing."
  ), routed_topic_label = topic_label)
}

deserialize_help_response_object <- function(response_text,
                                             routed_topic_label = NULL,
                                             topic_id = NULL,
                                             query_text = NULL) {
  parsed <- parse_help_response_json(response_text)
  if (!is.null(parsed)) {
    return(normalize_help_response_object(parsed, routed_topic_label = routed_topic_label))
  }
  
  normalize_help_response_object(
    build_help_fallback_response(topic_id, query_text),
    routed_topic_label = routed_topic_label %||% if (!is.null(topic_id) && is_valid_topic_id(topic_id, require_known = TRUE)) get_topic_label(topic_id) else "General course help"
  )
}

render_help_response_content <- function(response) {
  response <- normalize_help_response_object(response)
  
  tagList(
    div(class = "help-topic-label", response$routed_topic_label),
    div(
      class = "help-section",
      div(class = "help-section-label", "Direct answer"),
      p(class = "help-direct-answer", response$direct_answer)
    ),
    div(
      class = "help-section",
      div(class = "help-section-label", "Remember"),
      tags$ul(class = "help-remember-list", lapply(response$remember_bullets, tags$li))
    ),
    div(
      class = "help-analogy-box",
      div(class = "help-section-label", "Analogy"),
      p(response$analogy)
    ),
    div(
      class = "help-warning-box",
      div(class = "help-section-label", "Common mistake"),
      p(response$common_mistake)
    ),
    if (nzchar(response$next_step)) {
      div(
        class = "help-section help-next-step",
        div(class = "help-section-label", "Next step"),
        p(response$next_step)
      )
    }
  )
}

build_help_fallback_response <- function(topic_id, query_text) {
  topic_id <- sanitize_topic_id(topic_id, require_known = TRUE)
  if (is.null(topic_id)) {
    return(build_general_help_fallback(query_text))
  }
  
  topic_label <- get_topic_label(topic_id)
  concept_tag <- detect_help_concept_tag(query_text, topic_id)
  sections <- extract_concept_sections(topic_id)
  reminder_entry <- get_review_concept_entry(concept_tag)
  reminder_bullets <- if (nrow(reminder_entry) > 0) {
    reminder_entry$reminder_bullets[[1]] %||% character()
  } else {
    sections$reminder_bullets %||% character()
  }
  reminder_bullets <- reminder_bullets[nzchar(str_squish(reminder_bullets))]
  
  if (is_p_value_help_query(query_text, topic_id, concept_tag)) {
    direct_answer <- "A p-value tells you how unusual your observed result would be if the null hypothesis were true. Smaller p-values mean the data push harder against H0."
    reminder_bullets <- c(
      "P-values are calculated assuming H0 is true.",
      "A small p-value means the observed result would be unusual under H0.",
      "A p-value is not the probability that H0 is true.",
      "Compare the p-value to alpha when you decide whether to reject H0.",
      "Fail to reject does not mean accept H0."
    )
    common_mistake <- "Do not say the p-value is the chance that the null hypothesis is true."
  } else {
    direct_answer <- compact_help_text(
      sections$explanation,
      "Start by identifying what parameter or idea the question is targeting, then match the notation and method to that goal."
    )
    common_mistake <- compact_help_text(
      sections$common_mistake,
      "Do not skip the notation check or mix up which method fits the question."
    )
    if (length(reminder_bullets) == 0) {
      reminder_bullets <- c(
        direct_answer,
        compact_help_text(sections$formula, "Check the notation before you calculate."),
        common_mistake
      )
    }
  }
  
  normalize_help_response_object(
    list(
      routed_topic_label = topic_label,
      direct_answer = direct_answer,
      remember_bullets = unique(reminder_bullets)[seq_len(min(length(unique(reminder_bullets)), 5))],
      analogy = get_help_analogy(topic_id, concept_tag, query_text),
      common_mistake = common_mistake,
      next_step = get_help_next_step(topic_id, concept_tag)
    ),
    routed_topic_label = topic_label
  )
}

build_general_help_fallback <- function(query_text = NULL) {
  prompt_line <- if (nzchar(str_squish(query_text %||% ""))) {
    glue("I need one more clue to pin \"{str_sub(str_squish(query_text), 1, 90)}\" to the right topic.")
  } else {
    "I need one more clue to pin this to the right topic."
  }
  
  normalize_help_response_object(
    list(
      routed_topic_label = "General course help",
      direct_answer = prompt_line,
      remember_bullets = c(
        "Tell me whether this is mostly about hypothesis tests, confidence intervals, normal or binomial models, graphs, or variable types.",
        "Include the notation or phrase that is tripping you up, like p-value, alpha, z-score, margin of error, or BINS.",
        "Once I have that clue, I can route the question more precisely and explain the setup."
      ),
      analogy = "This is like sorting a problem into the right chapter before solving it.",
      common_mistake = "Do not jump into calculations before identifying the topic and notation.",
      next_step = "Try asking again with one key phrase from the problem statement."
    ),
    routed_topic_label = "General course help"
  )
}

REVIEW_CONCEPT_ALIASES <- c(
  "pvalue_interpretation" = "p_value_interpretation",
  "p_value_conclusion_interpretation" = "p_value_interpretation",
  "p_value_interpretation_and_conclusion" = "p_value_interpretation",
  "p_value_decision_rule" = "p_value_interpretation",
  "decision_rule_p_value" = "p_value_interpretation",
  "decision_rule_conclusion" = "fail_to_reject",
  "conclusion_and_pvalue_interpretation" = "p_value_interpretation",
  "hypothesis_test_conclusion_interpretation" = "fail_to_reject",
  "interpreting_hypothesis_test_conclusion" = "fail_to_reject",
  "interpreting_p_value_conclusion" = "p_value_interpretation",
  "conclusion_language" = "fail_to_reject",
  "appropriate_graph_selection" = "graph_selection",
  "appropriate_graph_selection_categorical" = "graph_selection",
  "variable_classification" = "variable_type_identification",
  "variable_classification_quantitative_continuous" = "variable_type_identification",
  "variable_subtypes" = "variable_type_identification",
  "common_classification_mistakes" = "variable_type_identification",
  "common_mistakes_classification" = "variable_type_identification",
  "common_mistakes_identification" = "variable_type_identification",
  "symbol_classification" = "variable_type_identification",
  "measures_classification" = "variable_type_identification",
  "bins_conditions" = "binomial_conditions",
  "bins_conditions_and_model_selection" = "binomial_conditions",
  "normal_approximation_condition" = "binomial_conditions",
  "binomial_formula_components" = "binomial_probability_formula",
  "binomial_exact_probability" = "binomial_probability_formula",
  "probability_wording_translation" = "binomial_at_least_at_most",
  "standard_error_calculation_proportion" = "one_proportion_standard_error",
  "standard_error_calculation_proportions" = "one_proportion_standard_error",
  "test_statistic_standard_error" = "one_proportion_standard_error",
  "mean_standard_error" = "mean_standard_error",
  "standard_error_formula_mean" = "mean_standard_error",
  "standard_error_and_sample_size_relationship" = "mean_standard_error",
  "z_score_interpretation" = "z_score_interpretation",
  "statistical_vs_practical_significance" = "statistical_vs_practical_significance"
)

REVIEW_REMINDER_BANK <- tibble(
  concept_tag = c(
    "p_value_interpretation",
    "fail_to_reject",
    "binomial_conditions",
    "binomial_at_least_at_most",
    "binomial_probability_formula",
    "ci_interpretation",
    "one_proportion_standard_error",
    "slope_interpretation",
    "variable_type_identification",
    "graph_selection",
    "mean_standard_error",
    "z_score_interpretation",
    "statistical_vs_practical_significance"
  ),
  topic_id = c(
    "ht_foundations",
    "ht_foundations",
    "binomial_dist",
    "binomial_dist",
    "binomial_dist",
    "ci_prop",
    "ht_prop",
    "relationships_regression",
    "data_graphs",
    "data_graphs",
    "ci_mean",
    "normal_dist",
    "uses_abuses_tests"
  ),
  concept_label = c(
    "P-value interpretation",
    "Fail to reject wording",
    "Binomial conditions",
    "Binomial at least / at most wording",
    "Binomial probability formula",
    "Confidence interval interpretation",
    "One-proportion standard error",
    "Slope interpretation",
    "Variable type identification",
    "Graph selection",
    "Mean standard error",
    "Z-score interpretation",
    "Statistical vs. practical significance"
  ),
  reminder_bullets = list(
    c(
      "Remember that the p-value is computed assuming the null hypothesis is true.",
      "A p-value is not the probability that the null hypothesis is true.",
      "Smaller p-values mean the data would be more unusual under the null.",
      "A small p-value supports evidence against the null, not automatic practical importance."
    ),
    c(
      "\"Fail to reject\" means the data did not give strong enough evidence against the null.",
      "It does not mean the null hypothesis has been proven true.",
      "Keep the conclusion in context and talk about evidence, not certainty."
    ),
    c(
      "Use BINS: binary outcomes, independent trials, fixed number of trials, same success probability.",
      "Check independence before using binomial formulas.",
      "If the success probability changes across trials, the setting is not binomial."
    ),
    c(
      "\"At least k\" means P(X >= k).",
      "Use the complement when it is easier: P(X >= k) = 1 - P(X <= k - 1).",
      "\"At most k\" means P(X <= k).",
      "\"More than k\" means P(X > k) = 1 - P(X <= k)."
    ),
    c(
      "For exactly x successes, use choose(n, x) p^x (1-p)^(n-x).",
      "Match x to the number of successes named in the question.",
      "Keep p as the success probability on one trial."
    ),
    c(
      "A confidence interval estimates a population parameter, not the sample statistic.",
      "The confidence level is about the method's long-run success rate, not this one interval alone.",
      "Interpret the interval in context and name the parameter clearly."
    ),
    c(
      "In a one-proportion test, the standard error uses the null value p0.",
      "In a one-proportion confidence interval, the standard error uses p-hat.",
      "Do not mix the test formula with the interval formula."
    ),
    c(
      "Interpret slope as the predicted change in y for a one-unit increase in x.",
      "Keep the units and context in the sentence.",
      "Slope describes association in the fitted line, not proof of causation."
    ),
    c(
      "Ask whether arithmetic on the values makes sense.",
      "Labels and identifiers are categorical, even when they look numeric.",
      "Quantitative variables measure an amount; categorical variables place observations into groups."
    ),
    c(
      "Use bar charts or pie charts for categorical variables.",
      "Use histograms, dotplots, stemplots, or boxplots for quantitative variables.",
      "Choose the graph type only after identifying the variable type."
    ),
    c(
      "For a sample mean, the standard error is s / sqrt(n).",
      "Larger samples shrink standard error.",
      "Do not confuse standard deviation with standard error."
    ),
    c(
      "A z-score tells how many standard deviations a value is from the mean.",
      "Positive z-scores are above the mean and negative z-scores are below it.",
      "Use z-scores to compare values on the same standardized scale."
    ),
    c(
      "Statistical significance is not the same as practical importance.",
      "A tiny effect can be statistically significant with a large sample.",
      "Always connect the result back to context, effect size, and study design."
    )
  )
)

REVIEW_REMINDER_BANK <- bind_rows(
  REVIEW_REMINDER_BANK,
  tibble(
    concept_tag = c(
      "ht_foundations",
      "ht_prop",
      "ht_mean",
      "ci_prop",
      "ci_mean",
      "data_graphs",
      "descriptive_stats",
      "binomial_dist",
      "normal_dist",
      "relationships_regression"
    ),
    topic_id = c(
      "ht_foundations",
      "ht_prop",
      "ht_mean",
      "ci_prop",
      "ci_mean",
      "data_graphs",
      "descriptive_stats",
      "binomial_dist",
      "normal_dist",
      "relationships_regression"
    ),
    concept_label = c(
      "Hypothesis testing foundations",
      "Hypothesis tests for proportions",
      "Hypothesis tests for means",
      "Confidence intervals for proportions",
      "Confidence intervals for means",
      "Data types and graphs",
      "Descriptive statistics",
      "Binomial distribution",
      "Normal distribution",
      "Relationships and regression"
    ),
    reminder_bullets = list(
      c(
        "Start by naming the null and alternative hypotheses clearly.",
        "Interpret evidence under the null model before making a conclusion.",
        "Reject or fail to reject based on evidence, not certainty."
      ),
      c(
        "For a one-proportion test, check conditions before computing z.",
        "Use p0 in the test standard error, not p-hat.",
        "State the conclusion in context with the population proportion."
      ),
      c(
        "For a one-mean t test, use the sample mean with s / sqrt(n).",
        "Check assumptions and keep the parameter in context.",
        "Use the t procedure when sigma is unknown."
      ),
      c(
        "A proportion interval estimates the population proportion p.",
        "Check success-failure conditions before using the interval.",
        "Interpret the interval in context, not as a probability statement about one interval."
      ),
      c(
        "A mean interval estimates the population mean mu.",
        "Use the sample standard deviation and s / sqrt(n).",
        "Keep the confidence interpretation tied to the long-run method."
      ),
      c(
        "Identify whether the variable is categorical or quantitative first.",
        "Then choose the display that matches that variable type.",
        "Do not treat labels or ID numbers as measurements."
      ),
      c(
        "Use resistant summaries like the median and IQR when outliers matter.",
        "Describe center, spread, shape, and unusual features in context.",
        "Match the summary statistic to the graph and the shape of the distribution."
      ),
      c(
        "Use BINS before deciding a setting is binomial.",
        "Translate wording like at least or more than into the right probability statement.",
        "Keep success probability tied to one trial."
      ),
      c(
        "A z-score measures distance from the mean in standard deviation units.",
        "Use normal-model areas only after standardizing correctly.",
        "Keep track of whether the question asks for left-tail, right-tail, or middle area."
      ),
      c(
        "Interpret slope as predicted change in y for a one-unit increase in x.",
        "Residuals are observed minus predicted.",
        "Association in a regression line does not prove causation."
      )
    )
  )
)

stable_css <- "
  :root {
    --sq-bg: #f4f7fb;
    --sq-surface: #ffffff;
    --sq-surface-soft: #f7f9fc;
    --sq-border: rgba(15, 23, 42, 0.10);
    --sq-text: #1f2933;
    --sq-muted: #617080;
    --sq-accent: #1f5fa8;
    --sq-accent-strong: #163f73;
    --sq-accent-soft: rgba(31, 95, 168, 0.10);
    --sq-success: #2e7d32;
    --sq-danger: #c62828;
    --sq-shadow: 0 16px 36px rgba(15, 23, 42, 0.08);
  }
  body {
    background: linear-gradient(180deg, #eef3f8 0%, var(--sq-bg) 100%);
    color: var(--sq-text);
  }
  .app-shell {
    max-width: 1320px;
    margin: 0 auto;
    padding-bottom: 2rem;
  }
  .login-card {
    max-width: 580px;
    margin: 60px auto;
  }
  .app-topbar {
    padding: 0.9rem 1rem;
    margin-bottom: 0.85rem;
    border-bottom: 1px solid var(--sq-border);
    background: rgba(255,255,255,0.78);
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 1rem;
    backdrop-filter: blur(10px);
  }
  .navbar {
    background: linear-gradient(90deg, var(--sq-accent-strong) 0%, var(--sq-accent) 100%) !important;
    box-shadow: 0 12px 30px rgba(15, 23, 42, 0.18);
  }
  .navbar .navbar-brand,
  .navbar .nav-link,
  .navbar .navbar-text {
    color: #f8fbff !important;
  }
  .nav-pills .nav-link,
  .nav-tabs .nav-link {
    color: var(--sq-text);
  }
  .nav-pills .nav-link.active,
  .nav-tabs .nav-link.active {
    color: var(--sq-accent-strong);
    background: color-mix(in srgb, var(--sq-accent-soft) 88%, white);
    border-color: var(--sq-border) var(--sq-border) transparent;
    font-weight: 700;
  }
  .card {
    background: var(--sq-surface);
    border: 1px solid var(--sq-border);
    box-shadow: var(--sq-shadow);
    border-radius: 22px;
    overflow: hidden;
  }
  .card-header {
    background: #f6f9fc;
    color: var(--sq-text);
    border-bottom: 1px solid var(--sq-border);
    font-weight: 700;
  }
  .card p,
  .card li,
  .card label,
  .card .form-text,
  .card .shiny-input-container,
  .table,
  .table th,
  .table td {
    color: var(--sq-text);
  }
  .form-control,
  .form-select,
  .selectize-input,
  .selectize-dropdown {
    color: var(--sq-text) !important;
    background: #ffffff !important;
    border-color: rgba(15, 23, 42, 0.14) !important;
  }
  .selectize-input input,
  .selectize-dropdown .option {
    color: var(--sq-text) !important;
    background: #ffffff !important;
  }
  .small-muted {
    color: var(--sq-muted);
    font-size: 0.92rem;
  }
.practice-main-shell {
    width: 100%;
    margin: 0 auto;
    display: grid;
    gap: 1rem;
  }
  .practice-setup-stack {
    display: grid;
    gap: 1rem;
    width: 100%;
  }
  .practice-setup-section {
    display: grid;
    gap: 0.6rem;
    width: 100%;
  }
  .practice-setup-section-title {
    font-size: 0.9rem;
    font-weight: 800;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    color: var(--sq-accent-strong);
  }
  .module-card-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(230px, 1fr));
    gap: 0.8rem;
  }
  .module-card-button {
    width: 100%;
    min-height: 5rem;
    text-align: left;
    white-space: normal;
    border: 1px solid var(--sq-border);
    border-radius: 8px;
    background: #ffffff;
    color: var(--sq-text);
    padding: 0.85rem 0.95rem;
    font-weight: 750;
    line-height: 1.25;
    box-shadow: none;
  }
  .module-card-button:hover {
    border-color: var(--sq-accent);
    background: #f8fbff;
  }
  .module-card-button.selected {
    border-color: var(--sq-accent);
    background: color-mix(in srgb, var(--sq-accent-soft) 82%, white);
    color: var(--sq-accent-strong);
    box-shadow: inset 0 0 0 2px color-mix(in srgb, var(--sq-accent) 35%, transparent);
  }
  .module-card-order {
    display: block;
    margin-bottom: 0.3rem;
    color: var(--sq-muted);
    font-size: 0.78rem;
    text-transform: uppercase;
    letter-spacing: 0.04em;
  }
  .practice-session-meta {
    display: flex;
    flex-wrap: wrap;
    gap: 0.55rem;
    align-items: center;
    margin-bottom: 0.8rem;
  }
  .practice-badge {
    display: inline-block;
    padding: 0.28rem 0.65rem;
    border-radius: 999px;
    background: color-mix(in srgb, var(--sq-accent-soft) 86%, white);
    color: var(--sq-accent-strong);
    font-weight: 700;
    font-size: 0.92rem;
  }
  .practice-question-wrap {
    background: #f8fbff;
    border: 1px solid var(--sq-border);
    border-left: 6px solid var(--sq-accent);
    border-radius: 18px;
    padding: 1rem 1.1rem;
  }
  .practice-response-shell {
    margin-top: 1rem;
    padding: 1rem 1.1rem;
    border: 1px solid var(--sq-border);
    border-radius: 18px;
    background: var(--sq-surface-soft);
  }
  .practice-ordering-grid,
  .practice-categorize-grid {
    display: grid;
    gap: 0.8rem;
  }
  .practice-choice-group .form-check {
    margin-bottom: 0.55rem;
  }
  .practice-actions {
    display: flex;
    gap: 0.75rem;
    flex-wrap: wrap;
    margin-top: 1rem;
  }
  .practice-feedback-card.correct {
    border-left: 6px solid var(--sq-success);
    background: #f1fbf2;
  }
  .practice-feedback-card.incorrect {
    border-left: 6px solid var(--sq-danger);
    background: #fff5f5;
  }
  .review-sheet-card {
    margin-bottom: 1rem;
  }
  .progress-summary-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
    gap: 1rem;
  }
  .summary-metric-card {
    padding: 1rem 1.1rem;
  }
  .summary-metric-label {
    color: var(--sq-muted);
    font-size: 0.85rem;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    margin-bottom: 0.35rem;
  }
  .summary-metric-value {
    font-size: 2rem;
    font-weight: 800;
    line-height: 1.05;
    color: var(--sq-accent-strong);
  }
  .summary-metric-note {
    margin-top: 0.4rem;
    color: var(--sq-muted);
    font-size: 0.92rem;
  }
  .progress-insight-card {
    background: linear-gradient(135deg, #f7fbff 0%, #eef5ff 100%);
  }
  .progress-module-list,
  .progress-weak-grid,
  .recent-activity-list {
    display: grid;
    gap: 0.9rem;
  }
  .module-progress-card,
  .weak-concept-card,
  .activity-item {
    border: 1px solid var(--sq-border);
    border-radius: 18px;
    padding: 1rem 1.05rem;
    background: var(--sq-surface-soft);
  }
  .module-progress-top,
  .weak-card-top,
  .activity-top {
    display: flex;
    gap: 0.75rem;
    justify-content: space-between;
    align-items: flex-start;
    flex-wrap: wrap;
    margin-bottom: 0.6rem;
  }
  .module-progress-title,
  .weak-card-title,
  .activity-title {
    font-weight: 700;
    color: var(--sq-text);
  }
  .status-badge {
    display: inline-flex;
    align-items: center;
    gap: 0.35rem;
    padding: 0.28rem 0.72rem;
    border-radius: 999px;
    font-size: 0.84rem;
    font-weight: 700;
    border: 1px solid transparent;
  }
  .status-badge.not-started {
    background: #eef2f7;
    color: #5b6675;
    border-color: #d6dde7;
  }
  .status-badge.needs-review {
    background: #fff3e8;
    color: #a95b12;
    border-color: #f2d0b0;
  }
  .status-badge.improving {
    background: #eef8ef;
    color: #2f6a39;
    border-color: #c8e3cb;
  }
  .status-badge.strong {
    background: #ebf4ff;
    color: #1b4f91;
    border-color: #c7daf5;
  }
  .module-progress-meta,
  .weak-card-meta,
  .activity-meta {
    display: flex;
    flex-wrap: wrap;
    gap: 0.55rem 1rem;
    color: var(--sq-muted);
    font-size: 0.92rem;
  }
  .progress-track {
    width: 100%;
    height: 12px;
    border-radius: 999px;
    background: #e6edf5;
    overflow: hidden;
    margin: 0.7rem 0 0.45rem;
  }
  .progress-fill {
    height: 100%;
    background: linear-gradient(90deg, var(--sq-accent) 0%, #61a5ff 100%);
    border-radius: 999px;
  }
  .weak-card-body {
    color: var(--sq-text);
  }
  .action-pill {
    display: inline-block;
    margin-top: 0.75rem;
    padding: 0.42rem 0.82rem;
    border-radius: 999px;
    background: var(--sq-accent-soft);
    color: var(--sq-accent-strong);
    font-weight: 700;
    font-size: 0.9rem;
  }
  .activity-result {
    font-weight: 700;
  }
  .activity-result.correct {
    color: var(--sq-success);
  }
  .activity-result.incorrect {
    color: var(--sq-danger);
  }
  .empty-state-card {
    text-align: left;
  }
  .help-thread {
    display: grid;
    gap: 0.8rem;
  }
  .help-entry {
    border: 1px solid var(--sq-border);
    border-radius: 18px;
    padding: 0.95rem 1rem;
    background: var(--sq-surface-soft);
  }
  .help-entry.user {
    border-left: 5px solid var(--sq-accent);
  }
  .help-entry.assistant {
    border-left: 5px solid var(--sq-success);
    background: #fbfcfe;
  }
  .help-topic-label {
    display: inline-block;
    margin-bottom: 0.7rem;
    padding: 0.2rem 0.55rem;
    border-radius: 999px;
    background: rgba(31, 95, 168, 0.08);
    color: var(--sq-accent-strong);
    font-size: 0.82rem;
    font-weight: 700;
  }
  .help-section + .help-section,
  .help-analogy-box,
  .help-warning-box {
    margin-top: 0.8rem;
  }
  .help-section-label {
    margin-bottom: 0.28rem;
    font-size: 0.78rem;
    line-height: 1.3;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    color: var(--sq-muted);
    font-weight: 700;
  }
  .help-direct-answer,
  .help-analogy-box p,
  .help-warning-box p,
  .help-next-step p {
    margin: 0;
    font-size: 0.96rem;
    line-height: 1.5;
    color: var(--sq-text);
  }
  .help-remember-list {
    margin: 0;
    padding-left: 1.05rem;
    color: var(--sq-text);
  }
  .help-remember-list li {
    margin-bottom: 0.18rem;
    line-height: 1.45;
  }
  .help-analogy-box,
  .help-warning-box {
    padding: 0.7rem 0.8rem;
    border-radius: 14px;
    border: 1px solid var(--sq-border);
  }
  .help-analogy-box {
    background: #f4f8ff;
  }
  .help-warning-box {
    background: #fff6eb;
    border-color: #f1d1ab;
  }
  .table {
    --bs-table-bg: transparent;
    --bs-table-color: var(--sq-text);
    --bs-table-striped-bg: #f7f9fc;
    --bs-table-border-color: var(--sq-border);
  }
  .shiny-notification {
    color: var(--sq-text);
    background: var(--sq-surface);
    border: 1px solid var(--sq-border);
  }
  .selection-grid,
  .selection-grid > .shiny-input-container,
  .selection-grid .shiny-input-container,
  .selection-grid .shiny-options-group {
    width: 100%;
    max-width: none;
  }
  .selection-grid .shiny-input-container {
    margin-bottom: 0;
  }
  .selection-grid .shiny-options-group {
    display: grid;
    gap: 1rem;
    align-items: stretch;
  }
  .selection-grid .control-label {
    margin-bottom: 0.55rem;
    font-weight: 700;
    color: var(--sq-text);
  }
  .module-selector .shiny-options-group {
    grid-template-columns: repeat(3, minmax(0, 1fr));
  }
  .mode-selector .shiny-options-group {
    grid-template-columns: repeat(4, minmax(0, 1fr));
  }
  .module-selector .checkbox:last-child {
    grid-column: 1 / -1;
  }
  .selection-grid .checkbox,
  .selection-grid .radio {
    margin: 0;
    height: 100%;
    width: 100%;
  }
  .selection-grid label {
    display: block;
    margin: 0;
    cursor: pointer;
    height: 100%;
    width: 100%;
  }
  .selection-grid input {
    position: absolute;
    opacity: 0;
    pointer-events: none;
  }
  .selection-grid input + span {
    display: block;
    height: 100%;
    min-height: 96px;
    padding: 0.9rem 1rem;
    padding-right: 4.9rem;
    border-radius: 18px;
    border: 1px solid rgba(15, 23, 42, 0.12);
    background: var(--sq-surface-soft);
    color: var(--sq-text);
    font-weight: 600;
    line-height: 1.35;
    position: relative;
    transition: transform 0.12s ease, box-shadow 0.12s ease, border-color 0.12s ease, background 0.12s ease;
    overflow-wrap: normal;
    word-break: normal;
    white-space: normal;
  }
  .selection-grid input:hover + span,
  .selection-grid label:hover span {
    transform: translateY(-1px);
    box-shadow: 0 10px 22px rgba(15, 23, 42, 0.08);
  }
  .selection-grid input:checked + span {
    border-color: rgba(31, 95, 168, 0.40);
    background: linear-gradient(135deg, rgba(31, 95, 168, 0.14) 0%, rgba(255, 255, 255, 0.98) 100%);
    color: var(--sq-accent-strong);
    box-shadow: 0 12px 26px rgba(31, 95, 168, 0.14);
  }
  .selection-grid input:checked + span::after {
    content: 'Selected';
    position: absolute;
    top: 0.72rem;
    right: 0.78rem;
    padding: 0.12rem 0.48rem;
    border-radius: 999px;
    background: rgba(31, 95, 168, 0.12);
    color: var(--sq-accent-strong);
    font-size: 0.72rem;
    font-weight: 700;
    letter-spacing: 0.02em;
  }
  .module-selector input + span,
  .mode-selector input + span {
    min-height: 104px;
  }
  .module-selector .selection-card-title {
    font-size: 1rem;
  }
  .selection-card-copy {
    display: grid;
    gap: 0.24rem;
    min-width: 0;
  }
  .selection-card-title {
    display: block;
    font-size: 0.98rem;
    line-height: 1.28;
    font-weight: 700;
    color: inherit;
  }
  .selection-card-meta {
    display: block;
    font-size: 0.84rem;
    line-height: 1.35;
    color: var(--sq-muted);
    font-weight: 600;
  }
  .practice-setup-lead {
    font-size: 0.98rem;
    line-height: 1.55;
    color: var(--sq-muted);
    margin-bottom: 0.95rem;
  }
  .practice-setup-actions {
    display: flex;
    flex-wrap: wrap;
    gap: 0.65rem 0.9rem;
    align-items: center;
    margin-top: 0.75rem;
  }
  .practice-setup-summary {
    display: flex;
    flex-wrap: wrap;
    gap: 0.55rem;
    margin-top: 0.65rem;
  }
  .practice-mode-helper {
    margin: 0;
    color: var(--sq-muted);
    font-size: 0.9rem;
    line-height: 1.45;
  }
  .practice-question-title {
    font-size: 1.15rem;
    font-weight: 700;
    margin-bottom: 0.45rem;
  }
  .visual-aid {
    margin: 0.85rem 0;
    padding: 0.75rem;
    border: 1px solid var(--sq-border);
    border-radius: 14px;
    background: #ffffff;
  }
  .visual-aid-label {
    font-size: 0.82rem;
    font-weight: 800;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    color: var(--sq-accent-strong);
    margin-bottom: 0.45rem;
  }
  .visual-aid-img {
    display: block;
    width: 100%;
    max-height: 360px;
    object-fit: contain;
    border-radius: 10px;
    background: #ffffff;
  }
  .visual-aid-caption {
    margin-top: 0.45rem;
    color: var(--sq-muted);
    font-size: 0.92rem;
  }
  .tutor-bubble-visual {
    margin-top: 0.75rem;
    margin-bottom: 0.35rem;
    background: #ffffff;
  }
  .practice-topic-line {
    color: var(--sq-muted);
    margin-bottom: 0.75rem;
  }
  .practice-status-note {
    font-size: 0.92rem;
    color: var(--sq-muted);
  }
  .practice-feedback-grid {
    display: grid;
    gap: 0.65rem;
  }
  .practice-feedback-line strong {
    display: inline-block;
    min-width: 8.5rem;
  }
  .practice-empty-note {
    padding: 0.95rem 1rem;
    border: 1px dashed rgba(15, 23, 42, 0.18);
    border-radius: 16px;
    background: #fbfcfe;
    color: var(--sq-muted);
  }
  .help-markdown p:last-child,
  .review-bullet-list li:last-child {
    margin-bottom: 0;
  }
  .help-markdown {
    color: var(--sq-text);
    line-height: 1.55;
  }
  .help-markdown h1,
  .help-markdown h2,
  .help-markdown h3,
  .help-markdown h4 {
    font-size: 1.02rem;
    line-height: 1.3;
    margin: 0.75rem 0 0.45rem;
    font-weight: 800;
    text-transform: none;
    letter-spacing: 0;
    color: var(--sq-text);
  }
  .help-markdown p,
  .help-markdown li {
    font-size: 0.98rem;
    line-height: 1.55;
  }
  .help-markdown p {
    margin: 0.4rem 0 0.7rem;
  }
  .help-markdown ul,
  .help-markdown ol {
    margin: 0.3rem 0 0.8rem;
    padding-left: 1.35rem;
  }
  .help-markdown li {
    margin-bottom: 0.25rem;
  }
  .help-markdown strong {
    color: var(--sq-accent-strong);
    font-weight: 700;
  }
  .help-entry.assistant {
    background: #fbfcfe;
  }
  .tutor-message {
    line-height: 1.55;
  }
  .tutor-message .help-markdown {
    margin-top: 0.15rem;
  }
  .help-entry summary {
    cursor: pointer;
    font-weight: 700;
    color: var(--sq-accent-strong);
  }
  .help-markdown code {
    background: rgba(15, 23, 42, 0.06);
    color: var(--sq-accent-strong);
    border-radius: 6px;
    padding: 0.1rem 0.35rem;
  }
  .help-response-meta {
    margin-top: 0.75rem;
    color: var(--sq-muted);
    font-size: 0.9rem;
  }
  .review-sheet-stack,
  .review-module-stack {
    display: grid;
    gap: 1rem;
  }
  .review-module-card {
    padding: 1.05rem 1.1rem;
  }
  .review-topic-block + .review-topic-block {
    margin-top: 1rem;
    padding-top: 1rem;
    border-top: 1px solid var(--sq-border);
  }
  .review-topic-title {
    font-weight: 700;
    margin-bottom: 0.45rem;
  }
  .review-bullet-list {
    margin: 0;
    padding-left: 1.2rem;
    color: var(--sq-text);
  }
  .progress-summary-grid > .card {
    height: 100%;
  }
  .summary-metric-card {
    display: flex;
    flex-direction: column;
    min-height: 160px;
  }
  .summary-metric-value.recommended-next {
    font-size: 1.05rem;
    line-height: 1.35;
    color: var(--sq-text);
  }
  .summary-metric-focus {
    margin-top: 0.4rem;
    font-size: 0.95rem;
    color: var(--sq-accent-strong);
    font-weight: 700;
  }
  .summary-metric-reason {
    margin-top: auto;
    padding-top: 0.65rem;
    color: var(--sq-muted);
    font-size: 0.9rem;
    line-height: 1.4;
  }
  @media (max-width: 1100px) {
    .mode-selector .shiny-options-group {
      grid-template-columns: repeat(2, minmax(0, 1fr));
    }
  }
  @media (max-width: 900px) {
    .module-selector .shiny-options-group {
      grid-template-columns: repeat(2, minmax(0, 1fr));
    }
  }
  @media (max-width: 640px) {
    .module-selector .shiny-options-group,
    .mode-selector .shiny-options-group {
      grid-template-columns: 1fr;
    }
    .selection-grid input + span {
      padding-right: 4.1rem;
      min-height: 92px;
    }
  }
  .module-progress-action {
    margin-top: 0.8rem;
    color: var(--sq-accent-strong);
    font-weight: 700;
    font-size: 0.92rem;
  }
  @media (max-width: 768px) {
    .app-topbar {
      flex-direction: column;
      align-items: flex-start;
    }
    .login-card {
      margin: 24px auto;
    }
  }
"

get_topic_meta <- function() {
  TOPIC_META
}

get_topic_choices <- function(module_id = NULL, include_auto = FALSE) {
  topics <- get_topic_meta()
  
  if (!is.null(module_id)) {
    topics <- topics %>% filter(module_id %in% !!module_id)
  }
  
  choices <- stats::setNames(topics$topic_id, topics$student_label)
  
  if (isTRUE(include_auto)) {
    c("Auto topic within module" = "__auto__", choices)
  } else {
    choices
  }
}

get_module_choices <- function() {
  stats::setNames(MODULES$module_id, MODULES$module_label)
}

build_selection_card <- function(title, meta = NULL) {
  div(
    class = "selection-card-copy",
    span(class = "selection-card-title", title),
    if (!is.null(meta) && nzchar(meta)) {
      span(class = "selection-card-meta", meta)
    }
  )
}

build_module_choice_names <- function() {
  purrr::map(MODULES$module_label, function(module_label) {
    module_label <- module_label %||% ""
    if (str_detect(module_label, "^Module\\s+[0-9]+:")) {
      build_selection_card(
        title = str_extract(module_label, "^Module\\s+[0-9]+"),
        meta = str_trim(str_remove(module_label, "^Module\\s+[0-9]+:\\s*"))
      )
    } else {
      build_selection_card(
        title = module_label,
        meta = "Mixed practice across the course"
      )
    }
  })
}

build_practice_mode_choice_names <- function() {
  mode_meta <- c(
    recommended = "Balanced mix of weak skills and coverage",
    weak_areas = "Stays close to concepts that still need work",
    quick_review = "Faster pacing with easier follow-up",
    challenge = "Pushes difficulty higher sooner"
  )
  
  purrr::map(unname(PRACTICE_MODES), function(mode_value) {
    build_selection_card(
      title = get_practice_mode_label(mode_value),
      meta = mode_meta[[mode_value]] %||% "Adaptive practice"
    )
  })
}

get_module_selection_summary <- function(module_ids) {
  module_ids <- intersect(module_ids %||% character(), MODULES$module_id)
  if (length(module_ids) == 0) {
    return("No modules selected yet")
  }
  if (length(module_ids) == nrow(MODULES)) {
    return("All modules selected")
  }
  if (length(module_ids) == 1) {
    return(get_module_label(module_ids[[1]]))
  }
  glue("{length(module_ids)} modules selected")
}

normalize_review_concept_tag <- function(concept_tag = NULL, topic_id = NULL) {
  topic_value <- sanitize_topic_id(topic_id, require_known = TRUE)
  tag_value <- normalize_scalar_string(concept_tag)
  
  if (!is.null(tag_value)) {
    tag_value <- tag_value %>%
      str_replace_all("[^A-Za-z0-9]+", "_") %>%
      str_replace_all("_+", "_") %>%
      str_replace("^_", "") %>%
      str_replace("_$", "") %>%
      str_to_lower()
    
    if (tag_value %in% names(REVIEW_CONCEPT_ALIASES)) {
      return(REVIEW_CONCEPT_ALIASES[[tag_value]])
    }
    
    return(tag_value)
  }
  
  topic_value %||% NULL
}

get_review_concept_entry <- function(concept_tag = NULL, topic_id = NULL) {
  normalized_tag <- normalize_review_concept_tag(concept_tag, topic_id)
  if (is.null(normalized_tag)) {
    return(REVIEW_REMINDER_BANK[0, , drop = FALSE])
  }
  
  REVIEW_REMINDER_BANK %>%
    filter(concept_tag == !!normalized_tag) %>%
    slice_head(n = 1)
}

get_topic_label <- function(topic_id) {
  row <- get_topic_row(topic_id)
  first_row_value(row, "student_label", sanitize_topic_id(topic_id) %||% "Unknown topic")
}

get_module_label <- function(module_id) {
  row <- MODULES %>% filter(module_id == !!module_id) %>% slice_head(n = 1)
  first_row_value(row, "module_label", module_id %||% "Unknown module")
}

get_module_for_topic <- function(topic_id) {
  row <- get_topic_row(topic_id)
  first_row_value(row, "module_id", "unknown_module")
}

get_concept_tag_for_topic <- function(topic_id) {
  row <- get_topic_row(topic_id)
  first_row_value(row, "concept_tag", sanitize_topic_id(topic_id) %||% "unknown_topic")
}

get_default_practice_modules <- function(user_id, max_modules = 3L) {
  if (is.null(normalize_scalar_string(user_id))) {
    return(MODULES$module_id[seq_len(min(2L, nrow(MODULES)))])
  }
  
  weak_modules <- get_weak_concepts(user_id) %>%
    arrange(desc(weakness_score), module_order, topic_order) %>%
    distinct(module_id) %>%
    pull(module_id)
  
  if (length(weak_modules) > 0) {
    return(weak_modules[seq_len(min(max_modules, length(weak_modules)))])
  }
  
  attempted_modules <- get_module_progress(user_id) %>%
    filter(total_attempts > 0) %>%
    arrange(avg_mastery, avg_accuracy, module_order) %>%
    pull(module_id)
  
  if (length(attempted_modules) > 0) {
    return(attempted_modules[seq_len(min(max_modules, length(attempted_modules)))])
  }
  
  MODULES$module_id[seq_len(min(2L, nrow(MODULES)))]
}

get_selected_modules <- function(module_ids = NULL) {
  selected <- module_ids %||% character()
  selected <- unlist(selected, use.names = FALSE)
  intersect(selected, MODULES$module_id)
}

sanitize_input_id <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[^A-Za-z0-9_]", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "")
}

module_button_id <- function(module_id) {
  paste0("module_btn_", sanitize_input_id(module_id))
}

render_module_button_grid <- function(selected_module_ids = character(), ns = identity) {
  selected_module_ids <- get_selected_modules(selected_module_ids)
  div(
    class = "module-card-grid",
    lapply(seq_len(nrow(MODULES)), function(i) {
      module_id <- MODULES$module_id[[i]]
      module_label <- MODULES$module_label[[i]] %||% module_id
      title <- if (str_detect(module_label, "^Module\\s+[0-9]+:")) {
        str_extract(module_label, "^Module\\s+[0-9]+")
      } else {
        module_label
      }
      meta <- if (str_detect(module_label, "^Module\\s+[0-9]+:")) {
        str_trim(str_remove(module_label, "^Module\\s+[0-9]+:\\s*"))
      } else {
        "Mixed practice across the course"
      }
      actionButton(
        ns(module_button_id(module_id)),
        label = tagList(
          span(class = "module-card-order", title),
          span(meta)
        ),
        class = paste("module-card-button", if (module_id %in% selected_module_ids) "selected" else "")
      )
    })
  )
}
resolve_practice_modules <- function(user_id, module_ids = NULL) {
  selected_modules <- get_selected_modules(module_ids)
  if (length(selected_modules) > 0) {
    return(selected_modules)
  }
  
  get_default_practice_modules(user_id)
}

get_pages_ordered <- function() {
  pages <- load_concept_pages()
  pages %>%
    left_join(select(get_topic_meta(), topic_id, student_label, module_id, module_label, module_order, topic_order), by = "topic_id") %>%
    arrange(module_order, topic_order)
}

parse_json_safely <- function(x) {
  text <- x %||% ""
  if (!is.character(text) || length(text) != 1 || !nzchar(str_squish(text))) {
    return(NULL)
  }
  tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

coerce_choice_values <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(character())
  }
  
  if (is.character(x) && length(x) == 1) {
    x_trim <- str_trim(x)
    if (str_detect(x_trim, "^[\\[{]")) {
      parsed <- parse_json_safely(x_trim)
      if (!is.null(parsed)) {
        return(coerce_choice_values(parsed))
      }
    }
    if (str_detect(x_trim, "^c\\(")) {
      matches <- str_match_all(x_trim, "\"([^\"]+)\"|'([^']+)'")[[1]]
      parsed <- c(matches[, 2], matches[, 3]) %>% discard(is.na)
      if (length(parsed) > 0) {
        return(parsed)
      }
    }
    if (str_detect(x_trim, "\\n")) {
      return(
        str_split(x_trim, "\\n")[[1]] %>%
          str_squish() %>%
          discard(~ !nzchar(.x))
      )
    }
  }
  
  if (is.list(x) && length(x) == 1) {
    x <- x[[1]]
  }
  
  as.character(unlist(x, use.names = FALSE))
}

make_choice_ids <- function(n) {
  if (n <= length(LETTERS)) {
    LETTERS[seq_len(n)]
  } else {
    paste0("C", seq_len(n))
  }
}

is_choice_object <- function(x) {
  is.list(x) && !is.null(names(x)) && all(c("id", "text") %in% names(x))
}

as_choice_object <- function(x, fallback_id = NULL) {
  if (is.data.frame(x) && nrow(x) > 0) {
    x <- as.list(x[1, , drop = FALSE])
  }
  if (!is_choice_object(x)) {
    return(NULL)
  }
  list(
    id = as.character(x$id %||% fallback_id %||% ""),
    text = as.character(x$text %||% "")
  )
}

normalize_choice_objects <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(list())
  }
  
  if (is.character(x) && length(x) == 1) {
    parsed <- parse_json_safely(str_trim(x))
    if (!is.null(parsed)) {
      return(normalize_choice_objects(parsed))
    }
  }
  
  if (is.data.frame(x) && all(c("id", "text") %in% names(x))) {
    return(
      purrr::map(seq_len(nrow(x)), function(i) {
        list(
          id = as.character(x$id[[i]] %||% ""),
          text = as.character(x$text[[i]] %||% "")
        )
      })
    )
  }
  
  if (is_choice_object(x)) {
    normalized <- as_choice_object(x)
    return(if (is.null(normalized)) list() else list(normalized))
  }
  
  if (is.list(x) && length(x) > 0 && all(map_lgl(x, ~ is_choice_object(.x) || (is.data.frame(.x) && all(c("id", "text") %in% names(.x)))))) {
    normalized <- purrr::imap(x, function(item, idx) {
      choice <- as_choice_object(item, fallback_id = make_choice_ids(length(x))[[idx]])
      if (is.null(choice) || !nzchar(choice$text)) {
        return(NULL)
      }
      choice$id <- choice$id %||% make_choice_ids(length(x))[[idx]]
      choice
    })
    return(compact(normalized))
  }
  
  values <- coerce_choice_values(x)
  if (length(values) == 0) {
    return(list())
  }
  
  ids <- make_choice_ids(length(values))
  purrr::map2(values, ids, function(text, id) {
    list(id = as.character(id), text = as.character(text))
  })
}

serialize_choice_objects <- function(x) {
  jsonlite::toJSON(normalize_choice_objects(x), auto_unbox = TRUE, null = "null")
}

deserialize_choice_objects <- function(x) {
  normalize_choice_objects(x)
}

get_choice_ids <- function(choices) {
  if (length(choices) == 0) {
    return(character())
  }
  map_chr(choices, ~ as.character(.x$id %||% ""))
}

get_choice_texts <- function(choices) {
  if (length(choices) == 0) {
    return(character())
  }
  map_chr(choices, ~ as.character(.x$text %||% ""))
}

VALID_DRAG_INTERACTION_TYPES <- c("select_all", "ordering", "categorize")

infer_drag_interaction_type <- function(question_text = NULL, interaction_type = NULL) {
  explicit_type <- normalize_scalar_string(interaction_type)
  if (!is.null(explicit_type) && explicit_type %in% VALID_DRAG_INTERACTION_TYPES) {
    return(explicit_type)
  }
  
  question_text <- str_to_lower(question_text %||% "")
  if (str_detect(question_text, "arrange|put\\s+.*in\\s+order|sequence|first\\s+to\\s+last|order\\s+from")) {
    return("ordering")
  }
  if (str_detect(question_text, "categor|sort\\s+.*group|sort\\s+.*categor|group\\s+the|classify|match\\s+.*category")) {
    return("categorize")
  }
  
  "select_all"
}

normalize_drag_interaction_type <- function(format, question_text = NULL, interaction_type = NULL) {
  if (!identical(format, "drag_and_drop")) {
    return(NA_character_)
  }
  
  infer_drag_interaction_type(question_text = question_text, interaction_type = interaction_type)
}

resolve_choice_value_to_id <- function(value, choices) {
  value <- normalize_scalar_string(value)
  if (is.null(value) || length(choices) == 0) {
    return(NA_character_)
  }
  
  ids <- get_choice_ids(choices)
  texts <- get_choice_texts(choices)
  if (value %in% ids) {
    return(value)
  }
  
  match_idx <- match(str_to_lower(value), str_to_lower(texts))
  if (!is.na(match_idx)) {
    return(ids[[match_idx]])
  }
  
  NA_character_
}

normalize_choice_match_key <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("[\u2018\u2019]", "'") %>%
    str_replace_all("[\u2010-\u2015]", "-") %>%
    str_replace_all("[^[:alnum:]_]+", " ") %>%
    str_squish()
}

resolve_choice_label_to_id <- function(value, choices) {
  direct <- resolve_choice_value_to_id(value, choices)
  if (!is.na(direct) && nzchar(direct)) {
    return(direct)
  }
  
  value <- normalize_scalar_string(value)
  if (is.null(value) || length(choices) == 0) {
    return(NA_character_)
  }
  
  ids <- get_choice_ids(choices)
  texts <- get_choice_texts(choices)
  key <- normalize_choice_match_key(value)
  text_keys <- normalize_choice_match_key(texts)
  id_keys <- normalize_choice_match_key(ids)
  
  exact_idx <- which(key == text_keys | key == id_keys)
  if (length(exact_idx) == 1) {
    return(ids[[exact_idx]])
  }
  
  key_contains_text <- vapply(
    text_keys,
    function(text_key) nzchar(text_key) && str_detect(key, fixed(text_key)),
    FUN.VALUE = logical(1)
  )
  partial_idx <- which(
    nzchar(key) &
      nchar(key) >= 2 &
      (str_detect(text_keys, fixed(key)) | key_contains_text)
  )
  if (length(partial_idx) == 1) {
    return(ids[[partial_idx]])
  }
  
  NA_character_
}

split_category_item_list <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all(regex("\\band\\b", ignore_case = TRUE), ",") %>%
    str_split(",") %>%
    unlist(use.names = FALSE) %>%
    str_replace_all("^[-*\\s]+|[-*\\s]+$", "") %>%
    str_replace_all("^['\"]|['\"]$", "") %>%
    str_squish() %>%
    discard(~ !nzchar(.x))
}

extract_category_labels_from_question <- function(question_text) {
  text <- str_squish(question_text %||% "")
  if (!nzchar(text)) {
    return(character())
  }
  
  quoted <- str_match_all(text, "['\"]([^'\"]+)['\"]")[[1]]
  if (nrow(quoted) >= 2) {
    return(unique(str_squish(quoted[, 2])))
  }
  
  lower <- str_to_lower(text)
  if (str_detect(lower, "nominal.*ordinal.*discrete.*continuous")) {
    return(c("Nominal", "Ordinal", "Discrete", "Continuous"))
  }
  if (str_detect(lower, "categorical.*quantitative")) {
    return(c("Categorical", "Quantitative"))
  }
  if (str_detect(lower, "resistant to outliers.*not resistant")) {
    return(c("Resistant to outliers", "Not resistant to outliers"))
  }
  if (str_detect(lower, "measure of center.*measure of spread.*measure of position")) {
    return(c("Measure of Center", "Measure of Spread", "Measure of Position"))
  }
  if (str_detect(lower, "sample statistic.*population parameter.*null value")) {
    return(c("Population Parameter", "Sample Statistic", "Null Value"))
  }
  if (str_detect(lower, "sample statistic.*population parameter")) {
    return(c("Sample Statistic", "Population Parameter"))
  }
  if (str_detect(lower, "correct interpretation.*incorrect interpretation")) {
    return(c("Correct Interpretation", "Incorrect Interpretation"))
  }
  if (str_detect(lower, "correct idea.*common mistake")) {
    return(c("Correct Idea", "Common Mistake"))
  }
  if (str_detect(lower, "correct practice.*mistake")) {
    return(c("Correct Practice", "Mistake"))
  }
  if (str_detect(lower, "correct statistical practice.*incorrect statistical practice")) {
    return(c("Correct Statistical Practice", "Incorrect Statistical Practice"))
  }
  if (str_detect(lower, "valid interpretation.*invalid interpretation")) {
    return(c("Valid Interpretation", "Invalid Interpretation"))
  }
  
  candidate <- str_match(text, regex("category\\s*:\\s*([^\\.]+)", ignore_case = TRUE))[, 2]
  if (!is.na(candidate) && nzchar(candidate)) {
    candidate <- str_replace_all(candidate, "\\s+OR\\s+|\\s+or\\s+", ",")
    labels <- split_category_item_list(candidate)
    if (length(labels) >= 2 && length(labels) <= 6) {
      return(labels)
    }
  }
  
  character()
}

coerce_category_list_mapping <- function(values, choices = list()) {
  mapping <- setNames(character(), character())
  entries <- coerce_choice_values(values)
  for (entry in entries) {
    if (str_detect(entry, "::|=>|->|→|←|\\|")) {
      next
    }
    match <- str_match(entry, "^\\s*([^:]+?)\\s*:\\s*(.+?)\\s*$")
    if (is.na(match[[1, 1]])) {
      next
    }
    category <- str_squish(match[[1, 2]])
    items <- split_category_item_list(match[[1, 3]])
    ids <- vapply(items, resolve_choice_label_to_id, choices = choices, FUN.VALUE = character(1))
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (length(ids) > 0) {
      mapping <- c(mapping, stats::setNames(rep(category, length(ids)), ids))
    }
  }
  mapping[!duplicated(names(mapping))]
}

coerce_ordered_category_mapping <- function(values, choices = list(), question_text = NULL) {
  answers <- coerce_choice_values(values)
  categories <- extract_category_labels_from_question(question_text)
  if (length(answers) == 0 || length(categories) == 0) {
    return(setNames(character(), character()))
  }
  
  ids <- vapply(answers, resolve_choice_label_to_id, choices = choices, FUN.VALUE = character(1))
  if (any(is.na(ids) | !nzchar(ids))) {
    return(setNames(character(), character()))
  }
  
  category_for_answer <- if (length(ids) == length(categories)) {
    categories
  } else if (length(ids) %% length(categories) == 0) {
    rep(categories, each = length(ids) / length(categories))
  } else {
    character()
  }
  if (length(category_for_answer) != length(ids)) {
    return(setNames(character(), character()))
  }
  
  stats::setNames(category_for_answer, ids)
}

coerce_drag_category_mapping <- function(x, choices = list(), question_text = NULL) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(setNames(character(), character()))
  }
  
  if (is.list(x) && length(x) > 0 && all(map_lgl(x, ~ is.list(.x) && !is.null(names(.x)) && all(c("id", "category") %in% names(.x))))) {
    ids <- map_chr(x, ~ as.character(.x$id %||% ""))
    categories <- map_chr(x, ~ as.character(.x$category %||% ""))
    ids <- vapply(ids, resolve_choice_label_to_id, choices = choices, FUN.VALUE = character(1))
    keep <- nzchar(ids) & nzchar(categories)
    return(stats::setNames(categories[keep], ids[keep]))
  }
  
  values <- x
  if (!is.character(values) || is.null(names(values))) {
    values <- coerce_choice_values(x)
  }
  
  mapping <- setNames(character(), character())
  
  if (is.character(values) && !is.null(names(values)) && any(nzchar(names(values)))) {
    ids <- vapply(names(values), resolve_choice_label_to_id, choices = choices, FUN.VALUE = character(1))
    categories <- as.character(values %||% "")
    keep <- nzchar(ids) & nzchar(categories)
    if (any(keep)) {
      mapping <- stats::setNames(categories[keep], ids[keep])
    }
  } else {
    parsed <- lapply(coerce_choice_values(values), function(entry) {
      match <- str_match(entry, "^\\s*(.*?)\\s*(::|=>|->|→|←|=|\\|)\\s*(.*?)\\s*$")
      if (is.na(match[[1, 1]])) {
        return(NULL)
      }
      choice_id <- resolve_choice_label_to_id(match[[1, 2]], choices)
      category <- str_squish(match[[1, 4]] %||% "")
      if (!nzchar(choice_id %||% "") || !nzchar(category)) {
        return(NULL)
      }
      stats::setNames(category, choice_id)
    })
    parsed <- compact(parsed)
    if (length(parsed) > 0) {
      mapping <- unlist(parsed, use.names = TRUE)
    }
  }
  
  if (length(mapping) == 0) {
    mapping <- coerce_category_list_mapping(values, choices = choices)
  }
  if (length(mapping) == 0) {
    mapping <- coerce_ordered_category_mapping(values, choices = choices, question_text = question_text)
  }
  
  mapping[!duplicated(names(mapping))]
}

get_drag_grading_values <- function(correct_answer, choices, interaction_type = NULL, question_text = NULL) {
  interaction_type <- infer_drag_interaction_type(question_text = question_text, interaction_type = interaction_type)
  
  if (identical(interaction_type, "categorize")) {
    return(coerce_drag_category_mapping(correct_answer, choices = choices, question_text = question_text))
  }
  
  answers <- coerce_choice_values(correct_answer)
  resolved <- vapply(answers, resolve_choice_value_to_id, choices = choices, FUN.VALUE = character(1))
  resolved <- resolved[nzchar(resolved)]
  
  if (identical(interaction_type, "select_all")) {
    return(unique(resolved))
  }
  
  resolved
}

get_drag_correct_answer_display <- function(correct_answer, choices, interaction_type = NULL, question_text = NULL) {
  interaction_type <- infer_drag_interaction_type(question_text = question_text, interaction_type = interaction_type)
  
  if (identical(interaction_type, "categorize")) {
    mapping <- coerce_drag_category_mapping(correct_answer, choices = choices, question_text = question_text)
    if (length(mapping) == 0) {
      return(character())
    }
    choice_labels <- map_choice_values_to_text(names(mapping), choices)
    return(paste0(choice_labels, " -> ", unname(mapping)))
  }
  
  map_choice_values_to_text(
    get_drag_grading_values(correct_answer, choices, interaction_type = interaction_type, question_text = question_text),
    choices
  )
}

get_drag_categories <- function(correct_answer, choices, interaction_type = NULL, question_text = NULL) {
  interaction_type <- infer_drag_interaction_type(question_text = question_text, interaction_type = interaction_type)
  if (!identical(interaction_type, "categorize")) {
    return(character())
  }
  
  categories <- unname(coerce_drag_category_mapping(correct_answer, choices = choices, question_text = question_text))
  unique(categories[nzchar(categories)])
}

FILL_IN_BLANK_RUNTIME_BLOCKLIST <- regex(
  paste(
    c(
      "sqrt",
      "\\bh\\s*0\\s*:",
      "\\bp\\s*\\(",
      "\\bp_?0\\b",
      "\\bp0\\b",
      "p₀",
      "p\\s*-?hat",
      "\\bphat\\b",
      "p̂",
      "\\bx\\s*-?bar\\b",
      "\\bxbar\\b",
      "x̄",
      "\\bmu0\\b",
      "\\bmu\\b",
      "μ",
      "μ₀",
      "\\bsigma\\b",
      "σ",
      "\\balpha\\b",
      "α",
      "=",
      "/",
      "\\^",
      "≤",
      "≥",
      "\\bformula\\b",
      "\\bexpression\\b"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

FILL_IN_BLANK_RUNTIME_COMPLEX_PATTERN <- regex(
  paste(
    c(
      "μ",
      "μ₀",
      "\\bmu0\\b",
      "\\bmu\\b",
      "p₀",
      "\\bp_?0\\b",
      "\\bp0\\b",
      "p̂",
      "\\bphat\\b",
      "x̄",
      "\\bxbar\\b",
      "σ",
      "\\balpha\\b",
      "α",
      "\\bh\\s*0\\s*:",
      "\\bp\\s*\\(",
      "≤",
      "≥",
      "<",
      ">",
      "=",
      "/",
      "\\+",
      "\\*",
      "\\^",
      "sqrt",
      "\\\\hat",
      "\\(",
      "\\)",
      "\\[",
      "\\]",
      "\\{",
      "\\}",
      ",",
      ";"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

FILL_IN_BLANK_RUNTIME_ROUNDING_PATTERN <- regex(
  paste(
    c(
      "\\bround\\b",
      "\\bdecimal\\b",
      "nearest\\s+(whole number|integer|tenth|hundredth|thousandth)",
      "\\b[0-9]+\\s+decimal"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

normalize_fill_runtime_answer <- function(x) {
  first_or_default(x, "") %>%
    as.character() %>%
    str_squish()
}

fill_in_blank_runtime_answer_is_numeric <- function(answer_text) {
  answer <- normalize_fill_runtime_answer(answer_text)
  nzchar(answer) && str_detect(answer, "^[+-]?(?:\\d+(?:\\.\\d+)?|\\.\\d+)$")
}

fill_in_blank_runtime_answer_is_simple_phrase <- function(answer_text) {
  answer <- normalize_fill_runtime_answer(answer_text)
  if (!nzchar(answer)) {
    return(FALSE)
  }
  
  !fill_in_blank_runtime_answer_is_numeric(answer) &&
    !str_detect(answer, FILL_IN_BLANK_RUNTIME_COMPLEX_PATTERN) &&
    !str_detect(answer, "[\r\n]") &&
    str_count(answer, "\\S+") <= 6 &&
    str_detect(answer, "^[[:alnum:]' -]+$")
}

fill_in_blank_runtime_has_rounding_instruction <- function(question_text) {
  text <- str_squish(as.character(question_text %||% ""))
  nzchar(text) && str_detect(text, FILL_IN_BLANK_RUNTIME_ROUNDING_PATTERN)
}

fill_in_blank_runtime_question_is_formula_heavy <- function(question_text) {
  text <- str_squish(as.character(question_text %||% ""))
  if (!nzchar(text)) {
    return(FALSE)
  }
  
  str_detect(text, FILL_IN_BLANK_RUNTIME_BLOCKLIST) ||
    str_count(text, "_{3,}") > 1 ||
    str_detect(text, regex("enter your answer as|type the expression|type the formula|type the notation|write the notation", ignore_case = TRUE))
}

fill_in_blank_runtime_variant_is_allowed <- function(answer_text) {
  fill_in_blank_runtime_answer_is_numeric(answer_text) || fill_in_blank_runtime_answer_is_simple_phrase(answer_text)
}

is_unsafe_fill_in_blank_question <- function(question_text, correct_answer, accepted_answers, choices = list()) {
  answers <- c(coerce_choice_values(correct_answer), coerce_choice_values(accepted_answers))
  answers <- answers[nzchar(str_squish(answers))]
  canonical <- normalize_fill_runtime_answer(correct_answer)
  
  if (length(normalize_choice_objects(choices)) > 0) {
    return(TRUE)
  }
  if (fill_in_blank_runtime_question_is_formula_heavy(question_text)) {
    return(TRUE)
  }
  if (!nzchar(canonical)) {
    return(TRUE)
  }
  if (!(fill_in_blank_runtime_answer_is_numeric(canonical) || fill_in_blank_runtime_answer_is_simple_phrase(canonical))) {
    return(TRUE)
  }
  if (fill_in_blank_runtime_answer_is_numeric(canonical) && !fill_in_blank_runtime_has_rounding_instruction(question_text)) {
    return(TRUE)
  }
  if (length(answers) > 0 && !all(vapply(answers, fill_in_blank_runtime_variant_is_allowed, FUN.VALUE = logical(1)))) {
    return(TRUE)
  }
  
  FALSE
}

filter_unsafe_fill_in_blank_questions <- function(bank, source_label = "question bank", warn = TRUE) {
  if (is.null(bank) || !is.data.frame(bank) || nrow(bank) == 0) {
    return(bank)
  }
  
  unsafe_rows <- bank %>%
    mutate(
      unsafe_fill_in_blank = if_else(
        format == "fill_in_blank",
        pmap_lgl(
          list(question_text, correct_answer, accepted_answers, choices),
          is_unsafe_fill_in_blank_question
        ),
        FALSE
      )
    )
  
  removed_n <- sum(unsafe_rows$unsafe_fill_in_blank, na.rm = TRUE)
  if (removed_n > 0 && isTRUE(warn)) {
    warning(glue("Removed {removed_n} unsafe fill-in-the-blank question(s) from {source_label} because they require notation-heavy or formula-heavy input."), call. = FALSE)
  }
  
  unsafe_rows %>%
    filter(!unsafe_fill_in_blank) %>%
    select(-unsafe_fill_in_blank)
}

get_choice_text_by_id <- function(choice_id, choices) {
  if (is.null(choice_id) || !nzchar(choice_id %||% "") || length(choices) == 0) {
    return(choice_id %||% "")
  }
  ids <- get_choice_ids(choices)
  texts <- get_choice_texts(choices)
  match_idx <- match(choice_id, ids)
  if (is.na(match_idx)) choice_id else texts[[match_idx]]
}

map_choice_values_to_text <- function(values, choices) {
  values <- coerce_choice_values(values)
  if (length(values) == 0) {
    return(character())
  }
  vapply(values, get_choice_text_by_id, choices = choices, FUN.VALUE = character(1))
}

derive_correct_choice_id <- function(correct_choice_id, correct_answer, choices, format) {
  if (!format %in% c("multiple_choice", "choose_best_answer")) {
    return(NA_character_)
  }
  
  ids <- get_choice_ids(choices)
  texts <- get_choice_texts(choices)
  candidate <- as.character(correct_choice_id %||% "") %>% str_squish()
  if (nzchar(candidate) && candidate %in% ids) {
    return(candidate)
  }
  
  answers <- coerce_choice_values(correct_answer)
  if (length(answers) == 0) {
    return(NA_character_)
  }
  
  answer <- answers[[1]]
  if (answer %in% ids) {
    return(answer)
  }
  
  match_idx <- match(str_to_lower(answer), str_to_lower(texts))
  if (!is.na(match_idx)) {
    return(ids[[match_idx]])
  }
  
  NA_character_
}

get_correct_answer_display <- function(correct_answer, correct_choice_id, choices, format, interaction_type = NULL, question_text = NULL) {
  answers <- coerce_choice_values(correct_answer)
  
  if (format %in% c("multiple_choice", "choose_best_answer")) {
    derived_id <- derive_correct_choice_id(correct_choice_id, answers, choices, format)
    if (!is.na(derived_id) && nzchar(derived_id)) {
      return(get_choice_text_by_id(derived_id, choices))
    }
    return(first_or_default(answers, ""))
  }
  
  if (identical(format, "drag_and_drop")) {
    return(get_drag_correct_answer_display(
      correct_answer = answers,
      choices = choices,
      interaction_type = interaction_type,
      question_text = question_text
    ))
  }
  
  answers
}

get_grading_values <- function(correct_answer, correct_choice_id, choices, format, interaction_type = NULL, question_text = NULL) {
  answers <- coerce_choice_values(correct_answer)
  
  if (format %in% c("multiple_choice", "choose_best_answer")) {
    derived_id <- derive_correct_choice_id(correct_choice_id, answers, choices, format)
    return(if (is.na(derived_id)) character() else derived_id)
  }
  
  if (identical(format, "drag_and_drop")) {
    return(get_drag_grading_values(
      correct_answer = answers,
      choices = choices,
      interaction_type = interaction_type,
      question_text = question_text
    ))
  }
  
  answers
}

is_practice_row_answerable <- function(row) {
  if (is.null(row) || !is.data.frame(row) || nrow(row) == 0) {
    return(FALSE)
  }
  row <- row[1, , drop = FALSE]
  format <- as.character(row$format[[1]] %||% "")
  question_text <- as.character(row$question_text[[1]] %||% "")
  choices <- normalize_choice_objects(row$choices[[1]])
  correct_answer <- row$correct_answer[[1]]
  correct_choice_id <- row$correct_choice_id[[1]] %||% NA_character_
  interaction_type <- normalize_drag_interaction_type(
    format = format,
    question_text = question_text,
    interaction_type = row$interaction_type[[1]] %||% NA_character_
  )
  
  if (format %in% c("multiple_choice", "choose_best_answer")) {
    return(length(choices) > 0 && !is.na(derive_correct_choice_id(correct_choice_id, correct_answer, choices, format)))
  }
  
  if (identical(format, "fill_in_blank")) {
    accepted_answers <- row$accepted_answers[[1]]
    return(length(c(coerce_choice_values(correct_answer), coerce_choice_values(accepted_answers))) > 0)
  }
  
  if (identical(format, "drag_and_drop")) {
    if (length(choices) == 0) {
      return(FALSE)
    }
    grading <- get_grading_values(
      correct_answer = correct_answer,
      correct_choice_id = correct_choice_id,
      choices = choices,
      format = format,
      interaction_type = interaction_type,
      question_text = question_text
    )
    if (identical(interaction_type, "categorize")) {
      return(length(grading) > 0 && setequal(names(grading), get_choice_ids(choices)))
    }
    if (identical(interaction_type, "ordering")) {
      return(length(grading) == length(choices))
    }
    return(length(grading) > 0)
  }
  
  FALSE
}

filter_unanswerable_practice_questions <- function(bank, source_label = "question bank", warn = TRUE) {
  if (is.null(bank) || !is.data.frame(bank) || nrow(bank) == 0) {
    return(bank)
  }
  checked <- bank %>%
    mutate(.answerable = map_lgl(seq_len(n()), ~ is_practice_row_answerable(bank[.x, , drop = FALSE])))
  removed_n <- sum(!checked$.answerable, na.rm = TRUE)
  if (removed_n > 0 && isTRUE(warn)) {
    warning(glue("Removed {removed_n} unanswerable practice question(s) from {source_label}. Check drag/categorize answer keys."), call. = FALSE)
  }
  checked %>%
    filter(.answerable) %>%
    select(-.answerable)
}

normalize_text_answer <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}

render_template_text <- function(text, topic_label, module_label) {
  if (is.null(text) || length(text) == 0 || all(is.na(text))) {
    return(NULL)
  }
  
  as.character(glue::glue_data(
    list(topic_label = topic_label, module_label = module_label),
    text
  ))
}

question_row <- function(question_id,
                         topic_id,
                         module_id,
                         concept_tag = NULL,
                         format,
                         difficulty,
                         question_text,
                         choices = character(),
                         interaction_type = NULL,
                         correct_choice_id = NA_character_,
                         correct_answer = character(),
                         accepted_answers = character(),
                         hint = NA_character_,
                         explanation = NA_character_,
                         visual_id = NA_character_,
                         visual_ids = character(),
                         visual_position = "above",
                         visual_required = FALSE,
                         tutor_visual_ids = character(),
                         hint_1 = NA_character_,
                         hint_2 = NA_character_,
                         hint_3 = NA_character_,
                         concept_explanation = NA_character_,
                         solution_explanation = NA_character_) {
  choice_objects <- normalize_choice_objects(choices)
  resolved_correct_choice_id <- derive_correct_choice_id(correct_choice_id, correct_answer, choice_objects, format)
  resolved_interaction_type <- normalize_drag_interaction_type(format, question_text, interaction_type)
  
  tibble::tibble(
    question_id = question_id,
    topic_id = topic_id,
    module_id = module_id,
    concept_tag = concept_tag %||% topic_id,
    format = format,
    difficulty = difficulty,
    question_text = question_text,
    choices = list(choice_objects),
    interaction_type = as.character(resolved_interaction_type %||% NA_character_),
    correct_choice_id = as.character(resolved_correct_choice_id %||% NA_character_),
    correct_answer = list(as.character(coerce_choice_values(correct_answer) %||% character())),
    accepted_answers = list(as.character(accepted_answers %||% character())),
    hint = as.character(hint %||% NA_character_),
    explanation = as.character(explanation %||% NA_character_),
    visual_id = as.character(visual_id %||% NA_character_),
    visual_ids = list(as.character(coerce_choice_values(visual_ids) %||% character())),
    visual_position = as.character(visual_position %||% "above"),
    visual_required = isTRUE(visual_required),
    tutor_visual_ids = list(as.character(coerce_choice_values(tutor_visual_ids) %||% character())),
    hint_1 = as.character(hint_1 %||% hint %||% NA_character_),
    hint_2 = as.character(hint_2 %||% NA_character_),
    hint_3 = as.character(hint_3 %||% NA_character_),
    concept_explanation = as.character(concept_explanation %||% explanation %||% NA_character_),
    solution_explanation = as.character(solution_explanation %||% explanation %||% NA_character_)
  )
}

serialize_text_vector <- function(x) {
  values <- coerce_choice_values(x)
  if (length(values) == 0) {
    return("")
  }
  paste(values, collapse = "\n")
}

deserialize_text_vector <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(x[[1]] %||% "")) {
    return(character())
  }
  coerce_choice_values(x[[1]])
}

as_question_row <- function(data) {
  if (!"correct_choice_id" %in% names(data)) {
    data <- mutate(data, correct_choice_id = NA_character_)
  }
  if (!"interaction_type" %in% names(data)) {
    data <- mutate(data, interaction_type = NA_character_)
  }
  visual_defaults <- list(
    visual_id = NA_character_,
    visual_ids = list(character()),
    visual_position = "above",
    visual_required = FALSE,
    tutor_visual_ids = list(character()),
    hint_1 = NA_character_,
    hint_2 = NA_character_,
    hint_3 = NA_character_,
    concept_explanation = NA_character_,
    solution_explanation = NA_character_
  )
  for (col in names(visual_defaults)) {
    if (!col %in% names(data)) {
      data[[col]] <- rep(visual_defaults[[col]], nrow(data))
    }
  }
  
  data %>%
    mutate(
      choices = map(choices, normalize_choice_objects),
      correct_answer = map(correct_answer, coerce_choice_values),
      accepted_answers = map(accepted_answers, coerce_choice_values),
      visual_ids = map(visual_ids, coerce_choice_values),
      tutor_visual_ids = map(tutor_visual_ids, coerce_choice_values),
      interaction_type = pmap_chr(
        list(format, question_text, interaction_type),
        normalize_drag_interaction_type
      )
    ) %>%
    mutate(
      correct_choice_id = pmap_chr(
        list(correct_choice_id, correct_answer, choices, format),
        derive_correct_choice_id
      )
    )
}

is_resistant_center_question <- function(question_text, concept_tag = NULL) {
  combined <- str_to_lower(paste(question_text %||% "", concept_tag %||% "", collapse = " "))
  str_detect(combined, "resistant|nonresistant|non resistant|right-skew|right skew|skewed|large outlier|outlier|extreme value") &&
    str_detect(combined, "measure of center|median|mean|center|typical")
}

clean_student_facing_source_language <- function(text) {
  text %||% "" %>%
    as.character() %>%
    str_replace_all(regex("\\b(on|from|in)\\s+the\\s+concept\\s+page\\b", ignore_case = TRUE), "") %>%
    str_replace_all(regex("\\bconcept\\s+page\\b", ignore_case = TRUE), "course material") %>%
    str_replace_all(regex("\\s+", ignore_case = TRUE), " ") %>%
    str_squish()
}

repair_question_bank_metadata <- function(bank) {
  if (!is.data.frame(bank) || nrow(bank) == 0) {
    return(bank)
  }
  resistant_rows <- map2_lgl(bank$question_text %||% rep("", nrow(bank)), bank$concept_tag %||% rep("", nrow(bank)), is_resistant_center_question)
  bank %>%
    mutate(
      topic_id = if_else(resistant_rows, "descriptive_stats", topic_id),
      concept_tag = if_else(resistant_rows, "resistant_measures", concept_tag),
      hint = if_else(
        resistant_rows,
        "Look for the measure of center that is not pulled much by very large or very small values.",
        clean_student_facing_source_language(hint)
      ),
      hint_1 = if_else(
        resistant_rows,
        "Focus on the phrase 'resistant to extreme values.' Which measure of center is based on the middle position instead of using every value?",
        clean_student_facing_source_language(hint_1)
      ),
      concept_explanation = if_else(
        resistant_rows,
        "When a distribution has extreme outliers, the median is usually preferred because it depends on position, not the size of every value. The mean uses every value, so large outliers pull it toward the tail.",
        clean_student_facing_source_language(concept_explanation)
      ),
      explanation = if_else(
        resistant_rows,
        "The median is resistant to outliers and skewness, making it a better summary of center than the mean when the distribution is skewed or contains extreme values. The mean gets pulled toward the tail or outliers.",
        clean_student_facing_source_language(explanation)
      ),
      solution_explanation = if_else(
        resistant_rows,
        "The median is the preferred center for strongly skewed data with large outliers because it stays near the middle position. The mean is nonresistant and is pulled toward the large values.",
        clean_student_facing_source_language(solution_explanation)
      )
    )
}

get_concept_label <- function(concept_tag) {
  normalized_tag <- normalize_review_concept_tag(concept_tag)
  reminder_entry <- get_review_concept_entry(normalized_tag)
  
  if (nrow(reminder_entry) > 0) {
    return(reminder_entry$concept_label[[1]])
  }
  
  if (is_valid_topic_id(normalized_tag, require_known = TRUE)) {
    return(get_topic_label(normalized_tag))
  }
  
  label <- normalized_tag %||% "unknown_concept"
  label %>%
    str_replace_all("_", " ") %>%
    str_squish() %>%
    str_to_title() %>%
    str_replace_all("\\bP Value\\b", "P-value") %>%
    str_replace_all("\\bType I\\b", "Type I") %>%
    str_replace_all("\\bType Ii\\b", "Type II") %>%
    str_replace_all("\\bBins\\b", "BINS")
}

markdown_to_ui <- function(text) {
  safe_text <- text %||% ""
  if (exists("clean_tutor_markdown", mode = "function")) {
    safe_text <- clean_tutor_markdown(safe_text)
  }
  safe_text <- normalize_help_markdown(safe_text)
  safe_text <- htmltools::htmlEscape(safe_text)
  
  tryCatch({
    rendered <- markdown::markdownToHTML(
      text = prep_math_for_html(safe_text),
      fragment.only = TRUE
    )
    withMathJax(div(class = "help-markdown", HTML(rendered)))
  }, error = function(e) {
    message(glue("[help] markdown render fallback: {conditionMessage(e)}"))
    div(
      class = "help-markdown",
      HTML(str_replace_all(safe_text, "\n", "<br/>"))
    )
  })
}

visual_src_for_ui <- function(path) {
  path <- as.character(path %||% "")
  if (!nzchar(path) || !fs::file_exists(path)) {
    return(NULL)
  }
  normalized_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  normalized_www <- normalizePath("www", winslash = "/", mustWork = FALSE)
  normalized_data_visuals <- normalizePath("data/visuals", winslash = "/", mustWork = FALSE)

  if (startsWith(normalized_path, paste0(normalized_www, "/"))) {
    return(fs::path_rel(normalized_path, normalized_www) %>% as.character())
  }
  if (startsWith(normalized_path, paste0(normalized_data_visuals, "/"))) {
    return(fs::path("local_visuals", fs::path_rel(normalized_path, normalized_data_visuals)) %>% as.character())
  }
  NULL
}

attach_visual_to_question <- function(question, visual_ids = NULL, tutor_visual_ids = NULL, visual_required = NULL) {
  question$visual_ids <- unique(c(question$visual_ids %||% character(), visual_ids %||% character())) %>%
    discard(~ is.na(.x) || !nzchar(.x))
  if (length(question$visual_ids) > 0) {
    question$visual_id <- question$visual_ids[[1]]
  }
  question$tutor_visual_ids <- unique(c(question$tutor_visual_ids %||% character(), tutor_visual_ids %||% character())) %>%
    discard(~ is.na(.x) || !nzchar(.x))
  if (!is.null(visual_required)) {
    question$visual_required <- isTRUE(visual_required)
  }
  question
}

get_question_visuals <- function(question, top_k = 3L, for_tutor = FALSE) {
  if (is.null(question)) {
    return(empty_image_metadata_table())
  }
  image_table <- load_visual_metadata_cached()
  explicit_ids <- unique(c(
    question$visual_id %||% character(),
    question$visual_ids %||% character(),
    if (isTRUE(for_tutor)) question$tutor_visual_ids %||% character() else character()
  )) %>% discard(~ is.na(.x) || !nzchar(.x))

  visuals <- if (length(explicit_ids) > 0 && is.data.frame(image_table) && nrow(image_table) > 0) {
    image_table %>%
      filter(image_id %in% explicit_ids) %>%
      mutate(final_visual_score = if_else(image_id == explicit_ids[[1]], 100, 90))
  } else {
    empty_image_metadata_table()
  }

  if (nrow(visuals) == 0 && (isTRUE(question$visual_required) || isTRUE(for_tutor))) {
    visuals <- retrieve_relevant_visuals(
      query = question$question_text %||% "",
      current_question = question,
      concept_tag = question$concept_tag,
      module_id = question$current_module_id %||% question$module_id,
      top_k = top_k
    )
  }

  if (!is.data.frame(visuals) || nrow(visuals) == 0) {
    return(empty_image_metadata_table())
  }
  keep <- map_lgl(seq_len(nrow(visuals)), ~ is_visual_safe_to_show(visuals[.x, , drop = FALSE]))
  visuals[keep, , drop = FALSE] %>%
    slice_head(n = top_k)
}

render_visual_cards <- function(visuals, label = "Visual aid") {
  if (!is.data.frame(visuals) || nrow(visuals) == 0) {
    return(NULL)
  }
  tagList(lapply(seq_len(nrow(visuals)), function(i) {
    visual <- visuals[i, , drop = FALSE]
    path <- get_visual_path(visual)
    src <- visual_src_for_ui(path)
    if (is.null(src)) {
      return(
        div(
          class = "visual-aid",
          div(class = "visual-aid-label", label),
          p(class = "small-muted", "A visual is linked to this item, but the image file is not available in this local run.")
        )
      )
    }
    div(
      class = "visual-aid",
      div(class = "visual-aid-label", label),
      tags$img(
        class = "visual-aid-img",
        src = src,
        alt = visual$caption[[1]] %||% "Introduction to Statistics visual aid"
      ),
      div(class = "visual-aid-caption", visual$caption[[1]] %||% visual$vision_description[[1]] %||% "")
    )
  }))
}

render_question_visuals <- function(question) {
  # Question-card visuals should be intentional. Optional visuals can still
  # appear inside tutor replies when a student asks for help or when the
  # question clearly matches a visual template.
  if (is.null(question) || !isTRUE(question$visual_required %||% FALSE)) {
    return(NULL)
  }
  render_visual_cards(get_question_visuals(question, top_k = 2L), label = "Visual aid")
}

render_tutor_visuals <- function(visuals) {
  render_visual_cards(visuals, label = "Here's a visual way to think about it")
}

row_value <- function(row, column, default = NA_character_) {
  if (is.null(row) || !is.data.frame(row) || !column %in% names(row)) {
    return(default)
  }
  value <- row[[column]][[1]] %||% default
  value %||% default
}

visual_metadata_to_message_visuals <- function(visuals, message_id = NULL, top_k = 2L) {
  if (!is.data.frame(visuals) || nrow(visuals) == 0) {
    return(list())
  }
  rows <- seq_len(min(nrow(visuals), top_k))
  out <- lapply(rows, function(i) {
    visual <- visuals[i, , drop = FALSE]
    path <- row_value(visual, "file_path", "")
    src <- visual_src_for_ui(path)
    if (is.null(src) || !nzchar(src)) {
      return(NULL)
    }
    list(
      message_id = message_id %||% NA_character_,
      visual_id = row_value(visual, "image_id", row_value(visual, "visual_id", paste0("visual_", i))),
      visual_type = "image",
      file_path = path,
      src = src,
      caption = row_value(visual, "caption", row_value(visual, "vision_description", "Visual aid")),
      source_type = row_value(visual, "source_type", "visual_metadata"),
      display_permission_status = row_value(visual, "display_permission_status", "unknown"),
      safe_for_deployment = isTRUE(row_value(visual, "safe_for_deployment", FALSE)),
      module_id = row_value(visual, "module_id", NA_character_),
      concept_tag = row_value(visual, "concept_tag", row_value(visual, "concept_tags", NA_character_))
    )
  })
  out[!vapply(out, is.null, logical(1))]
}

render_tutor_message_visuals <- function(visuals) {
  visuals <- if (exists("normalize_tutor_message_visuals", mode = "function")) {
    normalize_tutor_message_visuals(visuals)
  } else {
    visuals %||% list()
  }
  if (length(visuals) == 0) {
    return(NULL)
  }
  cards <- lapply(visuals, function(visual) {
    src <- visual$src %||% visual_src_for_ui(visual$file_path %||% "")
    if (is.null(src) || !nzchar(src)) {
      return(NULL)
    }
    div(
      class = "visual-aid tutor-bubble-visual",
      div(class = "visual-aid-label", "Visual aid"),
      tags$img(
        class = "visual-aid-img",
        src = src,
        alt = visual$caption %||% "Introduction to Statistics visual aid"
      ),
      div(class = "visual-aid-caption", visual$caption %||% "")
    )
  })
  cards <- cards[!vapply(cards, is.null, logical(1))]
  if (length(cards) == 0) {
    return(NULL)
  }
  tagList(cards)
}

starter_question_bank <- dplyr::bind_rows(
  question_row(
    question_id = "q_data_graphs_mc_1",
    topic_id = "data_graphs",
    module_id = "module_1",
    format = "multiple_choice",
    difficulty = "easy",
    question_text = "Which variable type best describes an area code?",
    choices = c("Quantitative", "Categorical/nominal", "Quantitative discrete", "Ordinal numerical"),
    correct_answer = "Categorical/nominal",
    accepted_answers = character(),
    hint = "Ask whether arithmetic with the value makes sense.",
    explanation = "An area code acts like a label, not a measurement, so it should be treated as categorical/nominal."
  ),
  question_row(
    question_id = "q_data_graphs_fill_1",
    topic_id = "data_graphs",
    module_id = "module_1",
    format = "fill_in_blank",
    difficulty = "medium",
    question_text = "A bar chart is most appropriate for displaying a ____ variable.",
    choices = character(),
    correct_answer = "categorical",
    accepted_answers = c("categorical", "qualitative"),
    hint = "Think about graphs used for labels or categories rather than measured values.",
    explanation = "Bar charts summarize counts or proportions for categories, so they are used for categorical variables.",
    visual_id = "recreated_bar_chart_categorical",
    visual_required = TRUE,
    tutor_visual_ids = c("recreated_bar_chart_categorical", "recreated_histogram_quantitative"),
    hint_2 = "Look at whether the x-axis has names/categories or measured intervals."
  ),
  question_row(
    question_id = "q_descriptive_stats_fill_1",
    topic_id = "descriptive_stats",
    module_id = "module_1",
    format = "fill_in_blank",
    difficulty = "easy",
    question_text = "The interquartile range is Q3 minus ____.",
    choices = character(),
    correct_answer = "Q1",
    accepted_answers = c("q1", "first quartile"),
    hint = "The IQR uses the middle 50% of the distribution.",
    explanation = "The interquartile range is Q3 - Q1, measuring the spread of the middle half of the data."
  ),
  question_row(
    question_id = "q_descriptive_stats_cba_1",
    topic_id = "descriptive_stats",
    module_id = "module_1",
    format = "choose_best_answer",
    difficulty = "hard",
    question_text = "Which statistic is most resistant to high outliers?",
    choices = c("Mean", "Standard deviation", "Median", "Range"),
    correct_answer = "Median",
    accepted_answers = character(),
    hint = "Resistant statistics do not move much when extreme values appear.",
    explanation = "The median is resistant because it depends on order, not the exact size of extreme observations."
  ),
  question_row(
    question_id = "q_relationships_regression_mc_1",
    topic_id = "relationships_regression",
    module_id = "module_2",
    format = "multiple_choice",
    difficulty = "easy",
    question_text = "Which sentence correctly interprets a positive slope in context?",
    choices = c(
      "For each one-unit increase in x, the predicted y increases by the slope amount on average.",
      "A positive slope means the data are perfectly linear.",
      "A positive slope proves causation.",
      "A positive slope means the correlation must be 1."
    ),
    correct_answer = "For each one-unit increase in x, the predicted y increases by the slope amount on average.",
    accepted_answers = character(),
    hint = "Slope describes predicted change in the response for a one-unit increase in the explanatory variable.",
    explanation = "A positive slope means the predicted response increases as the explanatory variable increases, on average."
  ),
  question_row(
    question_id = "q_producing_data_mc_1",
    topic_id = "producing_data",
    module_id = "module_3",
    format = "multiple_choice",
    difficulty = "medium",
    question_text = "Which design feature helps support cause-and-effect conclusions?",
    choices = c("Random sampling", "Random assignment", "Convenience sampling", "Voluntary response"),
    correct_answer = "Random assignment",
    accepted_answers = character(),
    hint = "Random sampling and random assignment do different jobs.",
    explanation = "Random assignment helps create comparable treatment groups, which supports cause-and-effect conclusions."
  ),
  question_row(
    question_id = "q_probability_basics_fill_1",
    topic_id = "probability_basics",
    module_id = "module_4",
    format = "fill_in_blank",
    difficulty = "easy",
    question_text = "The complement rule says P(A^c) = 1 - ____.",
    choices = character(),
    correct_answer = "P(A)",
    accepted_answers = c("p(a)", "p of a"),
    hint = "The event and its complement fill the whole sample space.",
    explanation = "Because an event and its complement add up to the full sample space, their probabilities sum to 1."
  ),
  question_row(
    question_id = "q_probability_basics_drag_1",
    topic_id = "probability_basics",
    module_id = "module_4",
    format = "drag_and_drop",
    difficulty = "medium",
    question_text = "Checklist placeholder: select every statement that must be true for any probability model.",
    choices = c(
      "Probabilities are between 0 and 1",
      "Probabilities of all outcomes add to 1",
      "Probabilities can be negative",
      "A larger probability means a more likely outcome"
    ),
    correct_answer = c(
      "Probabilities are between 0 and 1",
      "Probabilities of all outcomes add to 1",
      "A larger probability means a more likely outcome"
    ),
    accepted_answers = character(),
    hint = "Review the basic probability rules before computing.",
    explanation = "Valid probability models keep all probabilities between 0 and 1 and total probability equal to 1."
  ),
  question_row(
    question_id = "q_normal_dist_mc_1",
    topic_id = "normal_dist",
    module_id = "module_5",
    format = "multiple_choice",
    difficulty = "easy",
    question_text = "A z-score tells you how many ____ an observation is from the mean.",
    choices = c("percentages", "standard deviations", "sample sizes", "quartiles"),
    correct_answer = "standard deviations",
    accepted_answers = character(),
    hint = "A z-score standardizes a raw value.",
    explanation = "A z-score measures distance from the mean in standard deviation units.",
    visual_id = "recreated_standard_normal_curve",
    tutor_visual_ids = "recreated_standard_normal_curve",
    hint_2 = "On the normal curve, z = 0 is the mean; positive z-scores move to the right."
  ),
  question_row(
    question_id = "q_binomial_dist_drag_1",
    topic_id = "binomial_dist",
    module_id = "module_5",
    format = "drag_and_drop",
    difficulty = "medium",
    question_text = "Checklist placeholder: select every condition in the BINS checklist for a binomial setting.",
    choices = c(
      "Binary outcomes",
      "Independent trials",
      "Fixed number of trials",
      "Same probability on each trial",
      "Changing probability from trial to trial"
    ),
    correct_answer = c(
      "Binary outcomes",
      "Independent trials",
      "Fixed number of trials",
      "Same probability on each trial"
    ),
    accepted_answers = character(),
    hint = "Use BINS to decide whether a binomial model fits the situation.",
    explanation = "A binomial setting requires binary outcomes, independent trials, a fixed number of trials, and a constant probability of success."
  ),
  question_row(
    question_id = "q_sampling_dist_cba_1",
    topic_id = "sampling_dist",
    module_id = "module_6",
    format = "choose_best_answer",
    difficulty = "medium",
    question_text = "What is a sampling distribution?",
    choices = c(
      "The distribution of a statistic across repeated samples.",
      "The distribution of raw data in one sample only.",
      "A list of all values in the population.",
      "A graph of residuals from a regression line."
    ),
    correct_answer = "The distribution of a statistic across repeated samples.",
    accepted_answers = character(),
    hint = "A sampling distribution is about a statistic, not the raw observations from one sample.",
    explanation = "A sampling distribution describes how a statistic varies from sample to sample."
  ),
  question_row(
    question_id = "q_ci_prop_mc_1",
    topic_id = "ci_prop",
    module_id = "module_7",
    format = "multiple_choice",
    difficulty = "medium",
    question_text = "What does a confidence interval for a proportion estimate?",
    choices = c("A population proportion p", "A sample proportion p-hat", "A population mean mu", "A sample standard deviation s"),
    correct_answer = "A population proportion p",
    accepted_answers = character(),
    hint = "The sample statistic is used to estimate an unknown population parameter.",
    explanation = "A confidence interval for a proportion estimates the unknown population proportion p.",
    tutor_visual_ids = "recreated_confidence_interval_number_line",
    concept_explanation = "A confidence interval uses a sample estimate and margin of error to describe plausible values for an unknown parameter."
  ),
  question_row(
    question_id = "q_ci_mean_fill_1",
    topic_id = "ci_mean",
    module_id = "module_7",
    format = "fill_in_blank",
    difficulty = "medium",
    question_text = "The standard error for a sample mean is s / sqrt(____).",
    choices = character(),
    correct_answer = "n",
    accepted_answers = c("n", "sample size"),
    hint = "Larger samples shrink standard error.",
    explanation = "For a sample mean, the standard error is s / sqrt(n)."
  ),
  question_row(
    question_id = "q_ht_foundations_cba_1",
    topic_id = "ht_foundations",
    module_id = "module_8",
    format = "choose_best_answer",
    difficulty = "medium",
    question_text = "What does a small p-value suggest?",
    choices = c(
      "The observed result would be unusual if the null hypothesis were true.",
      "The null hypothesis is definitely false.",
      "The p-value is the probability the null hypothesis is true.",
      "The result must be practically important."
    ),
    correct_answer = "The observed result would be unusual if the null hypothesis were true.",
    accepted_answers = character(),
    hint = "Do not interpret the p-value as the probability that the null hypothesis is true.",
    explanation = "A small p-value means the observed data would be unusual under the null model.",
    visual_id = "recreated_p_value_tail_area",
    tutor_visual_ids = c("recreated_p_value_tail_area", "recreated_standard_normal_curve"),
    hint_2 = "Picture the p-value as tail area beyond your observed statistic under the null model."
  ),
  question_row(
    question_id = "q_ht_prop_mc_1",
    topic_id = "ht_prop",
    module_id = "module_8",
    format = "multiple_choice",
    difficulty = "hard",
    question_text = "In a one-proportion z test, the standard error uses which proportion?",
    choices = c("The sample proportion p-hat", "The null value p0", "The confidence level", "The margin of error"),
    correct_answer = "The null value p0",
    accepted_answers = character(),
    hint = "A test standard error is built under the null model.",
    explanation = "A one-proportion z test uses the null value p0 in the standard error."
  ),
  question_row(
    question_id = "q_ht_mean_fill_1",
    topic_id = "ht_mean",
    module_id = "module_8",
    format = "fill_in_blank",
    difficulty = "hard",
    question_text = "A one-mean t statistic often has denominator s / sqrt(____).",
    choices = character(),
    correct_answer = "n",
    accepted_answers = c("n", "sample size"),
    hint = "The denominator is the estimated standard error of the sample mean.",
    explanation = "For a one-mean t procedure, the estimated standard error is s / sqrt(n)."
  ),
  question_row(
    question_id = "q_uses_abuses_tests_cba_1",
    topic_id = "uses_abuses_tests",
    module_id = "module_9",
    format = "choose_best_answer",
    difficulty = "medium",
    question_text = "Which statement is safest after a statistically significant result?",
    choices = c(
      "The result provides evidence against the null, but practical importance and study design still matter.",
      "The treatment definitely has a large real-world effect.",
      "The result proves the alternative hypothesis is true.",
      "The p-value gives the probability the null hypothesis is correct."
    ),
    correct_answer = "The result provides evidence against the null, but practical importance and study design still matter.",
    accepted_answers = character(),
    hint = "Statistical significance is not the same as practical importance.",
    explanation = "A significant result can still have limited practical importance or design limitations."
  ),
  question_row(
    question_id = "q_final_review_drag_1",
    topic_id = "final_review",
    module_id = "cumulative_review",
    format = "drag_and_drop",
    difficulty = "medium",
    question_text = "Checklist placeholder: select the planning moves that help you choose the right method on a mixed review problem.",
    choices = c(
      "Identify the variable type",
      "Decide whether the goal is describe, estimate, test, or compute probability",
      "Check conditions for the method",
      "Ignore context and compute immediately"
    ),
    correct_answer = c(
      "Identify the variable type",
      "Decide whether the goal is describe, estimate, test, or compute probability",
      "Check conditions for the method"
    ),
    accepted_answers = character(),
    hint = "Mixed review questions are easier when you classify the task first.",
    explanation = "Strong mixed-review work starts by identifying the variable type, the goal, and any needed conditions."
  ),
  question_row(
    question_id = "q_generic_mc_1",
    topic_id = "__generic__",
    module_id = "cumulative_review",
    format = "multiple_choice",
    difficulty = "easy",
    question_text = "When you begin a {topic_label} problem, which question is most useful first?",
    choices = c(
      "What quantity or idea is the problem asking me to identify or compute?",
      "How quickly can I start plugging numbers into a formula?",
      "Which answer choice looks longest?",
      "How can I ignore the context and focus only on symbols?"
    ),
    correct_answer = "What quantity or idea is the problem asking me to identify or compute?",
    accepted_answers = character(),
    hint = "A strong first step is to name the target quantity or idea.",
    explanation = "Naming the target quantity first keeps your method aligned with the problem."
  ),
  question_row(
    question_id = "q_generic_fill_1",
    topic_id = "__generic__",
    module_id = "cumulative_review",
    format = "fill_in_blank",
    difficulty = "easy",
    question_text = "Before calculating in {topic_label}, identify the parameter, statistic, or ____ the question is targeting.",
    choices = character(),
    correct_answer = "quantity",
    accepted_answers = c("quantity", "value", "value of interest"),
    hint = "Clarify the target before doing algebra.",
    explanation = "If you identify the target quantity first, the rest of the setup becomes much easier to organize."
  ),
  question_row(
    question_id = "q_generic_cba_1",
    topic_id = "__generic__",
    module_id = "cumulative_review",
    format = "choose_best_answer",
    difficulty = "medium",
    question_text = "Which study habit best supports solving {topic_label} questions correctly?",
    choices = c(
      "Match the question's goal to the correct statistical idea before doing calculations.",
      "Memorize isolated formulas without checking context.",
      "Skip conditions and trust the first method that comes to mind.",
      "Treat every word problem as the same type of inference task."
    ),
    correct_answer = "Match the question's goal to the correct statistical idea before doing calculations.",
    accepted_answers = character(),
    hint = "The method should match the task and the context.",
    explanation = "Students are more accurate when they identify the task first and then select a method that fits the problem."
  ),
  question_row(
    question_id = "q_generic_drag_1",
    topic_id = "__generic__",
    module_id = "cumulative_review",
    format = "drag_and_drop",
    difficulty = "medium",
    question_text = "Checklist placeholder: select the steps that belong in a careful setup for a {topic_label} problem.",
    choices = c("Identify the data type", "Identify the target quantity", "Check required conditions", "Randomly change notation"),
    correct_answer = c("Identify the data type", "Identify the target quantity", "Check required conditions"),
    accepted_answers = character(),
    hint = "A good setup usually happens before any calculator work.",
    explanation = "Careful setup means identifying the data type, clarifying the target quantity, and checking conditions before computing."
  )
 ) %>%
  mutate(
    concept_tag = case_when(
      question_id == "q_data_graphs_mc_1" ~ "variable_type_identification",
      question_id == "q_data_graphs_fill_1" ~ "graph_selection",
      question_id == "q_descriptive_stats_fill_1" ~ "iqr_definition",
      question_id == "q_descriptive_stats_cba_1" ~ "resistant_statistics",
      question_id == "q_relationships_regression_mc_1" ~ "slope_interpretation",
      question_id == "q_producing_data_mc_1" ~ "random_assignment",
      question_id == "q_probability_basics_fill_1" ~ "complement_rule",
      question_id == "q_probability_basics_drag_1" ~ "probability_model_rules",
      question_id == "q_normal_dist_mc_1" ~ "z_score_interpretation",
      question_id == "q_binomial_dist_drag_1" ~ "binomial_conditions",
      question_id == "q_sampling_dist_cba_1" ~ "sampling_distribution_definition",
      question_id == "q_ci_prop_mc_1" ~ "ci_interpretation",
      question_id == "q_ci_mean_fill_1" ~ "mean_standard_error",
      question_id == "q_ht_foundations_cba_1" ~ "p_value_interpretation",
      question_id == "q_ht_prop_mc_1" ~ "one_proportion_test_setup",
      question_id == "q_ht_mean_fill_1" ~ "one_mean_test_statistic",
      question_id == "q_uses_abuses_tests_cba_1" ~ "statistical_vs_practical_significance",
      question_id == "q_final_review_drag_1" ~ "method_selection",
      str_detect(question_id, "^q_generic_") ~ "careful_setup",
      TRUE ~ concept_tag
    )
  )

load_external_question_bank <- function(path = QUESTION_BANK_PATH) {
  if (!file.exists(path)) {
    return(NULL)
  }
  
  required_cols <- c(
    "question_id", "module_id", "topic_id", "concept_tag", "difficulty", "format",
    "question_text", "choices", "correct_answer", "accepted_answers", "hint", "explanation"
  )
  
  bank <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  missing_cols <- setdiff(required_cols, names(bank))
  if (length(missing_cols) > 0) {
    warning(glue("External question bank is missing required columns: {paste(missing_cols, collapse = ', ')}. Falling back to starter bank."))
    return(NULL)
  }
  if (!"correct_choice_id" %in% names(bank)) {
    bank$correct_choice_id <- NA_character_
  }
  if (!"interaction_type" %in% names(bank)) {
    bank$interaction_type <- NA_character_
  }
  optional_text_cols <- c(
    "visual_id", "visual_position", "hint_1", "hint_2", "hint_3",
    "concept_explanation", "solution_explanation", "question_family", "answer_type",
    "grading_rule", "misconception_notes", "visual_template_id", "visual_params",
    "generation_method", "evidence_used", "safe_for_demo", "created_at", "reviewed_status"
  )
  for (col in optional_text_cols) {
    if (!col %in% names(bank)) bank[[col]] <- NA_character_
  }
  if (!"visual_required" %in% names(bank)) {
    bank$visual_required <- FALSE
  }
  if (!"visual_ids" %in% names(bank)) {
    bank$visual_ids <- ""
  }
  if (!"tutor_visual_ids" %in% names(bank)) {
    bank$tutor_visual_ids <- ""
  }
  
  bank %>%
    mutate(
      choices = map(choices, deserialize_choice_objects),
      correct_choice_id = as.character(correct_choice_id),
      interaction_type = as.character(interaction_type %||% NA_character_),
      correct_answer = map(correct_answer, deserialize_text_vector),
      accepted_answers = map(accepted_answers, deserialize_text_vector),
      visual_ids = map(visual_ids, deserialize_text_vector),
      tutor_visual_ids = map(tutor_visual_ids, deserialize_text_vector),
      visual_required = visual_required %in% c(TRUE, "TRUE", "true", "1", 1),
      hint = na_if(hint, ""),
      explanation = na_if(explanation, "")
    ) %>%
    select(any_of(c(
      "question_id", "module_id", "topic_id", "concept_tag", "difficulty", "format",
      "question_text", "choices", "interaction_type", "correct_choice_id", "correct_answer", "accepted_answers", "hint", "explanation",
      "visual_id", "visual_ids", "visual_position", "visual_required", "tutor_visual_ids",
      "visual_template_id", "visual_params",
      "hint_1", "hint_2", "hint_3", "concept_explanation", "solution_explanation",
      "question_family", "answer_type", "grading_rule", "misconception_notes", "generation_method", "evidence_used", "safe_for_demo", "created_at", "reviewed_status",
      "module_label", "topic_label", "source_basis", "review_status", "generated_by"
    ))) %>%
    mutate(
      module_id = as.character(module_id),
      topic_id = as.character(topic_id),
      concept_tag = as.character(concept_tag),
      difficulty = as.character(difficulty),
      format = as.character(format),
      question_text = as.character(question_text)
    ) %>%
    as_question_row() %>%
    repair_question_bank_metadata() %>%
    filter_unsafe_fill_in_blank_questions(source_label = basename(path)) %>%
    filter_unanswerable_practice_questions(source_label = basename(path))
}

load_runtime_question_bank <- function(path = QUESTION_BANK_PATH, fallback_bank = starter_question_bank) {
  safe_fallback_bank <- filter_unsafe_fill_in_blank_questions(
    fallback_bank %>% as_question_row() %>% repair_question_bank_metadata(),
    source_label = "starter bank"
  ) %>%
    filter_unanswerable_practice_questions(source_label = "starter bank")
  
  external_bank <- tryCatch(
    load_external_question_bank(path),
    error = function(e) {
      warning(glue("Could not load external question bank from {path}: {conditionMessage(e)}. Falling back to starter bank."))
      NULL
    }
  )
  
  if (is.null(external_bank) || nrow(external_bank) == 0) {
    return(safe_fallback_bank)
  }
  
  external_bank
}

load_question_bank_cached <- function(path = QUESTION_BANK_PATH, fallback_bank = starter_question_bank, refresh = FALSE) {
  cache_key <- "question_bank"
  if (isTRUE(refresh) || !exists(cache_key, envir = .app_cache, inherits = FALSE)) {
    start_time <- Sys.time()
    .app_cache$question_bank <- load_runtime_question_bank(path = path, fallback_bank = fallback_bank)
    message(glue("[timer] loaded question bank in {round(as.numeric(difftime(Sys.time(), start_time, units = 'secs')), 2)} sec"))
  }
  .app_cache$question_bank
}

load_visual_metadata_cached <- function(refresh = FALSE) {
  cache_key <- "image_metadata"
  if (isTRUE(refresh) || !exists(cache_key, envir = .app_cache, inherits = FALSE)) {
    start_time <- Sys.time()
    .app_cache$image_metadata <- if (exists("load_image_metadata", mode = "function")) load_image_metadata() else tibble()
    message(glue("[timer] loaded visual metadata in {round(as.numeric(difftime(Sys.time(), start_time, units = 'secs')), 2)} sec"))
  }
  .app_cache$image_metadata
}

load_module_metadata_cached <- function(refresh = FALSE) {
  cache_key <- "module_metadata"
  if (isTRUE(refresh) || !exists(cache_key, envir = .app_cache, inherits = FALSE)) {
    .app_cache$module_metadata <- MODULES
  }
  .app_cache$module_metadata
}

question_bank <- load_question_bank_cached()

get_db <- function() {
  DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
}

init_db <- function() {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id TEXT PRIMARY KEY,
      display_name TEXT,
      role TEXT
    )
  ")
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS practice_attempts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id TEXT,
      topic_id TEXT,
      question_format TEXT,
      difficulty TEXT,
      correct INTEGER,
      hints_used INTEGER,
      question_id TEXT,
      student_answer TEXT,
      module_id TEXT,
      concept_tag TEXT,
      ts DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS help_queries (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id TEXT,
      topic_id TEXT,
      module_id TEXT,
      concept_tag TEXT,
      query_text TEXT,
      response_text TEXT,
      error_message TEXT,
      ts DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS generated_questions (
      question_id TEXT PRIMARY KEY,
      parent_question_id TEXT,
      module_id TEXT,
      topic_id TEXT,
      concept_tag TEXT,
      format TEXT,
      interaction_type TEXT,
      difficulty TEXT,
      question_text TEXT,
      choices TEXT,
      correct_choice_id TEXT,
      correct_answer TEXT,
      accepted_answers TEXT,
      hint TEXT,
      explanation TEXT,
      generated_by TEXT,
      ts DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS more_like_this_requests (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id TEXT,
      question_id TEXT,
      topic_id TEXT,
      module_id TEXT,
      concept_tag TEXT,
      ts DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  attempt_cols <- DBI::dbListFields(con, "practice_attempts")
  if (!"question_id" %in% attempt_cols) {
    DBI::dbExecute(con, "ALTER TABLE practice_attempts ADD COLUMN question_id TEXT")
  }
  if (!"student_answer" %in% attempt_cols) {
    DBI::dbExecute(con, "ALTER TABLE practice_attempts ADD COLUMN student_answer TEXT")
  }
  if (!"module_id" %in% attempt_cols) {
    DBI::dbExecute(con, "ALTER TABLE practice_attempts ADD COLUMN module_id TEXT")
  }
  if (!"concept_tag" %in% attempt_cols) {
    DBI::dbExecute(con, "ALTER TABLE practice_attempts ADD COLUMN concept_tag TEXT")
  }
  
  help_cols <- DBI::dbListFields(con, "help_queries")
  if (!"module_id" %in% help_cols) {
    DBI::dbExecute(con, "ALTER TABLE help_queries ADD COLUMN module_id TEXT")
  }
  if (!"concept_tag" %in% help_cols) {
    DBI::dbExecute(con, "ALTER TABLE help_queries ADD COLUMN concept_tag TEXT")
  }
  if (!"response_text" %in% help_cols) {
    DBI::dbExecute(con, "ALTER TABLE help_queries ADD COLUMN response_text TEXT")
  }
  if (!"error_message" %in% help_cols) {
    DBI::dbExecute(con, "ALTER TABLE help_queries ADD COLUMN error_message TEXT")
  }
  
  generated_cols <- DBI::dbListFields(con, "generated_questions")
  if (!"parent_question_id" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN parent_question_id TEXT")
  }
  if (!"module_id" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN module_id TEXT")
  }
  if (!"topic_id" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN topic_id TEXT")
  }
  if (!"concept_tag" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN concept_tag TEXT")
  }
  if (!"format" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN format TEXT")
  }
  if (!"difficulty" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN difficulty TEXT")
  }
  if (!"interaction_type" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN interaction_type TEXT")
  }
  if (!"question_text" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN question_text TEXT")
  }
  if (!"choices" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN choices TEXT")
  }
  if (!"correct_choice_id" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN correct_choice_id TEXT")
  }
  if (!"correct_answer" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN correct_answer TEXT")
  }
  if (!"accepted_answers" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN accepted_answers TEXT")
  }
  if (!"hint" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN hint TEXT")
  }
  if (!"explanation" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN explanation TEXT")
  }
  if (!"generated_by" %in% generated_cols) {
    DBI::dbExecute(con, "ALTER TABLE generated_questions ADD COLUMN generated_by TEXT")
  }
  
  mlt_cols <- DBI::dbListFields(con, "more_like_this_requests")
  if (!"question_id" %in% mlt_cols) {
    DBI::dbExecute(con, "ALTER TABLE more_like_this_requests ADD COLUMN question_id TEXT")
  }
  if (!"topic_id" %in% mlt_cols) {
    DBI::dbExecute(con, "ALTER TABLE more_like_this_requests ADD COLUMN topic_id TEXT")
  }
  if (!"module_id" %in% mlt_cols) {
    DBI::dbExecute(con, "ALTER TABLE more_like_this_requests ADD COLUMN module_id TEXT")
  }
  if (!"concept_tag" %in% mlt_cols) {
    DBI::dbExecute(con, "ALTER TABLE more_like_this_requests ADD COLUMN concept_tag TEXT")
  }
  
  existing <- DBI::dbReadTable(con, "users")
  missing_users <- dplyr::anti_join(student_accounts, existing, by = "user_id")
  
  if (nrow(missing_users) > 0) {
    to_write <- missing_users %>%
      transmute(
        user_id,
        display_name,
        role
      )
    DBI::dbWriteTable(con, "users", to_write, append = TRUE)
  }
}

get_user_from_db <- function(user_id) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbGetQuery(
    con,
    "SELECT user_id, display_name, role FROM users WHERE user_id = ?",
    params = list(user_id)
  )
}

record_attempt <- function(user_id, topic_id, question_format, difficulty, correct, hints_used, question_id, student_answer, module_id, concept_tag) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbWriteTable(
    con,
    "practice_attempts",
    tibble(
      user_id = user_id,
      topic_id = topic_id,
      question_format = question_format,
      difficulty = difficulty,
      correct = as.integer(correct),
      hints_used = as.integer(hints_used),
      question_id = question_id,
      student_answer = student_answer,
      module_id = module_id,
      concept_tag = concept_tag
    ),
    append = TRUE
  )
}

record_help_query <- function(user_id, topic_id, query_text, response_text = NA_character_, error_message = NA_character_, module_id = NULL, concept_tag = NULL) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  resolved_topic_id <- sanitize_topic_id(topic_id, require_known = TRUE)
  module_id <- if (!is.null(resolved_topic_id)) {
    get_module_for_topic(resolved_topic_id)
  } else {
    normalize_scalar_string(module_id) %||% NA_character_
  }
  concept_tag <- normalize_review_concept_tag(concept_tag, resolved_topic_id) %||%
    if (!is.null(resolved_topic_id)) normalize_review_concept_tag(get_concept_tag_for_topic(resolved_topic_id), resolved_topic_id) else NA_character_
  
  DBI::dbWriteTable(
    con,
    "help_queries",
    tibble(
      user_id = user_id,
      topic_id = resolved_topic_id %||% NA_character_,
      module_id = module_id,
      concept_tag = concept_tag,
      query_text = query_text,
      response_text = response_text,
      error_message = error_message
    ),
    append = TRUE
  )
}

safe_record_help_query <- function(...) {
  tryCatch({
    record_help_query(...)
    message("[help] help query saved: yes")
    list(ok = TRUE, error = NULL)
  }, error = function(e) {
    warning(glue("Failed to save help query: {conditionMessage(e)}"), call. = FALSE)
    message(glue("[help] help query saved: no ({conditionMessage(e)})"))
    list(ok = FALSE, error = conditionMessage(e))
  })
}

record_generated_question <- function(question) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  if (is.null(question$question_id) || !nzchar(question$question_id)) {
    return(invisible(FALSE))
  }
  
  existing <- DBI::dbGetQuery(
    con,
    "SELECT question_id FROM generated_questions WHERE question_id = ?",
    params = list(question$question_id)
  )
  
  if (nrow(existing) > 0) {
    return(invisible(TRUE))
  }
  
  DBI::dbWriteTable(
    con,
    "generated_questions",
    tibble(
      question_id = question$question_id,
      parent_question_id = question$parent_question_id %||% NA_character_,
      module_id = question$module_id %||% NA_character_,
      topic_id = question$topic_id %||% NA_character_,
      concept_tag = question$concept_tag %||% get_concept_tag_for_topic(question$topic_id %||% ""),
      format = question$format %||% NA_character_,
      interaction_type = question$interaction_type %||% NA_character_,
      difficulty = question$difficulty %||% NA_character_,
      question_text = question$question_text %||% NA_character_,
      choices = serialize_choice_objects(question$choices),
      correct_choice_id = question$correct_choice_id %||% NA_character_,
      correct_answer = serialize_text_vector(question$correct_answer),
      accepted_answers = serialize_text_vector(question$accepted_answers),
      hint = question$hint %||% NA_character_,
      explanation = question$explanation %||% NA_character_,
      generated_by = question$generated_by %||% "claude"
    ),
    append = TRUE
  )
  if (exists("question_pool", envir = .app_cache, inherits = FALSE)) {
    rm(question_pool, envir = .app_cache)
  }
  
  invisible(TRUE)
}

get_generated_questions <- function(topic_id = NULL, parent_question_id = NULL) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  query <- "SELECT question_id, parent_question_id, module_id, topic_id, concept_tag, format, interaction_type, difficulty, question_text, choices, correct_choice_id, correct_answer, accepted_answers, hint, explanation, generated_by FROM generated_questions"
  params <- list()
  clauses <- character()
  topic_id <- sanitize_topic_id(topic_id)
  parent_question_id <- normalize_scalar_string(parent_question_id)
  
  if (!is.null(topic_id)) {
    clauses <- c(clauses, "topic_id = ?")
    params <- c(params, list(topic_id))
  }
  if (!is.null(parent_question_id)) {
    clauses <- c(clauses, "parent_question_id = ?")
    params <- c(params, list(parent_question_id))
  }
  if (length(clauses) > 0) {
    query <- paste(query, "WHERE", paste(clauses, collapse = " AND "))
  }
  
  rows <- if (length(params) > 0) {
    DBI::dbGetQuery(con, query, params = params)
  } else {
    DBI::dbGetQuery(con, query)
  }
  if (nrow(rows) == 0) {
    return(question_bank[0, ] %>% mutate(parent_question_id = character(), generated_by = character()))
  }
  
  rows %>%
    mutate(
      choices = map(choices, deserialize_choice_objects),
      correct_answer = map(correct_answer, deserialize_text_vector),
      accepted_answers = map(accepted_answers, deserialize_text_vector)
    ) %>%
    as_question_row() %>%
    filter_unsafe_fill_in_blank_questions(source_label = "generated questions", warn = FALSE)
}

record_more_like_this_request <- function(user_id, question) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbWriteTable(
    con,
    "more_like_this_requests",
    tibble(
      user_id = user_id,
      question_id = question$question_id %||% NA_character_,
      topic_id = question$topic_id %||% NA_character_,
      module_id = question$module_id %||% NA_character_,
      concept_tag = question$concept_tag %||% NA_character_
    ),
    append = TRUE
  )
}

get_more_like_this_requests <- function(user_id) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  normalize_ts_column(DBI::dbGetQuery(
    con,
    "SELECT id, user_id, question_id, topic_id, module_id, concept_tag, ts FROM more_like_this_requests WHERE user_id = ? ORDER BY ts DESC",
    params = list(user_id)
  ))
}

get_user_attempts <- function(user_id) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  normalize_ts_column(DBI::dbGetQuery(
    con,
    "SELECT * FROM practice_attempts WHERE user_id = ? ORDER BY ts DESC",
    params = list(user_id)
  ))
}

get_user_help_queries <- function(user_id) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  normalize_ts_column(DBI::dbGetQuery(
    con,
    "SELECT id, user_id, topic_id, module_id, concept_tag, query_text, response_text, error_message, ts FROM help_queries WHERE user_id = ? ORDER BY ts DESC",
    params = list(user_id)
  ))
}

get_mastery <- function(user_id) {
  topics <- get_topic_meta() %>%
    select(module_id, module_label, module_order, topic_id, student_label, topic_order)
  
  attempts <- get_user_attempts(user_id)
  
  if (nrow(attempts) == 0) {
    return(
      topics %>%
        mutate(
          attempts = 0L,
          accuracy = NA_real_,
          avg_hints = NA_real_,
          mastery = 0
        )
    )
  }
  
  topics %>%
    left_join(
      attempts %>%
        group_by(topic_id) %>%
        summarise(
          attempts = n(),
          accuracy = mean(correct, na.rm = TRUE),
          avg_hints = mean(hints_used, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "topic_id"
    ) %>%
    mutate(
      attempts = coalesce(attempts, 0L),
      mastery = case_when(
        attempts == 0 ~ 0,
        accuracy >= 0.85 & avg_hints <= 0.5 ~ 100,
        accuracy >= 0.70 ~ 80,
        accuracy >= 0.50 ~ 55,
        TRUE ~ 25
      )
    )
}

get_module_progress <- function(user_id) {
  mastery <- get_mastery(user_id)
  
  mastery %>%
    group_by(module_id, module_label, module_order) %>%
    summarise(
      topics_in_module = n(),
      attempted_topics = sum(attempts > 0),
      total_attempts = sum(attempts),
      avg_accuracy = mean(accuracy, na.rm = TRUE),
      avg_mastery = mean(mastery, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      avg_accuracy = ifelse(is.nan(avg_accuracy), NA_real_, avg_accuracy),
      avg_mastery = ifelse(is.nan(avg_mastery), 0, avg_mastery)
    ) %>%
    arrange(module_order)
}

empty_concept_sections <- function() {
  list(
    explanation = "",
    formula_notes = "",
    common_mistakes = "",
    reminder_bullets = character(),
    formula = "",
    common_mistake = ""
  )
}

empty_weak_concepts <- function() {
  tibble(
    concept_tag = character(),
    topic_id = character(),
    module_id = character(),
    student_label = character(),
    module_label = character(),
    module_order = integer(),
    topic_order = integer(),
    recent_attempts = integer(),
    recent_correct = integer(),
    recent_hint_total = integer(),
    wrong_count = integer(),
    heavy_hint_count = integer(),
    recent_accuracy = numeric(),
    mastered = logical(),
    help_count = integer(),
    weakness_score = numeric(),
    still_weak = logical(),
    reason = character(),
    explanation = character(),
    formula = character(),
    common_mistake = character(),
    next_action = character()
  )
}

extract_concept_sections <- function(topic_id) {
  topic_id <- sanitize_topic_id(topic_id, require_known = TRUE)
  if (is.null(topic_id)) {
    return(empty_concept_sections())
  }
  
  page <- tryCatch(
    get_concept_page(topic_id),
    error = function(e) NULL
  )
  if (is.null(page) || !is.data.frame(page) || nrow(page) == 0) {
    return(empty_concept_sections())
  }
  
  body <- first_row_value(page, "markdown_body", "") %||% ""
  chunks <- body %>%
    str_split("\n\n") %>%
    unlist() %>%
    str_replace_all("[*_`>#]", " ") %>%
    str_squish() %>%
    discard(~ !nzchar(.x))
  
  explanation <- chunks[!str_detect(str_to_lower(chunks), "common mistake|formula|notation")][1] %||% ""
  formula_notes <- chunks[str_detect(str_to_lower(chunks), "formula|notation|z-score|standard error|confidence interval|p-value")][1] %||% ""
  common_mistakes <- chunks[str_detect(str_to_lower(chunks), "common mistake|mistake|pitfall|do not")][1] %||% ""
  reminder_bullets <- c(explanation, formula_notes, common_mistakes) %>%
    discard(~ !nzchar(.x)) %>%
    unique()
  
  list(
    explanation = str_sub(explanation, 1, 320),
    formula_notes = str_sub(formula_notes, 1, 220),
    common_mistakes = str_sub(common_mistakes, 1, 220),
    reminder_bullets = reminder_bullets,
    formula = str_sub(formula_notes, 1, 220),
    common_mistake = str_sub(common_mistakes, 1, 220)
  )
}

get_weak_concepts <- function(user_id) {
  concept_catalog <- bind_rows(
    get_question_pool() %>%
      transmute(
        concept_tag = map2_chr(concept_tag, topic_id, ~ normalize_review_concept_tag(.x, .y) %||% NA_character_),
        topic_id = map_chr(topic_id, ~ sanitize_topic_id(.x, require_known = TRUE) %||% "")
      ),
    REVIEW_REMINDER_BANK %>%
      transmute(concept_tag, topic_id)
  ) %>%
    filter(
      !is.na(concept_tag),
      concept_tag != "",
      !is.na(topic_id),
      topic_id != "",
      topic_id != "__generic__"
    ) %>%
    distinct(concept_tag, topic_id) %>%
    left_join(
      select(get_topic_meta(), topic_id, module_id, student_label, module_label, module_order, topic_order),
      by = "topic_id"
    )
  
  if (nrow(concept_catalog) == 0) {
    return(empty_weak_concepts())
  }
  
  attempts <- get_user_attempts(user_id)
  help_queries <- get_user_help_queries(user_id)
  
  attempt_flags <- if (nrow(attempts) == 0) {
    tibble(
      concept_tag = character(),
      recent_attempts = integer(),
      recent_correct = integer(),
      recent_hint_total = integer(),
      wrong_count = integer(),
      heavy_hint_count = integer(),
      recent_accuracy = numeric(),
      mastered = logical()
    )
  } else {
    attempts %>%
      mutate(
        concept_tag = coalesce(
          map2_chr(concept_tag, topic_id, ~ normalize_review_concept_tag(.x, .y) %||% NA_character_),
          map_chr(topic_id, ~ {
            topic_value <- sanitize_topic_id(.x, require_known = TRUE)
            if (is.null(topic_value)) NA_character_ else normalize_review_concept_tag(get_concept_tag_for_topic(topic_value), topic_value)
          })
        )
      ) %>%
      filter(!is.na(concept_tag), concept_tag != "") %>%
      mutate(ts_order = parse_ts_utc(ts)) %>%
      arrange(desc(ts_order), desc(id)) %>%
      group_by(concept_tag) %>%
      mutate(recent_rank = row_number()) %>%
      summarise(
        recent_attempts = sum(recent_rank <= 3),
        recent_correct = sum(correct[recent_rank <= 3] == 1, na.rm = TRUE),
        recent_hint_total = sum(hints_used[recent_rank <= 3], na.rm = TRUE),
        wrong_count = sum(correct == 0, na.rm = TRUE),
        heavy_hint_count = sum(hints_used >= 2, na.rm = TRUE),
        recent_accuracy = mean(correct[recent_rank <= 3], na.rm = TRUE),
        mastered = recent_attempts >= 3 & recent_correct == recent_attempts & recent_hint_total <= 1,
        .groups = "drop"
      )
  }
  
  help_flags <- if (nrow(help_queries) == 0) {
    tibble(concept_tag = character(), help_count = integer())
  } else {
    help_queries %>%
      mutate(
        concept_tag = coalesce(
          map2_chr(concept_tag, topic_id, ~ normalize_review_concept_tag(.x, .y) %||% NA_character_),
          map_chr(topic_id, ~ {
            topic_value <- sanitize_topic_id(.x, require_known = TRUE)
            if (is.null(topic_value)) NA_character_ else normalize_review_concept_tag(get_concept_tag_for_topic(topic_value), topic_value)
          })
        )
      ) %>%
      filter(!is.na(concept_tag), concept_tag != "") %>%
      count(concept_tag, name = "help_count")
  }
  
  weak <- concept_catalog %>%
    left_join(attempt_flags, by = "concept_tag") %>%
    left_join(help_flags, by = "concept_tag") %>%
    mutate(
      recent_attempts = coalesce(recent_attempts, 0L),
      recent_correct = coalesce(recent_correct, 0L),
      recent_hint_total = coalesce(recent_hint_total, 0L),
      wrong_count = coalesce(wrong_count, 0L),
      heavy_hint_count = coalesce(heavy_hint_count, 0L),
      help_count = coalesce(help_count, 0L),
      recent_accuracy = coalesce(recent_accuracy, 1),
      mastered = coalesce(mastered, FALSE),
      weakness_score = wrong_count * 3 + heavy_hint_count * 2 + help_count * 2 + if_else(recent_attempts < 3 & recent_attempts > 0, 1, 0),
      still_weak = weakness_score > 0 & !mastered
    ) %>%
    filter(still_weak) %>%
    filter(map_lgl(topic_id, ~ is_valid_topic_id(.x, require_known = TRUE)))
  
  if (nrow(weak) == 0) {
    return(empty_weak_concepts())
  }
  
  weak %>%
    mutate(
      reason = case_when(
        wrong_count >= 2 ~ "Recent wrong answers show this skill is still shaky",
        heavy_hint_count >= 1 ~ "This skill still depends on hints",
        help_count >= 1 ~ "Recent help requests point to this skill",
        TRUE ~ "This skill needs one more clean stretch to stick"
      ),
      concept_sections = map(topic_id, extract_concept_sections),
      explanation = map_chr(concept_sections, "explanation", .default = ""),
      formula = map_chr(concept_sections, "formula", .default = ""),
      common_mistake = map_chr(concept_sections, "common_mistake", .default = ""),
      next_action = glue("Stay with {get_concept_label(concept_tag)} until you can answer three recent questions with at most one total hint.")
    ) %>%
    select(-concept_sections) %>%
    arrange(desc(weakness_score), module_order, topic_order, concept_tag)
}

get_recent_concept_attempts <- function(user_id, concept_tag, n = 3) {
  attempts <- get_user_attempts(user_id)
  if (nrow(attempts) == 0 || is.null(concept_tag) || !nzchar(concept_tag)) {
    return(tibble())
  }
  
  attempts %>%
    filter(concept_tag == !!concept_tag) %>%
    mutate(ts_order = parse_ts_utc(ts)) %>%
    arrange(desc(ts_order), desc(id)) %>%
    select(-ts_order) %>%
    slice_head(n = n)
}

is_concept_temporarily_mastered <- function(user_id, concept_tag) {
  recent <- get_recent_concept_attempts(user_id, concept_tag, n = 3)
  if (nrow(recent) < 3) {
    return(FALSE)
  }
  
  all(recent$correct == 1, na.rm = TRUE) && sum(recent$hints_used, na.rm = TRUE) <= 1
}

condense_review_text <- function(text, default = NULL, max_chars = 150) {
  cleaned <- text %||% "" %>%
    str_replace_all("[*_`>#]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
  
  if (!nzchar(cleaned)) {
    cleaned <- default %||% ""
  }
  
  str_sub(cleaned, 1, max_chars)
}

extract_fallback_review_bullets <- function(topic_id, max_bullets = 3L) {
  sections <- extract_concept_sections(topic_id)
  fallback_candidates <- c(
    sections$reminder_bullets %||% character(),
    sections$explanation %||% "",
    sections$formula %||% "",
    sections$common_mistake %||% ""
  )
  
  bullets <- fallback_candidates %>%
    str_replace_all("[*_`>#]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish() %>%
    discard(~ !nzchar(.x)) %>%
    discard(~ str_detect(str_to_lower(.x), "^no formula reminder|^no common mistake|^review this concept")) %>%
    map_chr(~ str_sub(.x, 1, 105)) %>%
    unique()
  
  if (length(bullets) == 0) {
    bullets <- c("Slow down on setup, notation, and what the question is asking.")
  }
  
  bullets[seq_len(min(max_bullets, length(bullets)))]
}

build_review_bullets <- function(weak_row) {
  reminder_entry <- get_review_concept_entry(weak_row$concept_tag, weak_row$topic_id)
  reminder_bullets <- if (nrow(reminder_entry) > 0) {
    reminder_entry$reminder_bullets[[1]]
  } else {
    extract_fallback_review_bullets(weak_row$topic_id, max_bullets = 3L)
  }
  
  signal_bullet <- case_when(
    (weak_row$help_count %||% 0L) > 0L ~ "This idea came up in recent help questions, so keep the wording and interpretation straight.",
    (weak_row$heavy_hint_count %||% 0L) > 0L ~ "Recent hints suggest you should slow down and identify the setup before computing.",
    (weak_row$wrong_count %||% 0L) > 0L ~ "Recent misses suggest checking the setup and interpretation before you calculate.",
    TRUE ~ NA_character_
  )
  
  bullets <- c(signal_bullet, reminder_bullets) %>%
    discard(~ is.na(.x) || !nzchar(.x)) %>%
    unique()
  
  bullets[seq_len(min(4L, length(bullets)))]
}

get_instructor_summary <- function() {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  attempts <- DBI::dbGetQuery(con, "
    SELECT module_id,
           topic_id,
           COUNT(*) AS attempts,
           AVG(correct) AS accuracy,
           AVG(hints_used) AS avg_hints
    FROM practice_attempts
    GROUP BY module_id, topic_id
  ")
  
  if (nrow(attempts) == 0) {
    return(
      get_topic_meta() %>%
        transmute(
          module_label,
          student_label,
          attempts = 0L,
          accuracy = NA_real_,
          avg_hints = NA_real_,
          weakness_index = 0
        )
    )
  }
  
  get_topic_meta() %>%
    select(module_id, module_label, module_order, topic_id, student_label, topic_order) %>%
    left_join(attempts, by = c("module_id", "topic_id")) %>%
    mutate(
      attempts = coalesce(as.integer(attempts), 0L),
      accuracy = coalesce(accuracy, 0),
      avg_hints = coalesce(avg_hints, 0),
      weakness_index = round((1 - accuracy) * 100 + avg_hints * 8, 1)
    ) %>%
    arrange(module_order, topic_order)
}

format_percent_label <- function(x, digits = 0) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return("No data yet")
  }
  
  paste0(round(x * 100, digits), "%")
}

get_progress_status <- function(avg_mastery, avg_accuracy, total_attempts) {
  if (is.na(total_attempts) || total_attempts == 0) {
    return("Not started")
  }
  
  if (!is.na(avg_accuracy) && (avg_accuracy < 0.60 || avg_mastery < 55)) {
    return("Needs review")
  }
  
  if (!is.na(avg_accuracy) && avg_accuracy >= 0.85 && avg_mastery >= 80) {
    return("Strong")
  }
  
  "Improving"
}

get_recommended_next_topic <- function(user_id) {
  weak <- get_weak_concepts(user_id)
  if (nrow(weak) > 0) {
    best <- weak %>% slice_head(n = 1)
    return(tibble(
      module_id = best$module_id[[1]],
      topic_id = best$topic_id[[1]],
      module_label = best$module_label[[1]],
      topic_label = best$student_label[[1]],
      reason = best$reason[[1]]
    ))
  }
  
  mastery <- get_mastery(user_id)
  candidate <- mastery %>%
    filter(attempts > 0) %>%
    arrange(mastery, topic_order) %>%
    slice_head(n = 1)
  
  if (nrow(candidate) > 0) {
    return(tibble(
      module_id = candidate$module_id[[1]],
      topic_id = candidate$topic_id[[1]],
      module_label = candidate$module_label[[1]],
      topic_label = candidate$student_label[[1]],
      reason = "This topic has the lowest current mastery among your attempted topics."
    ))
  }
  
  starter <- get_topic_meta() %>% slice_head(n = 1)
  tibble(
    module_id = starter$module_id[[1]],
    topic_id = starter$topic_id[[1]],
    module_label = starter$module_label[[1]],
    topic_label = starter$student_label[[1]],
    reason = "Start here to build your first progress signals."
  )
}

get_overall_progress_summary <- function(user_id) {
  mastery <- get_mastery(user_id)
  attempts <- get_user_attempts(user_id)
  weak <- get_weak_concepts(user_id)
  recommended <- get_recommended_next_topic(user_id)
  
  attempted_mastery <- mastery %>% filter(attempts > 0)
  overall_mastery <- if (nrow(attempted_mastery) == 0) NA_real_ else mean(attempted_mastery$mastery, na.rm = TRUE) / 100
  
  tibble(
    overall_mastery = overall_mastery,
    weak_count = nrow(weak),
    attempts_completed = nrow(attempts),
    recommended_module_id = recommended$module_id[[1]],
    recommended_topic_id = recommended$topic_id[[1]],
    recommended_module = recommended$module_label[[1]],
    recommended_topic = recommended$topic_label[[1]],
    recommended_reason = recommended$reason[[1]]
  )
}

get_recent_improvement_message <- function(user_id) {
  attempts <- get_user_attempts(user_id)
  
  if (nrow(attempts) == 0) {
    return("Start practicing to build your progress profile.")
  }
  
  attempts_with_labels <- attempts %>%
    left_join(select(get_topic_meta(), topic_id, student_label, module_label, topic_order), by = "topic_id")
  
  topic_trends <- attempts_with_labels %>%
    group_by(topic_id, student_label, module_label) %>%
    summarise(
      attempts = n(),
      recent_accuracy = mean(head(correct, 3), na.rm = TRUE),
      overall_accuracy = mean(correct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(improvement = recent_accuracy - overall_accuracy)
  
  improving <- topic_trends %>%
    filter(attempts >= 3, improvement >= 0.20, recent_accuracy >= overall_accuracy) %>%
    arrange(desc(improvement), desc(recent_accuracy))
  
  if (nrow(improving) > 0) {
    return(glue("You are improving in {improving$student_label[[1]]}."))
  }
  
  weak <- get_weak_concepts(user_id)
  if (nrow(weak) > 0) {
    return(glue("{weak$student_label[[1]]} still needs review."))
  }
  
  next_topic <- get_recommended_next_topic(user_id)
  glue("Your next best practice topic is {next_topic$topic_label[[1]]}.")
}

get_recent_activity <- function(user_id, n = 8) {
  attempts <- get_user_attempts(user_id)
  
  if (nrow(attempts) == 0) {
    return(tibble())
  }
  
  attempts %>%
    left_join(select(get_topic_meta(), topic_id, student_label, module_label), by = "topic_id") %>%
    slice_head(n = n) %>%
    mutate(
      result_label = if_else(correct == 1, "Correct", "Incorrect"),
      format_label = case_when(
        question_format == "multiple_choice" ~ "Multiple choice",
        question_format == "fill_in_blank" ~ "Fill in the blank",
        question_format == "choose_best_answer" ~ "Choose best answer",
        question_format == "drag_and_drop" ~ "Checklist",
        TRUE ~ question_format
      )
    )
}

difficulty_rank <- function(difficulty_key) {
  c(easy = 1L, medium = 2L, hard = 3L)[difficulty_key] %||% 2L
}

normalize_module_ids <- function(module_ids = NULL) {
  valid_modules <- MODULES$module_id
  module_ids <- intersect(module_ids %||% character(), valid_modules)
  if (length(module_ids) == 0) {
    valid_modules
  } else {
    module_ids
  }
}

get_question_pool <- function() {
  if (exists("question_pool", envir = .app_cache, inherits = FALSE)) {
    return(.app_cache$question_pool)
  }
  start_time <- Sys.time()
  pool <- bind_rows(
    load_question_bank_cached() %>% mutate(parent_question_id = NA_character_, generated_by = "bank"),
    get_generated_questions()
  ) %>%
    as_tibble() %>%
    filter_unanswerable_practice_questions(source_label = "runtime question pool", warn = FALSE)
  .app_cache$question_pool <- pool
  message(glue("[timer] built question pool in {round(as.numeric(difftime(Sys.time(), start_time, units = 'secs')), 2)} sec"))
  pool
}

get_questions_for_module <- function(module_ids) {
  module_ids <- normalize_module_ids(module_ids)
  get_question_pool() %>%
    filter(module_id %in% module_ids)
}

make_placeholder_question_row <- function(topic_id = NULL, format = "multiple_choice", difficulty = "easy", module_ids = NULL) {
  resolved_module_ids <- normalize_module_ids(module_ids)
  resolved_topic_id_input <- sanitize_topic_id(topic_id, require_known = TRUE)
  resolved_module_id <- if (!is.null(resolved_topic_id_input)) {
    get_module_for_topic(resolved_topic_id_input)
  } else {
    resolved_module_ids[[1]]
  }
  resolved_topic_id <- if (!is.null(resolved_topic_id_input)) {
    resolved_topic_id_input
  } else {
    first_row_value(
      get_topic_meta() %>% filter(module_id == !!resolved_module_id) %>% slice_head(n = 1),
      "topic_id",
      "__generic__"
    )
  }
  
  tibble(
    question_id = glue("placeholder_{resolved_topic_id}_{format}_{difficulty}"),
    topic_id = resolved_topic_id,
    module_id = resolved_module_id,
    concept_tag = if (!is.null(resolved_topic_id_input)) get_concept_tag_for_topic(resolved_topic_id_input) else "careful_setup",
    format = format,
    difficulty = difficulty,
    question_text = "A fresh practice question is not available for this exact setup yet. Use this quick reset item to stay moving.",
    choices = list(normalize_choice_objects(c(
      "Review the hint and explanation, then try the next question",
      "Ignore the setup and guess immediately",
      "Skip notation and conditions every time",
      "Treat all topics as the same method"
    ))),
    correct_choice_id = "A",
    correct_answer = list("Review the hint and explanation, then try the next question"),
    accepted_answers = list("Review the hint and explanation, then try the next question"),
    hint = "When the question pool is thin, slow down and reset the core idea, notation, and condition checks before moving on.",
    explanation = "This placeholder keeps the practice flow alive without crashing. The safest next move is to review the core setup, then continue to a fresh question from the selected modules.",
    parent_question_id = NA_character_,
    generated_by = "placeholder"
  )
}

find_practice_row <- function(topic_id, format, difficulty, module_ids = NULL, exclude_question_ids = character()) {
  resolved_module_ids <- normalize_module_ids(module_ids)
  target_topic_id <- sanitize_topic_id(topic_id)
  pool <- get_question_pool() %>%
    filter(!question_id %in% exclude_question_ids)
  
  if (nrow(pool) == 0) {
    return(make_placeholder_question_row(topic_id, format, difficulty, resolved_module_ids))
  }
  
  target_rank <- difficulty_rank(difficulty)
  module_pool <- pool %>%
    filter(module_id %in% resolved_module_ids)
  
  if (nrow(module_pool) == 0) {
    return(make_placeholder_question_row(topic_id, format, difficulty, resolved_module_ids))
  }
  
  exact_topic_pool <- if (!is.null(target_topic_id)) {
    module_pool %>% filter(topic_id == !!target_topic_id)
  } else {
    module_pool[0, , drop = FALSE]
  }
  
  nearby_topic_pool <- if (nrow(exact_topic_pool) > 0) exact_topic_pool else module_pool
  format_pool <- nearby_topic_pool %>%
    filter(format == !!format)
  
  difficulty_pool <- if (nrow(format_pool) > 0) {
    format_pool %>%
      mutate(difficulty_gap = abs(difficulty_rank(difficulty) - target_rank)) %>%
      arrange(difficulty_gap)
  } else {
    nearby_topic_pool %>%
      mutate(difficulty_gap = abs(difficulty_rank(difficulty) - target_rank)) %>%
      arrange(difficulty_gap)
  }
  
  candidates <- difficulty_pool %>%
    mutate(
      topic_priority = case_when(
        !is.null(target_topic_id) & topic_id == target_topic_id ~ 0L,
        topic_id == "__generic__" ~ 2L,
        TRUE ~ 1L
      ),
      format_priority = if_else(format == !!format, 0L, 1L),
      difficulty_gap = abs(difficulty_rank(difficulty) - target_rank)
    ) %>%
    arrange(topic_priority, format_priority, difficulty_gap)
  
  if (nrow(candidates) == 0) {
    fallback_modules <- pool %>%
      filter(module_id %in% resolved_module_ids) %>%
      mutate(
        topic_priority = if_else(topic_id == "__generic__", 1L, 0L),
        format_priority = if_else(format == !!format, 0L, 1L),
        difficulty_gap = abs(difficulty_rank(difficulty) - target_rank)
      ) %>%
      arrange(topic_priority, format_priority, difficulty_gap)
    if (nrow(fallback_modules) == 0) {
      return(make_placeholder_question_row(topic_id, format, difficulty, resolved_module_ids))
    }
    candidates <- fallback_modules
  }
  
  if (nrow(candidates) == 0) {
    return(make_placeholder_question_row(topic_id, format, difficulty, resolved_module_ids))
  }
  
  best_signature <- candidates %>%
    slice_head(n = 1) %>%
    select(topic_priority, format_priority, difficulty_gap)
  
  candidates %>%
    semi_join(best_signature, by = c("topic_priority", "format_priority", "difficulty_gap")) %>%
    slice_sample(n = 1)
}

make_practice_question_from_row <- function(row, similar_to = NULL) {
  req(nrow(row) > 0)
  topic_id <- row$topic_id[[1]]
  topic_label <- get_topic_label(topic_id)
  module_id <- row$module_id[[1]] %||% get_module_for_topic(topic_id)
  module_label <- get_module_label(module_id)
  format <- row$format[[1]]
  choice_objects <- normalize_choice_objects(row$choices[[1]])
  interaction_type <- normalize_drag_interaction_type(
    format = format,
    question_text = row$question_text[[1]] %||% "",
    interaction_type = row$interaction_type[[1]] %||% NA_character_
  )
  correct_choice_id <- derive_correct_choice_id(
    row$correct_choice_id[[1]] %||% NA_character_,
    row$correct_answer[[1]],
    choice_objects,
    format
  )
  correct_answer <- get_correct_answer_display(
    row$correct_answer[[1]],
    correct_choice_id,
    choice_objects,
    format,
    interaction_type = interaction_type,
    question_text = row$question_text[[1]] %||% ""
  )
  grading_values <- get_grading_values(
    row$correct_answer[[1]],
    correct_choice_id,
    choice_objects,
    format,
    interaction_type = interaction_type,
    question_text = row$question_text[[1]] %||% ""
  )
  
  question_text <- render_template_text(row$question_text[[1]], topic_label, module_label)
  hint <- render_template_text(row$hint[[1]], topic_label, module_label)
  explanation <- render_template_text(row$explanation[[1]], topic_label, module_label)
  row_value <- function(col, default = NA_character_) {
    if (col %in% names(row)) row[[col]][[1]] %||% default else default
  }
  row_vector <- function(col) {
    if (col %in% names(row)) coerce_choice_values(row[[col]][[1]]) else character()
  }
  
  # Keep similar-practice metadata internal. Do not append system guidance
  # to the student-facing question text.
  
  list(
    question_id = row$question_id[[1]],
    parent_question_id = row$parent_question_id[[1]] %||% NA_character_,
    topic_id = topic_id,
    topic_label = topic_label,
    module_id = module_id,
    module_label = module_label,
    concept_tag = row$concept_tag[[1]] %||% get_concept_tag_for_topic(topic_id),
    format = format,
    interaction_type = interaction_type,
    difficulty = row$difficulty[[1]],
    question_text = question_text,
    choices = choice_objects,
    correct_choice_id = correct_choice_id,
    correct_answer = correct_answer,
    grading_values = grading_values,
    accepted_answers = coerce_choice_values(row$accepted_answers[[1]]),
    hint = hint,
    explanation = explanation,
    visual_id = row_value("visual_id", NA_character_),
    visual_ids = unique(c(row_value("visual_id", NA_character_), row_vector("visual_ids"))) %>%
      discard(~ is.na(.x) || !nzchar(.x)),
    visual_position = row_value("visual_position", "above"),
    visual_required = isTRUE(row_value("visual_required", FALSE)),
    tutor_visual_ids = row_vector("tutor_visual_ids"),
    hint_1 = render_template_text(row_value("hint_1", hint), topic_label, module_label),
    hint_2 = render_template_text(row_value("hint_2", NA_character_), topic_label, module_label),
    hint_3 = render_template_text(row_value("hint_3", NA_character_), topic_label, module_label),
    concept_explanation = render_template_text(row_value("concept_explanation", explanation), topic_label, module_label),
    solution_explanation = render_template_text(row_value("solution_explanation", explanation), topic_label, module_label),
    is_fallback = identical(topic_id, "__generic__"),
    generated_by = row_value("generated_by", "bank"),
    question_family = row_value("question_family", row$concept_tag[[1]] %||% topic_id),
    answer_type = row_value("answer_type", format),
    grading_rule = row_value("grading_rule", NA_character_),
    misconception_notes = row_value("misconception_notes", NA_character_),
    visual_template_id = row_value("visual_template_id", NA_character_),
    visual_params = row_value("visual_params", NA_character_),
    generation_method = row_value("generation_method", row_value("generated_by", "bank")),
    evidence_used = row_value("evidence_used", row_value("source_basis", NA_character_)),
    safe_for_demo = row_value("safe_for_demo", TRUE),
    created_at = row_value("created_at", NA_character_),
    reviewed_status = row_value("reviewed_status", row_value("review_status", NA_character_))
  )
}

make_practice_question <- function(topic_id, format, difficulty, module_ids = NULL, exclude_question_ids = character(), similar_to = NULL) {
  row <- find_practice_row(
    topic_id = topic_id,
    format = format,
    difficulty = difficulty,
    module_ids = module_ids,
    exclude_question_ids = exclude_question_ids
  )
  
  if (nrow(row) == 0) {
    return(NULL)
  }
  
  make_practice_question_from_row(row, similar_to = similar_to)
}

find_similar_question_row <- function(question, module_ids = NULL, exclude_question_ids = character()) {
  resolved_module_ids <- normalize_module_ids(module_ids)
  pool <- get_question_pool() %>%
    filter(
      module_id %in% resolved_module_ids,
      question_id != question$question_id,
      !question_id %in% exclude_question_ids
    ) %>%
    mutate(
      concept_priority = if_else(concept_tag == question$concept_tag, 0L, 1L),
      topic_priority = if_else(topic_id == question$topic_id, 0L, 1L),
      format_priority = if_else(format == question$format, 0L, 1L),
      difficulty_gap = abs(difficulty_rank(difficulty) - difficulty_rank(question$difficulty))
    ) %>%
    arrange(concept_priority, topic_priority, format_priority, difficulty_gap)
  
  if (nrow(pool) == 0) {
    return(pool)
  }
  
  best_signature <- pool %>%
    slice_head(n = 1) %>%
    select(concept_priority, topic_priority, format_priority, difficulty_gap)
  
  pool %>%
    semi_join(best_signature, by = c("concept_priority", "topic_priority", "format_priority", "difficulty_gap")) %>%
    slice_sample(n = 1)
}

sanitize_generated_question <- function(question_data, parent_question) {
  topic_id <- question_data$topic_id %||% parent_question$topic_id
  module_id <- question_data$module_id %||% parent_question$module_id %||% get_module_for_topic(topic_id)
  concept_tag <- question_data$concept_tag %||% parent_question$concept_tag %||% get_concept_tag_for_topic(topic_id)
  format <- question_data$format %||% parent_question$format
  difficulty <- question_data$difficulty %||% parent_question$difficulty
  question_text <- str_squish(question_data$question_text %||% "")
  
  if (!nzchar(question_text) || identical(question_text, str_squish(parent_question$question_text %||% ""))) {
    return(NULL)
  }
  
  choice_objects <- normalize_choice_objects(question_data$choices)
  correct_answer <- coerce_choice_values(question_data$correct_answer)
  accepted_answers <- coerce_choice_values(question_data$accepted_answers)
  interaction_type <- normalize_drag_interaction_type(
    format = format,
    question_text = question_text,
    interaction_type = question_data$interaction_type %||% NA_character_
  )
  correct_choice_id <- derive_correct_choice_id(
    question_data$correct_choice_id %||% NA_character_,
    correct_answer,
    choice_objects,
    format
  )
  
  if (format %in% c("multiple_choice", "choose_best_answer") && (length(choice_objects) == 0 || is.na(correct_choice_id) || !nzchar(correct_choice_id))) {
    return(NULL)
  }
  
  if (identical(format, "fill_in_blank") && length(c(accepted_answers, correct_answer)) == 0) {
    return(NULL)
  }
  if (identical(format, "fill_in_blank") && isTRUE(is_unsafe_fill_in_blank_question(
    question_text = question_text,
    correct_answer = correct_answer,
    accepted_answers = if (length(accepted_answers) == 0) correct_answer else accepted_answers,
    choices = choice_objects
  ))) {
    return(NULL)
  }
  
  list(
    question_id = question_data$question_id %||% glue("gq_{topic_id}_{as.integer(as.numeric(Sys.time()))}_{sample(1000:9999, 1)}"),
    parent_question_id = parent_question$question_id,
    module_id = module_id,
    topic_id = topic_id,
    concept_tag = concept_tag,
    format = format,
    difficulty = difficulty,
    question_text = question_text,
    choices = choice_objects,
    interaction_type = interaction_type,
    correct_choice_id = correct_choice_id,
    correct_answer = get_correct_answer_display(
      correct_answer,
      correct_choice_id,
      choice_objects,
      format,
      interaction_type = interaction_type,
      question_text = question_text
    ),
    accepted_answers = if (length(accepted_answers) == 0) correct_answer else accepted_answers,
    hint = question_data$hint %||% parent_question$hint,
    explanation = question_data$explanation %||% parent_question$explanation,
    generated_by = "claude"
  )
}

build_nearby_question <- function(question, module_ids = NULL, exclude_question_ids = character()) {
  make_practice_question(
    topic_id = question$topic_id,
    format = question$format,
    difficulty = question$difficulty,
    module_ids = module_ids,
    exclude_question_ids = unique(c(exclude_question_ids, question$question_id)),
    similar_to = question$question_id
  )
}

get_similar_question_or_fallback <- function(question, module_ids = NULL, exclude_question_ids = character()) {
  stored <- find_similar_question_row(question, module_ids = module_ids, exclude_question_ids = exclude_question_ids)
  if (nrow(stored) > 0) {
    return(list(
      question = make_practice_question_from_row(stored, similar_to = question$question_id),
      message = NULL,
      source = "same_concept"
    ))
  }
  
  nearby <- build_nearby_question(question, module_ids = module_ids, exclude_question_ids = exclude_question_ids)
  if (!is.null(nearby) && !identical(nearby$question_id, question$question_id)) {
    return(list(
      question = nearby,
      message = "No stored same-skill question was available, so the app picked a nearby question from the selected modules.",
      source = "nearby"
    ))
  }
  
  list(
    question = make_practice_question_from_row(
      make_placeholder_question_row(question$topic_id, question$format, question$difficulty, module_ids),
      similar_to = question$question_id
    ),
    message = "No stored similar question is available yet. AI generation can be added later, so the app is using a quick reset item for now.",
    source = "placeholder"
  )
}

generate_similar_question_with_claude <- function(question) {
  cached <- get_generated_questions(topic_id = question$topic_id, parent_question_id = question$question_id)
  if (nrow(cached) > 0) {
    return(list(
      question = make_practice_question_from_row(cached %>% slice_sample(n = 1), similar_to = question$question_id),
      message = NULL,
      source = "cache"
    ))
  }
  
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(api_key)) {
    return(list(
      question = NULL,
      message = "No stored similar question is available yet. AI generation is not connected in this local run.",
      source = "fallback"
    ))
  }
  
  if (!requireNamespace("ellmer", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(
      question = NULL,
      message = "AI-generated follow-up questions need the `ellmer` and `jsonlite` packages in this local run.",
      source = "fallback"
    ))
  }
  
  concept_bits <- extract_concept_sections(question$topic_id)
  system_prompt <- paste(
    "You are generating one adaptive introductory statistics practice question.",
    "Keep the same underlying statistical skill as the parent question, but change the wording, context, and numbers so it is clearly not a duplicate.",
    "Use the same topic, notation style, and answer format.",
    "Do not mention any source materials or professor identities.",
    "Return valid JSON only with keys:",
    "question_text, choices, interaction_type, correct_choice_id, correct_answer, accepted_answers, hint, explanation, format, difficulty, topic_id, module_id, concept_tag.",
    "For multiple-choice formats, choices must be an array of objects with id and text, and correct_choice_id must match one of those ids."
  )
  user_prompt <- paste(
    glue("Topic: {question$topic_label}"),
    glue("Module: {question$module_label}"),
    glue("Format: {question$format}"),
    glue("Difficulty: {question$difficulty}"),
    glue("Parent question: {question$question_text}"),
    glue("Correct answer: {paste(question$correct_answer, collapse = ', ')}"),
    glue("Hint: {question$hint}"),
    glue("Explanation: {question$explanation}"),
    glue("Concept reminder: {concept_bits$explanation}"),
    glue("Formula / notation reminder: {concept_bits$formula}"),
    glue("Common mistake to avoid: {concept_bits$common_mistake}"),
    "Generate one similar but distinct follow-up question now.",
    sep = "\n\n"
  )
  
  generated <- tryCatch(
    {
      chat <- ellmer::chat_anthropic(
        model = "claude-sonnet-4-6",
        api_key = api_key,
        system_prompt = system_prompt
      )
      answer <- chat$chat(user_prompt) %||% ""
      json_text <- answer %>%
        str_replace("^```json\\s*", "") %>%
        str_replace("^```\\s*", "") %>%
        str_replace("\\s*```$", "") %>%
        str_squish()
      parsed <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
      sanitize_generated_question(parsed, question)
    },
    error = function(e) NULL
  )
  
  if (is.null(generated)) {
    return(list(
      question = NULL,
      message = "No stored similar question is available yet, and AI generation could not create one right now.",
      source = "fallback"
    ))
  }
  
  record_generated_question(generated)
  generated_row <- tibble(
    question_id = generated$question_id,
    parent_question_id = generated$parent_question_id,
    module_id = generated$module_id,
    topic_id = generated$topic_id,
    concept_tag = generated$concept_tag,
    format = generated$format,
    interaction_type = generated$interaction_type %||% NA_character_,
    difficulty = generated$difficulty,
    question_text = generated$question_text,
    choices = list(generated$choices),
    correct_choice_id = generated$correct_choice_id %||% NA_character_,
    correct_answer = list(generated$correct_answer),
    accepted_answers = list(generated$accepted_answers),
    hint = generated$hint,
    explanation = generated$explanation,
    generated_by = generated$generated_by
  )
  
  list(
    question = make_practice_question_from_row(generated_row, similar_to = question$question_id),
    message = NULL,
    source = "claude"
  )
}

get_more_like_this_question <- function(question) {
  stored <- find_similar_question_row(question)
  if (nrow(stored) > 0) {
    return(list(
      question = make_practice_question_from_row(stored, similar_to = question$question_id),
      message = NULL,
      source = "bank"
    ))
  }
  
  generate_similar_question_with_claude(question)
}

choose_next_topic_after_mastery <- function(user_id, current_question, module_ids, practice_mode, recent_topics = character()) {
  weak <- get_weak_concepts(user_id) %>%
    filter(
      module_id %in% normalize_module_ids(module_ids),
      concept_tag != current_question$concept_tag
    )
  
  if (nrow(weak) > 0) {
    return(weak$topic_id[[1]])
  }
  
  topic_order <- get_topic_meta() %>%
    filter(module_id %in% normalize_module_ids(module_ids)) %>%
    arrange(module_order, topic_order)
  
  current_idx <- which(topic_order$topic_id == current_question$topic_id)[1]
  if (!is.na(current_idx) && current_idx < nrow(topic_order)) {
    return(topic_order$topic_id[[current_idx + 1]])
  }
  
  next_topic <- choose_practice_topic(
    user_id = user_id,
    module_ids = module_ids,
    practice_mode = practice_mode,
    recent_topics = c(recent_topics, current_question$topic_id)
  )
  
  next_topic %||% current_question$topic_id
}

plan_next_practice_step <- function(user_id, current_question, is_correct, hints_used, module_ids, practice_mode, current_level, exclude_question_ids, recent_topics = character()) {
  if (!is_correct) {
    followup <- get_similar_question_or_fallback(
      current_question,
      module_ids = module_ids,
      exclude_question_ids = exclude_question_ids
    )
    return(list(
      question = followup$question,
      message = "Let's try another one like this.",
      detail = followup$message
    ))
  }
  
  if (hints_used > 0) {
    followup <- get_similar_question_or_fallback(
      current_question,
      module_ids = module_ids,
      exclude_question_ids = exclude_question_ids
    )
    return(list(
      question = followup$question,
      message = "Good, one more to make sure it sticks.",
      detail = followup$message
    ))
  }
  
  if (!is_concept_temporarily_mastered(user_id, current_question$concept_tag)) {
    followup <- get_similar_question_or_fallback(
      current_question,
      module_ids = module_ids,
      exclude_question_ids = exclude_question_ids
    )
    return(list(
      question = followup$question,
      message = "Good, one more to make sure it sticks.",
      detail = followup$message
    ))
  }
  
  next_topic <- choose_next_topic_after_mastery(
    user_id = user_id,
    current_question = current_question,
    module_ids = module_ids,
    practice_mode = practice_mode,
    recent_topics = recent_topics
  )
  next_question <- make_practice_question(
    topic_id = next_topic,
    format = choose_practice_format(next_topic, practice_mode, current_level),
    difficulty = choose_practice_difficulty(current_level, practice_mode),
    module_ids = module_ids,
    exclude_question_ids = exclude_question_ids
  )
  
  list(
    question = next_question,
    message = "Nice, moving to the next skill.",
    detail = if (!identical(next_topic, current_question$topic_id)) glue("Next focus: {get_topic_label(next_topic)}.") else "This module does not have another stored concept ready yet, so the app is keeping you nearby."
  )
}


feedback_text_is_weak <- function(text, correct_answer = NULL) {
  text <- stringr::str_squish(as.character(text %||% ""))
  correct_answer <- stringr::str_squish(paste(as.character(correct_answer %||% ""), collapse = ", "))
  if (!nzchar(text)) {
    return(TRUE)
  }
  if (nchar(text) < 35) {
    return(TRUE)
  }
  if (nzchar(correct_answer) && identical(stringr::str_to_lower(text), stringr::str_to_lower(correct_answer))) {
    return(TRUE)
  }
  if (stringr::str_detect(stringr::str_to_lower(text), "^the correct answer is\b")) {
    return(TRUE)
  }
  FALSE
}

build_conceptual_feedback_explanation <- function(question, result = NULL) {
  correct_answer <- result$correct_answer %||% question$correct_answer %||% "the listed answer"
  correct_text <- stringr::str_squish(paste(as.character(correct_answer), collapse = ", "))
  concept <- stringr::str_to_lower(question$concept_tag %||% "")
  topic <- stringr::str_to_lower(question$topic_id %||% "")
  prompt <- stringr::str_to_lower(question$question_text %||% "")

  tailored <- dplyr::case_when(
    stringr::str_detect(concept, "graph_selection") && stringr::str_detect(stringr::str_to_lower(correct_text), "histogram") ~
      "A histogram is appropriate because the variable is quantitative and the goal is to display its distribution. Bar charts are for category counts, scatterplots compare two quantitative variables, and time plots require measurements over time.",
    stringr::str_detect(concept, "graph_selection") && stringr::str_detect(stringr::str_to_lower(correct_text), "bar") ~
      "A bar chart is appropriate because the variable is categorical or the goal is to compare counts/percents across groups. Histograms are reserved for quantitative measurements grouped into intervals.",
    stringr::str_detect(concept, "graph_selection") && stringr::str_detect(stringr::str_to_lower(correct_text), "scatter") ~
      "A scatterplot is appropriate when the goal is to display the relationship between two quantitative variables. Each point represents one individual measured on both variables.",
    stringr::str_detect(concept, "graph_selection") && stringr::str_detect(stringr::str_to_lower(correct_text), "time") ~
      "A time plot is appropriate when observations are recorded over time. Time belongs on the horizontal axis so that trends, cycles, or unusual changes are visible.",
    stringr::str_detect(concept, "graph_selection") ~
      "The graph choice follows from the variable type and the purpose of the display. Ask whether the data are categories, one quantitative variable, two quantitative variables, or measurements over time.",

    stringr::str_detect(concept, "variable_classification") ~
      "This question is about the type of variable. Categorical variables place individuals into groups or labels, while quantitative variables are numerical measurements where arithmetic such as averages makes sense.",
    stringr::str_detect(concept, "data_context") ~
      "A statistical data set should be read by identifying the individuals, the variables measured on those individuals, and the context. That context determines whether a number is meaningful and which method is appropriate.",
    stringr::str_detect(concept, "statistical_thinking") ~
      "The reasoning is based on statistical thinking: data have context, variation is expected, and conclusions should be tied back to how the data were collected and what the data can actually support.",

    stringr::str_detect(concept, "resistant|skewness|outlier|center_spread|descriptive_summaries|spread_iqr") ~
      "Extreme values affect some summaries more than others. The mean and standard deviation use the actual sizes of all observations, so outliers and skewness can pull them. The median, quartiles, and IQR are more resistant because they depend more on order or position.",

    stringr::str_detect(concept, "z_score|standard_normal") ~
      "A z-score standardizes a value by measuring how many standard deviations it is from the mean. Positive z-scores are above the mean, negative z-scores are below the mean, and z = 0 is exactly at the mean.",
    stringr::str_detect(concept, "normal_probability") ~
      "Normal probability questions use area under a Normal curve. After standardizing to z when needed, the relevant probability is the area to the left, right, or between the marked values.",
    stringr::str_detect(concept, "empirical_rule") ~
      "The 68-95-99.7 rule is a shortcut for Normal distributions: about 68% of observations fall within 1 standard deviation of the mean, 95% within 2, and 99.7% within 3.",

    stringr::str_detect(concept, "probability_rules|complement|independence|conditional|probability_model|random_variable|disjoint") ~
      "Probability questions depend on the event wording. Complements use 1 minus the probability of the opposite event, independence means one event does not change the probability of another, and conditional probability restricts attention to cases where a condition is already true.",
    stringr::str_detect(concept, "binomial") ~
      "A binomial setting has a fixed number of trials, each trial is success/failure, trials are independent, and the success probability stays the same. The random variable counts the number of successes.",

    stringr::str_detect(concept, "association|correlation|lurking|causation") ~
      "For relationships between variables, describe the direction, form, and strength, but be careful about causation. Association alone does not prove cause-and-effect because lurking variables may explain the pattern.",
    stringr::str_detect(concept, "regression|residual|slope") ~
      "Regression questions connect a fitted line to prediction and interpretation. The slope describes the predicted change in the response for a one-unit increase in the explanatory variable, while residuals measure observed minus predicted values.",

    stringr::str_detect(concept, "sampling|bias|generalization|simple_random_sample|law_large_numbers") ~
      "Sampling questions focus on how data were produced. Random sampling helps reduce selection bias and supports generalizing from a sample to a population; biased sampling methods can produce misleading conclusions even with a large sample.",
    stringr::str_detect(concept, "experiment|random_assignment|control_group|placebo|blocking|blinding") ~
      "Experiment questions focus on treatment assignment. Random assignment helps create comparable groups, control groups provide a baseline for comparison, and blinding/placebos help separate treatment effects from expectations or measurement bias.",

    stringr::str_detect(concept, "sampling_distribution|central_limit|clt|standard_error|sampling_variability") ~
      "Sampling-distribution questions are about how a statistic varies from sample to sample. Larger samples usually reduce standard error, and the Central Limit Theorem explains why sample means tend to have an approximately Normal sampling distribution under broad conditions.",

    stringr::str_detect(concept, "ci_|confidence|margin_of_error|conditions_for_intervals|sample_size_ci|t_interval") ~
      "A confidence interval estimates a population parameter using a statistic plus or minus a margin of error. Its interpretation is about the long-run success of the method, not the probability that a fixed computed interval contains the parameter.",

    stringr::str_detect(concept, "p_value|hypothesis|test_statistic|decision_rule|null_alternative|alternative_hypothesis|type_i|testing|significance|conclusion_language|inference_cautions|multiple_testing|practical_significance") || stringr::str_detect(topic, "hypothesis|uses_abuses") ~
      "Hypothesis tests measure how surprising the sample result would be if the null hypothesis were true. A small p-value gives evidence against the null, but failing to reject the null does not prove the null is true. Conclusions should be stated in cautious, contextual language.",

    TRUE ~
      "The correct choice follows from matching the wording of the question to the statistical idea being tested. Focus first on what kind of variable, parameter, graph, or inference procedure the question is describing, then choose the option that matches that role."
  )

  if (nzchar(correct_text)) {
    paste0(tailored, " In this item, **", correct_text, "** is the best choice because it matches what the question is asking you to identify.")
  } else {
    tailored
  }
}

get_feedback_explanation <- function(question, result = NULL) {
  correct_answer <- result$correct_answer %||% question$correct_answer %||% ""
  candidates <- c(
    question$solution_explanation %||% "",
    question$explanation %||% "",
    question$concept_explanation %||% ""
  )
  candidates <- candidates[nzchar(stringr::str_squish(candidates))]
  for (candidate in candidates) {
    if (!feedback_text_is_weak(candidate, correct_answer = correct_answer)) {
      return(candidate)
    }
  }
  build_conceptual_feedback_explanation(question, result)
}

grade_practice_question <- function(question, response) {
  format <- question$format %||% ""
  
  if (format %in% c("multiple_choice", "choose_best_answer")) {
    submitted <- response %||% ""
    submitted_text <- get_choice_text_by_id(submitted, question$choices)
    return(list(
      is_valid = nzchar(submitted),
      is_correct = identical(submitted, question$correct_choice_id %||% ""),
      submitted_answer = submitted_text,
      correct_answer = first_or_default(question$correct_answer, "")
    ))
  }
  
  if (identical(format, "fill_in_blank")) {
    submitted_raw <- response %||% ""
    submitted <- normalize_text_answer(submitted_raw)
    accepted <- question$accepted_answers
    if (length(accepted) == 0) accepted <- question$correct_answer
    accepted <- normalize_text_answer(accepted)
    
    return(list(
      is_valid = nzchar(submitted),
      is_correct = submitted %in% accepted,
      submitted_answer = submitted_raw,
      correct_answer = first_or_default(question$correct_answer, "")
    ))
  }
  
  if (identical(format, "drag_and_drop")) {
    interaction_type <- question$interaction_type %||% infer_drag_interaction_type(question$question_text %||% "")
    
    if (identical(interaction_type, "ordering")) {
      submitted <- coerce_choice_values(response)
      correct <- coerce_choice_values(question$grading_values %||% character())
      has_all_positions <- length(submitted) == length(question$choices)
      has_no_blanks <- has_all_positions && all(nzchar(submitted))
      has_no_duplicates <- !anyDuplicated(submitted)
      
      return(list(
        is_valid = has_no_blanks && has_no_duplicates,
        validation_message = if (!has_all_positions || !has_no_blanks) "Put the steps in order before submitting." else if (!has_no_duplicates) "Use each step once when you put the process in order." else NULL,
        is_correct = identical(submitted, correct),
        submitted_answer = map_choice_values_to_text(submitted, question$choices),
        correct_answer = question$correct_answer
      ))
    }
    
    if (identical(interaction_type, "categorize")) {
      submitted <- response
      if (is.null(submitted)) {
        submitted <- setNames(character(), character())
      }
      submitted <- as.character(submitted)
      submitted <- stats::setNames(submitted, names(response %||% setNames(character(), character())))
      submitted <- submitted[nzchar(names(submitted))]
      correct <- question$grading_values
      has_all_choices <- length(submitted) == length(question$choices) && setequal(names(submitted), get_choice_ids(question$choices))
      has_no_blanks <- has_all_choices && all(nzchar(submitted))
      
      submitted_display <- if (length(submitted) == 0) {
        character()
      } else {
        paste0(map_choice_values_to_text(names(submitted), question$choices), " -> ", unname(submitted))
      }
      
      return(list(
        is_valid = has_no_blanks,
        validation_message = if (!has_no_blanks) "Assign every item to a category before submitting." else NULL,
        is_correct = isTRUE(has_no_blanks) && identical(unname(submitted[names(correct)]), unname(correct)) && identical(names(submitted[names(correct)]), names(correct)),
        submitted_answer = submitted_display,
        correct_answer = question$correct_answer
      ))
    }
    
    submitted <- sort(unique(coerce_choice_values(response)))
    correct <- sort(unique(coerce_choice_values(question$grading_values %||% character())))
    
    return(list(
      is_valid = length(submitted) > 0,
      validation_message = if (length(submitted) == 0) "Select at least one answer before submitting." else NULL,
      is_correct = identical(submitted, correct),
      submitted_answer = map_choice_values_to_text(submitted, question$choices),
      correct_answer = question$correct_answer
    ))
  }
  
  list(
    is_valid = FALSE,
    is_correct = FALSE,
    submitted_answer = response,
    correct_answer = question$correct_answer
  )
}

choose_practice_topic <- function(user_id, module_ids, practice_mode, recent_topics = character()) {
  module_ids <- normalize_module_ids(module_ids)
  module_topics <- get_topic_meta() %>% filter(module_id %in% !!module_ids)
  
  if (nrow(module_topics) == 0) {
    return(NULL)
  }
  
  mastery <- get_mastery(user_id) %>%
    filter(module_id %in% !!module_ids) %>%
    select(topic_id, attempts, mastery, accuracy, avg_hints)
  
  weak <- get_weak_concepts(user_id) %>%
    filter(module_id %in% !!module_ids) %>%
    group_by(topic_id) %>%
    summarise(weakness_score = max(weakness_score, na.rm = TRUE), .groups = "drop")
  
  candidates <- module_topics %>%
    left_join(mastery, by = "topic_id") %>%
    left_join(weak, by = "topic_id") %>%
    mutate(
      attempts = coalesce(attempts, 0L),
      mastery = coalesce(mastery, 0),
      accuracy = coalesce(accuracy, NA_real_),
      avg_hints = coalesce(avg_hints, 0),
      weakness_score = coalesce(weakness_score, 0),
      recent_penalty = if_else(topic_id %in% tail(recent_topics, 2), 0.35, 0)
    )
  
  weighted <- if (identical(practice_mode, "weak_areas")) {
    candidates %>%
      mutate(weight = pmax(0.2, 1 + weakness_score * 1.4 + (1 - coalesce(accuracy, 0.5)) * 2 - recent_penalty))
  } else if (identical(practice_mode, "challenge")) {
    candidates %>%
      mutate(weight = pmax(0.2, 0.5 + (mastery / 100) * 2 + attempts * 0.05 - recent_penalty))
  } else if (identical(practice_mode, "quick_review")) {
    candidates %>%
      mutate(weight = pmax(0.2, 1.4 + if_else(attempts > 0, 0.8, 0.2) + if_else(is.na(accuracy), 0.2, 1 - accuracy) - recent_penalty))
  } else {
    candidates %>%
      mutate(weight = pmax(0.2, 1 + weakness_score + (1 - pmin(mastery / 100, 1)) * 2 - recent_penalty))
  }
  
  weighted %>%
    slice_sample(n = 1, weight_by = weight) %>%
    pull(topic_id)
}

get_practice_pool <- function(active_module_ids = NULL) {
  module_ids <- get_selected_modules(active_module_ids)
  if (length(module_ids) == 0) {
    return(get_question_pool()[0, , drop = FALSE])
  }
  get_question_pool() %>%
    filter(module_id %in% module_ids)
}

choose_next_practice_module <- function(active_module_ids, user_id = NULL, weak_concepts = NULL) {
  module_ids <- get_selected_modules(active_module_ids)
  if (length(module_ids) == 0) {
    return(NULL)
  }

  weak <- weak_concepts %||% get_weak_concepts(user_id %||% "")
  if (is.data.frame(weak) && nrow(weak) > 0) {
    weak_match <- weak %>%
      filter(module_id %in% module_ids) %>%
      arrange(desc(weakness_score)) %>%
      slice_head(n = 1)
    if (nrow(weak_match) > 0) {
      return(weak_match$module_id[[1]])
    }
  }

  sample(module_ids, 1)
}

generate_practice_question <- function(active_module_ids = NULL,
                                       current_module_id = NULL,
                                       user_id = NULL,
                                       practice_mode = "recommended",
                                       current_level = 1L,
                                       exclude_question_ids = character(),
                                       recent_topics = character()) {
  module_ids <- get_selected_modules(active_module_ids)
  if (length(module_ids) == 0) {
    return(NULL)
  }
  current_module_id <- current_module_id %||% choose_next_practice_module(module_ids, user_id = user_id)
  if (!is.null(current_module_id) && current_module_id %in% module_ids) {
    topic_modules <- current_module_id
  } else {
    topic_modules <- module_ids
  }
  topic_id <- choose_practice_topic(
    user_id = user_id,
    module_ids = topic_modules,
    practice_mode = practice_mode,
    recent_topics = recent_topics
  )
  if (is.null(topic_id) || !nzchar(topic_id)) {
    return(NULL)
  }
  make_practice_question(
    topic_id = topic_id,
    format = choose_practice_format(topic_id, practice_mode, current_level),
    difficulty = choose_practice_difficulty(current_level, practice_mode),
    module_ids = module_ids,
    exclude_question_ids = exclude_question_ids
  )
}

get_initial_practice_level <- function(user_id, topic_id, practice_mode) {
  if (identical(practice_mode, "challenge")) {
    return(2L)
  }
  
  topic_id <- sanitize_topic_id(topic_id)
  if (is.null(topic_id)) {
    return(1L)
  }
  
  mastery_row <- get_mastery(user_id) %>% filter(topic_id == !!topic_id) %>% slice_head(n = 1)
  
  if (nrow(mastery_row) == 0 || (mastery_row$attempts[[1]] %||% 0L) == 0L) {
    return(1L)
  }
  
  mastery_value <- mastery_row$mastery[[1]] %||% 0
  
  case_when(
    mastery_value >= 80 ~ 3L,
    mastery_value >= 55 ~ 2L,
    TRUE ~ 1L
  )
}

choose_practice_difficulty <- function(current_level, practice_mode) {
  level <- max(1L, min(3L, as.integer(round(current_level %||% 1L))))
  
  if (identical(practice_mode, "quick_review")) {
    if (level <= 1L) return(sample(c("easy", "medium"), 1, prob = c(0.8, 0.2)))
    if (level == 2L) return(sample(c("easy", "medium"), 1, prob = c(0.4, 0.6)))
    return(sample(c("easy", "medium"), 1, prob = c(0.2, 0.8)))
  }
  
  if (identical(practice_mode, "challenge")) {
    return(sample(c("medium", "hard"), 1, prob = if (level >= 3L) c(0.25, 0.75) else c(0.65, 0.35)))
  }
  
  c("easy", "medium", "hard")[level]
}

choose_practice_format <- function(topic_id, practice_mode, current_level) {
  topic_id <- sanitize_topic_id(topic_id)
  topic_specific <- if (!is.null(topic_id)) {
    question_bank %>%
      filter(topic_id == !!topic_id) %>%
      distinct(format) %>%
      pull(format)
  } else {
    character()
  }
  
  candidates <- unique(c(topic_specific, c("multiple_choice", "fill_in_blank", "choose_best_answer", "drag_and_drop")))
  weights <- setNames(rep(1, length(candidates)), candidates)
  
  if (identical(practice_mode, "quick_review")) {
    if ("multiple_choice" %in% names(weights)) weights["multiple_choice"] <- 4
    if ("fill_in_blank" %in% names(weights)) weights["fill_in_blank"] <- 3
    if ("choose_best_answer" %in% names(weights)) weights["choose_best_answer"] <- 1.5
    if ("drag_and_drop" %in% names(weights)) weights["drag_and_drop"] <- 0.5
  } else if (identical(practice_mode, "challenge")) {
    if ("multiple_choice" %in% names(weights)) weights["multiple_choice"] <- 1
    if ("fill_in_blank" %in% names(weights)) weights["fill_in_blank"] <- 1.2
    if ("choose_best_answer" %in% names(weights)) weights["choose_best_answer"] <- 3
    if ("drag_and_drop" %in% names(weights)) weights["drag_and_drop"] <- 2
  } else if (current_level >= 3L) {
    if ("multiple_choice" %in% names(weights)) weights["multiple_choice"] <- 1
    if ("fill_in_blank" %in% names(weights)) weights["fill_in_blank"] <- 1.4
    if ("choose_best_answer" %in% names(weights)) weights["choose_best_answer"] <- 2.4
    if ("drag_and_drop" %in% names(weights)) weights["drag_and_drop"] <- 1.5
  } else if (current_level <= 1L) {
    if ("multiple_choice" %in% names(weights)) weights["multiple_choice"] <- 3.5
    if ("fill_in_blank" %in% names(weights)) weights["fill_in_blank"] <- 2.5
    if ("choose_best_answer" %in% names(weights)) weights["choose_best_answer"] <- 1.5
    if ("drag_and_drop" %in% names(weights)) weights["drag_and_drop"] <- 1
  }
  
  sample(names(weights), size = 1, prob = weights)
}

get_practice_format_label <- function(format_key) {
  c(
    multiple_choice = "Multiple choice",
    fill_in_blank = "Fill in the blank",
    choose_best_answer = "Choose the best answer",
    drag_and_drop = "Sort / select"
  )[format_key] %||% format_key
}

get_difficulty_label <- function(difficulty_key) {
  c(
    easy = "Easy",
    medium = "Medium",
    hard = "Hard"
  )[difficulty_key] %||% difficulty_key
}

get_practice_mode_label <- function(mode_key) {
  c(
    recommended = "Recommended practice",
    weak_areas = "Weak areas",
    quick_review = "Quick review",
    challenge = "Challenge mode"
  )[mode_key] %||% mode_key
}

make_help_response <- function(topic_id, query_text) {
  topic_id <- sanitize_topic_id(topic_id, require_known = TRUE)
  if (is.null(topic_id)) {
    return(build_general_help_fallback(query_text))
  }
  page <- if (!is.null(topic_id)) get_concept_page(topic_id) else NULL
  
  if (is.null(page)) {
    return(build_help_fallback_response(topic_id, query_text))
  }

  build_help_fallback_response(topic_id, query_text)
}

get_current_weak_topic <- function(user_id, module_id = NULL, fallback_to_default = FALSE) {
  if (is.null(normalize_scalar_string(user_id))) {
    return(if (isTRUE(fallback_to_default)) get_default_help_topic(module_id) else NULL)
  }
  
  weak <- get_weak_concepts(user_id)
  module_id <- normalize_scalar_string(module_id)
  if (!is.null(module_id)) {
    weak <- weak %>% filter(module_id == !!module_id)
  }
  
  weak_topic_id <- first_row_value(weak %>% slice_head(n = 1), "topic_id", NULL)
  if (is_valid_topic_id(weak_topic_id, require_known = TRUE)) {
    return(weak_topic_id)
  }
  
  if (isTRUE(fallback_to_default)) get_default_help_topic(module_id) else NULL
}

get_help_topic_choices <- function(module_id = NULL, user_id = NULL) {
  topics <- get_topics_for_help_module(module_id)
  
  c(
    "Let the app decide" = "__auto__",
    stats::setNames(topics$topic_id, topics$student_label)
  )
}

resolve_help_topic <- function(user_id, module_id, topic_choice) {
  topic_choice <- sanitize_topic_id(topic_choice, allow_special = TRUE)
  if (!is.null(topic_choice) && !topic_choice %in% c("__auto__", "__weak__")) {
    return(sanitize_topic_id(topic_choice, require_known = TRUE))
  }
  
  NULL
}

detect_help_topic_from_query <- function(query_text, selected_topic_id = NULL) {
  query <- str_to_lower(query_text %||% "")
  selected_topic_id <- sanitize_topic_id(selected_topic_id, require_known = TRUE)
  
  if (!nzchar(query)) {
    return(selected_topic_id %||% NULL)
  }
  
  if (str_detect(query, "one proportion test|one-proportion test|\\bp0\\b|p hat test|p-hat test")) {
    return("ht_prop")
  }
  if (str_detect(query, "t test|t-test|mean test|\\bmu0\\b|mu 0|hypothesis test for a mean")) {
    return("ht_mean")
  }
  if (str_detect(query, "p value|p-value|null hypothesis|alternative hypothesis|fail to reject|reject the null|\\breject\\b|significance level|level of significance|\\bsignificance\\b")) {
    return("ht_foundations")
  }
  if (str_detect(query, "confidence interval|margin of error")) {
    if (str_detect(query, "proportion|p-hat|phat|one proportion|one-proportion|success|failure")) {
      return("ci_prop")
    }
    if (str_detect(query, "mean|mu|x bar|xbar|t interval|sample mean")) {
      return("ci_mean")
    }
    if (!is.null(selected_topic_id) && selected_topic_id %in% c("ci_prop", "ci_mean")) {
      return(selected_topic_id)
    }
  }
  if (str_detect(query, "binomial|\\bbins\\b|at least|at most|binompdf|binomcdf")) {
    return("binomial_dist")
  }
  if (str_detect(query, "z score|z-score|standard normal|normal curve")) {
    return("normal_dist")
  }
  if (str_detect(query, "slope|residual|correlation|regression line|least squares")) {
    return("relationships_regression")
  }
  if (str_detect(query, "variable type|categorical|quantitative|qualitative|nominal|ordinal|discrete|continuous")) {
    return("data_graphs")
  }
  if (str_detect(query, "histogram|boxplot|stemplot|five-number summary|iqr|interquartile")) {
    return("descriptive_stats")
  }
  if (str_detect(query, "graph|bar chart|pie chart|scatterplot")) {
    return("data_graphs")
  }
  
  selected_topic_id %||% NULL
}

detect_help_concept_tag <- function(query_text, topic_id = NULL) {
  query <- str_to_lower(query_text %||% "")
  topic_id <- sanitize_topic_id(topic_id, require_known = TRUE)
  
  if (!nzchar(query)) {
    return(normalize_review_concept_tag(NULL, topic_id))
  }
  
  if (str_detect(query, "fail to reject|accept the null|accept null")) {
    return("fail_to_reject")
  }
  if (str_detect(query, "p value|p-value|significance")) {
    return("p_value_interpretation")
  }
  if (str_detect(query, "at least|at most|more than|less than|binompdf|binomcdf")) {
    return("binomial_at_least_at_most")
  }
  if (str_detect(query, "\\bbins\\b|binary outcomes|independent trials|fixed number of trials|same probability")) {
    return("binomial_conditions")
  }
  if (str_detect(query, "choose\\(|exactly|binomial formula|n choose x")) {
    return("binomial_probability_formula")
  }
  if (!is.null(topic_id) && topic_id %in% c("ci_prop", "ci_mean") && str_detect(query, "confidence interval|margin of error|interpret")) {
    return("ci_interpretation")
  }
  if (identical(topic_id, "ht_prop") && str_detect(query, "standard error|p0|p-hat|p hat|phat")) {
    return("one_proportion_standard_error")
  }
  if (identical(topic_id, "relationships_regression") && str_detect(query, "slope")) {
    return("slope_interpretation")
  }
  if (identical(topic_id, "data_graphs") && str_detect(query, "variable type|categorical|quantitative|qualitative|nominal|ordinal|discrete|continuous")) {
    return("variable_type_identification")
  }
  if (!is.null(topic_id) && topic_id %in% c("data_graphs", "descriptive_stats") && str_detect(query, "graph|histogram|boxplot|bar chart|pie chart|stemplot|scatterplot")) {
    return("graph_selection")
  }
  
  normalize_review_concept_tag(NULL, topic_id)
}

route_help_topic <- function(user_id, module_id, topic_choice, query_text) {
  manual_topic <- resolve_help_topic(user_id, module_id, topic_choice)
  keyword_topic <- if (is.null(manual_topic)) detect_help_topic_from_query(query_text) else manual_topic
  weak_topic <- get_current_weak_topic(user_id, fallback_to_default = FALSE)
  default_topic <- sanitize_topic_id(
    if (is_valid_topic_id("ht_foundations", require_known = TRUE)) "ht_foundations" else get_default_help_topic(module_id),
    require_known = TRUE
  )
  
  if (is_valid_topic_id(manual_topic, require_known = TRUE)) {
    route_source <- "manual"
    topic_id <- manual_topic
  } else if (is_valid_topic_id(keyword_topic, require_known = TRUE)) {
    route_source <- "keyword"
    topic_id <- keyword_topic
  } else if (is_valid_topic_id(weak_topic, require_known = TRUE)) {
    route_source <- "weak"
    topic_id <- weak_topic
  } else if (is_valid_topic_id(default_topic, require_known = TRUE)) {
    route_source <- "default"
    topic_id <- default_topic
  } else {
    route_source <- "general"
    topic_id <- NULL
  }
  
  topic_label <- if (!is.null(topic_id)) get_topic_label(topic_id) else "General course help"
  module_id <- if (!is.null(topic_id)) get_module_for_topic(topic_id) else NA_character_
  concept_tag <- detect_help_concept_tag(query_text, topic_id)
  route_note <- if (identical(route_source, "keyword")) {
    glue("Matched topic: {topic_label}")
  } else if (identical(route_source, "manual")) {
    glue("Using your chosen topic: {topic_label}")
  } else if (identical(route_source, "weak")) {
    glue("No exact keyword match, so I used your current weak topic: {topic_label}")
  } else if (identical(route_source, "default")) {
    glue("No exact topic match yet, so I started with a safe fallback topic: {topic_label}")
  } else {
    "No clear topic match yet. I will give a short general guide and ask a clarifying follow-up."
  }
  response_intro <- if (!is.null(topic_id)) {
    glue("This question fits best under {topic_label}, so I'll answer it from that angle.")
  } else {
    NA_character_
  }
  
  list(
    topic_id = topic_id,
    topic_label = topic_label,
    module_id = module_id,
    concept_tag = concept_tag,
    route_source = route_source,
    route_note = route_note,
    response_intro = response_intro
  )
}

detect_direct_answer_request <- function(query_text) {
  str_detect(
    str_to_lower(query_text %||% ""),
    "just give|give me the answer|final answer|solve it for me|homework answer|quiz answer|test answer|exam answer|do this problem for me"
  )
}

build_help_context <- function(topic_id, query_text, max_chunks = 4) {
  topic_id <- sanitize_topic_id(topic_id, require_known = TRUE)
  if (is.null(topic_id)) {
    return(list(
      status = "general_fallback",
      topic_label = "General course help",
      context_text = "No topic-specific course guide was selected.",
      fallback_text = build_general_help_fallback(query_text)
    ))
  }
  
  page <- get_concept_page(topic_id)
  if (is.null(page)) {
    return(list(
      status = "missing_page",
      topic_label = get_topic_label(topic_id),
      context_text = "No course guide is available for this topic yet.",
      fallback_text = normalize_help_response_object(list(
        routed_topic_label = get_topic_label(topic_id),
        direct_answer = glue("I do not have a full course guide loaded for {get_topic_label(topic_id)} yet, so I can only give a short grounded guide."),
        remember_bullets = c(
          "Start by naming the target quantity or parameter.",
          "Match the notation to the method before calculating.",
          "Check the conditions before you use a formula."
        ),
        analogy = "This is like sketching the map before all the street labels are filled in.",
        common_mistake = "Do not guess the method just because the notation looks familiar.",
        next_step = "Use a nearby practice question to strengthen the review signal for this topic."
      ), routed_topic_label = get_topic_label(topic_id))
    ))
  }
  
  body <- page$markdown_body %||% ""
  if (!nzchar(body)) {
    return(list(
      status = "empty_page",
      topic_label = get_topic_label(topic_id),
      context_text = "The course guide for this topic is currently empty.",
      fallback_text = normalize_help_response_object(list(
        routed_topic_label = get_topic_label(topic_id),
        direct_answer = glue("The course guide for {get_topic_label(topic_id)} is sparse right now, so I am giving a short grounded summary."),
        remember_bullets = c(
          "Start by identifying what the question is asking for.",
          "Keep the notation consistent with the method.",
          "Check the setup before you calculate."
        ),
        analogy = "This is like using a short study card instead of a full chapter summary.",
        common_mistake = "Do not rush into algebra before you identify the statistical target.",
        next_step = "Practice one nearby question so the next explanation can be more specific."
      ), routed_topic_label = get_topic_label(topic_id))
    ))
  }
  
  chunks <- body %>%
    str_split("\n\n") %>%
    unlist() %>%
    str_squish() %>%
    discard(~ !nzchar(.x))
  
  if (length(chunks) == 0) {
    fallback_text <- extract_concept_sections(topic_id)$explanation
    if (!nzchar(str_squish(fallback_text %||% ""))) {
      fallback_text <- make_help_response(topic_id, query_text)
    } else {
      fallback_text <- build_help_fallback_response(topic_id, query_text)
    }
    
    return(list(
      status = "limited_context",
      topic_label = get_topic_label(topic_id),
      context_text = body,
      fallback_text = fallback_text
    ))
  }
  
  query_tokens <- query_text %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_split("\\s+") %>%
    unlist() %>%
    unique()
  query_tokens <- query_tokens[nchar(query_tokens) >= 4]
  
  if (length(query_tokens) > 0) {
    match_pattern <- paste(query_tokens, collapse = "|")
    matched_chunks <- chunks[str_detect(str_to_lower(chunks), regex(match_pattern, ignore_case = TRUE))]
  } else {
    matched_chunks <- character()
  }
  
  available_chunks <- unique(c(matched_chunks, chunks))
  selected_chunks <- available_chunks[seq_len(min(max_chunks, length(available_chunks)))]
  sections <- extract_concept_sections(topic_id)
  if (length(selected_chunks) == 0) {
    selected_chunks <- str_sub(body, 1, 500)
  }
  fallback_text <- paste(
    sections$explanation,
    glue("Formula / notation reminder: {sections$formula}"),
    glue("Common mistake to avoid: {sections$common_mistake}"),
    sep = "\n\n"
  )
  if (!nzchar(str_squish(fallback_text))) {
    fallback_text <- make_help_response(topic_id, query_text)
  } else {
    fallback_text <- build_help_fallback_response(topic_id, query_text)
  }
  
  list(
    status = "ok",
    topic_label = get_topic_label(topic_id),
    context_text = paste(
      glue("Topic: {get_topic_label(topic_id)}"),
      glue("Core explanation: {sections$explanation}"),
      glue("Formula / notation reminder: {sections$formula}"),
      glue("Common mistake: {sections$common_mistake}"),
      "Relevant course guide excerpts:",
      paste0("- ", selected_chunks, collapse = "\n"),
      sep = "\n\n"
    ),
    fallback_text = fallback_text
  )
}

call_claude_help <- function(query, topic_id, concept_context) {
  topic_id <- sanitize_topic_id(topic_id, require_known = TRUE)
  concept_tag <- detect_help_concept_tag(query, topic_id)
  if (is.null(topic_id)) {
    return(list(
      response_object = build_general_help_fallback(query),
      source = "fallback",
      error_message = NA_character_
    ))
  }
  
  if (!is.list(concept_context)) {
    concept_context <- list(
      status = "limited_context",
      topic_label = get_topic_label(topic_id),
      context_text = as.character(concept_context %||% ""),
      fallback_text = build_help_fallback_response(topic_id, query)
    )
  }
  fallback_text <- concept_context$fallback_text
  if (!is.list(fallback_text)) {
    fallback_text <- build_help_fallback_response(topic_id, query)
  }
  fallback_text <- normalize_help_response_object(fallback_text, routed_topic_label = get_topic_label(topic_id))
  
  fallback_response <- if (detect_direct_answer_request(query)) {
    normalize_help_response_object(list(
      routed_topic_label = get_topic_label(topic_id),
      direct_answer = "I can help you reason through it, but I should not give a direct homework or test answer.",
      remember_bullets = c(
        "I can explain the setup, notation, and decision steps.",
        "Tell me which part feels unclear and I can narrow the explanation.",
        "Use the next practice question to apply the idea yourself."
      ),
      analogy = "Think of this like a coach guiding the setup instead of taking the shot for you.",
      common_mistake = "Do not skip the reasoning and ask only for the final graded answer.",
      next_step = "Point to the step that is stuck and I will walk through that part."
    ), routed_topic_label = get_topic_label(topic_id))
  } else {
    fallback_text
  }
  
  if (!identical(concept_context$status, "ok")) {
    return(list(
      response_object = fallback_text,
      source = "fallback",
      error_message = if (identical(concept_context$status, "missing_page")) "Selected topic has no course guide." else NA_character_
    ))
  }
  
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(api_key)) {
    return(list(
      response_object = fallback_response,
      source = "fallback",
      error_message = "ANTHROPIC_API_KEY is missing."
    ))
  }
  
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    return(list(
      response_object = fallback_response,
      source = "fallback",
      error_message = "The ellmer package is not installed."
    ))
  }
  
  topic_label <- if (!is.null(topic_id)) get_topic_label(topic_id) else "General course help"
  p_value_guidance <- if (is_p_value_help_query(query, topic_id, concept_tag)) {
    paste(
      "Because this is a p-value style question, make sure the answer explicitly says:",
      "- the p-value is calculated assuming H0 is true",
      "- smaller p-values mean the observed result would be unusual under H0",
      "- the p-value is not the probability that H0 is true",
      "- compare the p-value to alpha",
      "- fail to reject does not mean accept H0",
      sep = "\n"
    )
  } else {
    NULL
  }
  system_prompt <- paste(
    "You are a helpful introductory statistics study assistant.",
    "Use current-course notation and terminology.",
    "Ground your explanation in the provided concept-page context only.",
    "Do not mention professors, source files, or where the material came from.",
    "Prioritize conceptual understanding, setup, notation, common mistakes, and next steps.",
    "If the student asks for a direct homework, quiz, or test answer, refuse to provide the final answer and instead give guidance, hints, and reasoning steps.",
    "Keep the response student-friendly, concise, and supportive.",
    "Target roughly 120 to 200 words.",
    "Return valid JSON only with exactly these top-level fields:",
    "routed_topic_label, direct_answer, remember_bullets, analogy, common_mistake, next_step.",
    "remember_bullets must be a JSON array with 3 to 5 short bullet strings.",
    "direct_answer must be 1 or 2 short sentences.",
    "analogy must be 1 short analogy.",
    "common_mistake must be 1 short warning.",
    "next_step must be 1 short sentence or an empty string.",
    "Do not return markdown, code fences, commentary, or any extra keys.",
    sep = " "
  )
  
  user_prompt <- paste(
    glue("Selected topic: {topic_label}"),
    glue("Student question: {query}"),
    "Concept-page context:",
    concept_context$context_text %||% compact_help_text(fallback_response$direct_answer, ""),
    if (!is.null(p_value_guidance)) p_value_guidance else NULL,
    "Write a helpful explanation that teaches the idea without simply giving away a final graded answer.",
    sep = "\n\n"
  )
  
  help_result <- tryCatch(
    {
      chat <- ellmer::chat_anthropic(
        model = "claude-sonnet-4-6",
        api_key = api_key,
        system_prompt = system_prompt
      )
      answer <- chat$chat(user_prompt)
      parsed_answer <- parse_help_response_json(answer %||% "")
      if (is.null(parsed_answer)) {
        list(
          response_object = fallback_response,
          source = "fallback",
          error_message = "Claude returned invalid JSON."
        )
      } else {
        list(
          response_object = normalize_help_response_object(parsed_answer, routed_topic_label = topic_label),
          source = "claude",
          error_message = NA_character_
        )
      }
    },
    error = function(e) {
      list(
        response_object = fallback_response,
        source = "fallback",
        error_message = conditionMessage(e)
      )
    }
  )
  
  help_result
}

student_ui <- function(id) {
  ns <- NS(id)
  
  page_navbar(
    id = ns("student_nav"),
    title = APP_TITLE,
    selected = "practice",
    bg = "#1f5fa8",
    nav_panel(
      "Practice",
      value = "practice",
      br(),
      uiOutput(ns("practice_panel"))
    ),
    nav_panel(
      "My Review Sheet",
      value = "review_sheet",
      br(),
      div(class = "practice-main-shell", uiOutput(ns("review_sheet_ui")))
    ),
    nav_panel(
      "Progress",
      value = "progress",
      br(),
      div(class = "practice-main-shell", uiOutput(ns("progress_dashboard_ui")))
    )
  )
}

instructor_ui <- function(id) {
  ns <- NS(id)
  
  page_navbar(
    title = glue("{APP_TITLE} | Instructor"),
    nav_panel(
      "Analytics",
      br(),
      card(
        card_header("Aggregate weak topics and modules"),
        tableOutput(ns("analytics_table"))
      )
    ),
    nav_panel(
      "Module summary",
      br(),
      card(
        card_header("Aggregate module progress"),
        tableOutput(ns("module_summary_table"))
      )
    ),
    nav_panel(
      "Knowledge Base",
      br(),
      card(
        card_header("Concept pages loaded"),
        tableOutput(ns("concept_pages_table"))
      )
    )
  )
}

student_server <- function(id, user_info) {
  moduleServer(id, function(input, output, session) {
    practice_state <- reactiveValues(
      active = FALSE,
      selected_modules = character(),
      topic_id = NULL,
      topic_label = NULL,
      practice_mode = "recommended",
      session_question_number = 0L,
      current_level = 1L,
      streak_correct = 0,
      streak_wrong = 0L,
      hints_used_current_question = 0L,
      current_question = NULL,
      submission_result = NULL,
      hint_visible = FALSE,
      concept_reminder = NULL,
      question_history = character(),
      seen_question_ids = character(),
      recent_topics = character(),
      queued_next_question = NULL,
      next_step_message = NULL,
      next_step_detail = NULL,
      last_question_selection_debug = NULL,
      source_mode = "general",
      professor_id = NULL,
      practice_help = NULL,
      practice_help_loading = FALSE,
      practice_help_error = NULL,
      practice_help_debug = NULL,
      deterministic_visual_show = FALSE,
      deterministic_visual_type = NULL,
      deterministic_visual_caption = NULL,
      evidence_cache = NULL,
      hint_count_current_question = 0L
    )
    attempts_refresh <- reactiveVal(0L)
    help_refresh <- reactiveVal(0L)
    help_state <- reactiveValues(is_loading = FALSE, status_text = NULL, latest_error = NULL, route_note = NULL, latest_exchange = NULL, latest_debug = NULL)
    tutor_state <- reactiveValues(
      conversation_history = list(),
      active_module_id = NULL,
      active_module_ids = character(),
      current_module_id = NULL,
      current_question_id = NULL,
      current_question_text = NULL,
      current_answer_choices = NULL,
      student_answer = NULL,
      answer_submitted = FALSE,
      attempt_count = 0L,
      expected_concept_tag = NULL,
      weak_concept_tag = NULL,
      last_retrieved_evidence = NULL,
      last_retrieval_trace = NULL,
      last_tutor_answer = NULL,
      last_help_mode = NULL,
      last_visual_refs = NULL
    )

    reset_tutor_state <- function(context = NULL) {
      context <- context %||% list()
      tutor_state$conversation_history <- list()
      tutor_state$active_module_id <- context$active_module_id %||% NULL
      tutor_state$active_module_ids <- get_selected_modules(context$active_module_ids %||% character())
      tutor_state$current_module_id <- context$current_module_id %||% context$active_module_id %||% NULL
      tutor_state$current_question_id <- context$current_question_id %||% NULL
      tutor_state$current_question_text <- context$question_text %||% NULL
      tutor_state$current_answer_choices <- context$answer_choices %||% NULL
      tutor_state$student_answer <- context$student_answer %||% NULL
      tutor_state$answer_submitted <- isTRUE(context$answer_submitted)
      tutor_state$attempt_count <- context$attempt_count %||% 0L
      tutor_state$expected_concept_tag <- context$expected_concept_tag %||% NULL
      tutor_state$weak_concept_tag <- context$weak_concept_tag %||% NULL
      tutor_state$last_retrieved_evidence <- NULL
      tutor_state$last_retrieval_trace <- NULL
      tutor_state$last_tutor_answer <- NULL
      tutor_state$last_help_mode <- NULL
      tutor_state$last_visual_refs <- NULL
    }

    output$practice_module_buttons <- renderUI({
      render_module_button_grid(practice_state$selected_modules, ns = session$ns)
    })

    lapply(MODULES$module_id, function(module_id_value) {
      local({
        mid <- module_id_value
        observeEvent(input[[module_button_id(mid)]], {
          current <- get_selected_modules(practice_state$selected_modules)
          updated <- if (mid %in% current) setdiff(current, mid) else unique(c(current, mid))
          practice_state$selected_modules <- updated
          if (isTRUE(practice_state$active)) {
            reset_practice_session()
            practice_state$selected_modules <- updated
            showNotification("Module selection changed. Start practice again when you are ready.", type = "message")
          }
        }, ignoreInit = TRUE)
      })
    })

    practice_timer <- function(start_time) {
      round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 3)
    }

    clear_deterministic_tutor_visual <- function() {
      practice_state$deterministic_visual_show <- FALSE
      practice_state$deterministic_visual_type <- NULL
      practice_state$deterministic_visual_caption <- NULL
      invisible(TRUE)
    }

    build_question_retrieval_query <- function(question) {
      context_terms <- paste(
        question$hint_1 %||% "",
        question$hint_2 %||% "",
        question$hint %||% "",
        question$concept_explanation %||% "",
        question$solution_explanation %||% "",
        question$explanation %||% "",
        sep = " "
      ) %>%
        clean_student_facing_source_language()
      anchor_terms <- if (isTRUE(is_resistant_center_question(question$question_text, question$concept_tag))) {
        "resistant measures median mean outliers skewed right-skewed measure of center extreme values nonresistant"
      } else {
        ""
      }
      paste(
        glue("Practice question: {question$question_text %||% ''}"),
        glue("Expected concept: {question$concept_tag %||% question$topic_id %||% ''}"),
        glue("Topic: {question$topic_label %||% ''}"),
        glue("Module: {question$module_label %||% ''}"),
        glue("Current question explanation anchors: {context_terms}"),
        glue("Concept anchor terms: {anchor_terms}"),
        sep = "\n"
      ) %>% str_squish()
    }

    set_cached_question_evidence <- function(question, evidence_result, visuals, retrieval_query, timing = list()) {
      current_module_id <- normalize_rag_module_id(question$module_id, query = question$question_text) %||%
        topic_to_rag_module(question$topic_id)
      active_module_ids <- get_selected_modules(practice_state$selected_modules)
      if (is.list(evidence_result)) {
        evidence_result$current_question_id <- question$question_id
      }
      practice_state$evidence_cache <- list(
        current_question_id = question$question_id,
        active_module_id = current_module_id,
        current_module_id = current_module_id,
        active_module_ids = active_module_ids,
        expected_concept_tag = question$concept_tag,
        retrieval_query = retrieval_query,
        evidence_result = evidence_result,
        visuals = visuals %||% tibble(),
        retrieval_time = timing$retrieval_time %||% NA_real_,
        visual_time = timing$visual_time %||% NA_real_,
        cached_at = Sys.time()
      )
      invisible(practice_state$evidence_cache)
    }

    should_refresh_evidence <- function(question, active_module_id = NULL) {
      cache <- practice_state$evidence_cache
      current_module_id <- active_module_id %||%
        normalize_rag_module_id(question$module_id, query = question$question_text) %||%
        topic_to_rag_module(question$topic_id)
      active_module_ids <- get_selected_modules(practice_state$selected_modules)
      if (is.null(cache)) {
        return(TRUE)
      }
      if (!identical(cache$current_question_id %||% "", question$question_id %||% "")) {
        return(TRUE)
      }
      if (!identical(cache$current_module_id %||% cache$active_module_id %||% "", current_module_id %||% "")) {
        return(TRUE)
      }
      if (!identical(sort(cache$active_module_ids %||% character()), sort(active_module_ids))) {
        return(TRUE)
      }
      if (!identical(cache$expected_concept_tag %||% "", question$concept_tag %||% "")) {
        return(TRUE)
      }
      evidence <- cache$evidence_result$evidence %||% tibble()
      !is.data.frame(evidence) || nrow(evidence) == 0
    }

    get_cached_question_evidence <- function(question, force = FALSE) {
      current_module_id <- normalize_rag_module_id(question$module_id, query = question$question_text) %||%
        topic_to_rag_module(question$topic_id)
      active_module_ids <- get_selected_modules(practice_state$selected_modules)
      if (!isTRUE(force) && !should_refresh_evidence(question, current_module_id)) {
        cache <- practice_state$evidence_cache
        return(list(
          evidence_result = cache$evidence_result,
          visuals = cache$visuals %||% tibble(),
          retrieval_query = cache$retrieval_query %||% build_question_retrieval_query(question),
          used_cached_evidence = TRUE,
          retrieval_time = 0,
          visual_time = 0
        ))
      }

      retrieval_query <- build_question_retrieval_query(question)
      retrieval_start <- Sys.time()
      evidence_result <- tryCatch(
        retrieve_evidence(
          query = retrieval_query,
          active_module_id = current_module_id,
          active_module_ids = active_module_ids,
          current_module_id = current_module_id,
          mode = practice_state$source_mode %||% "general",
          professor_id = practice_state$professor_id %||% NULL,
          top_k = 8L,
          expected_concept_tag = question$concept_tag
        ),
        error = function(e) list(
          query = retrieval_query,
          normalized_query = normalize_student_query(retrieval_query),
          expanded_queries = expand_query(retrieval_query),
          active_module_id = current_module_id,
          current_module_id = current_module_id,
          active_module_ids = active_module_ids,
          inferred_module_id = route_question_to_module(retrieval_query, active_module_ids = active_module_ids),
          expanded_outside_active = FALSE,
          expanded_outside_selected = FALSE,
          evidence = tibble(),
          retrieval_trace = tibble(),
          llm_error = conditionMessage(e)
        )
      )
      retrieval_time <- practice_timer(retrieval_start)

      visual_start <- Sys.time()
      visuals <- tryCatch(
        retrieve_relevant_visuals(
          query = retrieval_query,
          current_question = question,
          concept_tag = question$concept_tag,
          module_id = current_module_id,
          active_module_id = current_module_id,
          top_k = 3L
        ),
        error = function(e) tibble()
      )
      visual_time <- practice_timer(visual_start)

      set_cached_question_evidence(
        question = question,
        evidence_result = evidence_result,
        visuals = visuals,
        retrieval_query = retrieval_query,
        timing = list(retrieval_time = retrieval_time, visual_time = visual_time)
      )

      list(
        evidence_result = evidence_result,
        visuals = visuals,
        retrieval_query = retrieval_query,
        used_cached_evidence = FALSE,
        retrieval_time = retrieval_time,
        visual_time = visual_time
      )
    }

    build_hint_ladder <- function(question) {
      hints <- c(
        question$hint_1 %||% "",
        question$hint_2 %||% "",
        question$hint_3 %||% "",
        question$hint %||% "",
        glue("Hint: focus on {get_concept_label(question$concept_tag %||% question$topic_id)} before doing any calculation."),
        "Hint: name what the question is asking for, then match that to the relevant statistic, parameter, or condition."
      ) %>%
        as.character() %>%
        str_replace_all("[*_`>#]", " ") %>%
        map_chr(clean_student_facing_source_language)
      unique(hints[nzchar(hints)])
    }

    build_fast_practice_help <- function(question, context, help_question, help_mode, cached) {
      total_start <- Sys.time()
      hint_ladder <- build_hint_ladder(question)
      practice_state$hint_count_current_question <- practice_state$hint_count_current_question + 1L
      hint_index <- min(practice_state$hint_count_current_question, length(hint_ladder))
      answer <- hint_ladder[[hint_index]]
      if (!str_detect(str_to_lower(answer), "^hint")) {
        answer <- paste("Hint:", answer)
      }
      evidence_result <- cached$evidence_result
      evidence <- evidence_result$evidence %||% tibble()
      list(
        answer = answer,
        help_mode = help_mode,
        retrieval_query = cached$retrieval_query %||% build_question_retrieval_query(question),
        evidence_used = evidence,
        visuals_used = cached$visuals %||% tibble(),
        confidence = evidence_confidence(evidence, mode = context$mode %||% "general", professor_id = context$professor_id %||% NULL),
        needs_clarification = FALSE,
        hallucination_check = "skipped",
        hallucination_score = NA_real_,
        retrieval_trace = evidence_result$retrieval_trace %||% tibble(),
        normalized_query = evidence_result$normalized_query %||% normalize_student_query(cached$retrieval_query %||% help_question),
        expanded_queries = evidence_result$expanded_queries %||% expand_query(cached$retrieval_query %||% help_question),
        active_module_id = evidence_result$active_module_id %||% context$active_module_id,
        current_module_id = evidence_result$current_module_id %||% context$current_module_id %||% context$active_module_id,
        active_module_ids = evidence_result$active_module_ids %||% context$active_module_ids %||% character(),
        inferred_module_id = evidence_result$inferred_module_id %||% NA_character_,
        expanded_outside_active = isTRUE(evidence_result$expanded_outside_active),
        expanded_outside_selected = isTRUE(evidence_result$expanded_outside_selected),
        answer_submitted = isTRUE(context$answer_submitted),
        answer_withheld = TRUE,
        current_question_id = context$current_question_id %||% question$question_id,
        expected_concept_tag = context$expected_concept_tag %||% question$concept_tag,
        llm_error = "fast_hint_ladder",
        used_cached_evidence = isTRUE(cached$used_cached_evidence),
        llm_calls_count = 0L,
        retrieval_time = cached$retrieval_time %||% 0,
        rerank_time = evidence_result$rerank_time %||% NA_real_,
        generation_time = 0,
        verifier_time = 0,
        total_time = practice_timer(total_start),
        stored_content_used = TRUE,
        concept_anchor_used = context$expected_concept_tag %||% NA_character_,
        concept_mismatch_guardrail = FALSE
      )
    }

    update_tutor_history <- function(role,
                                     text,
                                     help_mode = NULL,
                                     visuals = list(),
                                     evidence_used = NULL,
                                     retrieval_trace = NULL,
                                     message_id = NULL,
                                     max_turns = 10L) {
      turn <- if (exists("create_tutor_message", mode = "function")) {
        create_tutor_message(
          role = role,
          text = text,
          help_mode = help_mode,
          visuals = visuals,
          evidence_used = evidence_used,
          retrieval_trace = retrieval_trace,
          message_id = message_id
        )
      } else {
        list(
          message_id = message_id %||% paste0(role, "_", length(tutor_state$conversation_history %||% list()) + 1L),
          role = role,
          text = str_squish(as.character(text %||% "")),
          help_mode = help_mode %||% NA_character_,
          timestamp = format(Sys.time(), "%I:%M %p"),
          visual_ids = character(),
          visuals = list(),
          evidence_used = evidence_used,
          retrieval_trace = retrieval_trace
        )
      }
      if (!nzchar(turn$text)) {
        return(invisible(tutor_state$conversation_history))
      }
      tutor_state$conversation_history <- tail(c(tutor_state$conversation_history, list(turn)), max_turns)
      invisible(tutor_state$conversation_history)
    }

    new_tutor_message_id <- function(role = "assistant") {
      paste0(
        role,
        "_",
        str_replace_all(format(Sys.time(), "%Y%m%d%H%M%OS3"), "[^A-Za-z0-9]", "_"),
        "_",
        sample.int(999999L, 1L)
      )
    }

    build_tutor_message_visuals <- function(result,
                                            visual_requested = FALSE,
                                            deterministic_visual_type = NULL,
                                            question = NULL,
                                            message_id = NULL) {
      if (!isTRUE(visual_requested)) {
        return(list())
      }

      current_module_id <- result$current_module_id %||%
        result$active_module_id %||%
        question$current_module_id %||%
        question$module_id %||%
        NA_character_
      concept_tag <- result$expected_concept_tag %||%
        question$concept_tag %||%
        question$topic_id %||%
        NA_character_

      if (!is.null(deterministic_visual_type) && nzchar(deterministic_visual_type %||% "") &&
          exists("save_stat2331_visual_png", mode = "function")) {
        visual <- save_stat2331_visual_png(
          visual_type = deterministic_visual_type,
          message_id = message_id,
          module_id = current_module_id,
          concept_tag = concept_tag
        )
        if (!is.null(visual)) {
          return(list(visual))
        }
      }

      question_visuals <- if (!is.null(question) && exists("get_question_visuals", mode = "function")) {
        tryCatch(
          get_question_visuals(question, top_k = 2L, for_tutor = TRUE),
          error = function(e) tibble::tibble()
        )
      } else {
        tibble::tibble()
      }

      if (is.data.frame(question_visuals) && nrow(question_visuals) > 0 &&
          (is.null(deterministic_visual_type) || !nzchar(deterministic_visual_type %||% ""))) {
        return(
          visual_metadata_to_message_visuals(
            visuals = question_visuals,
            message_id = message_id,
            top_k = 2L
          )
        )
      }

      visual_metadata_to_message_visuals(
        visuals = result$visuals_used %||% tibble(),
        message_id = message_id,
        top_k = 2L
      )
    }

    get_tutor_context <- function(context = NULL) {
      context <- context %||% list()
      context$conversation_history <- tutor_state$conversation_history %||% list()
      context$last_tutor_answer <- tutor_state$last_tutor_answer %||% ""
      context$last_help_mode <- tutor_state$last_help_mode %||% ""
      context$last_visual_refs <- tutor_state$last_visual_refs %||% NULL
      context$answer_submitted <- isTRUE(context$answer_submitted %||% tutor_state$answer_submitted)
      context$active_module_ids <- get_selected_modules(context$active_module_ids %||% tutor_state$active_module_ids %||% character())
      context$current_module_id <- context$current_module_id %||% tutor_state$current_module_id %||% context$active_module_id %||% NULL
      context
    }
    
    load_practice_question <- function(question) {
      load_start <- Sys.time()
      req(!is.null(question))
      practice_state$session_question_number <- practice_state$session_question_number + 1L
      practice_state$current_question <- question
      practice_state$topic_id <- question$topic_id
      practice_state$topic_label <- question$topic_label
      practice_state$question_history <- c(practice_state$question_history, question$question_id)
      practice_state$recent_topics <- c(practice_state$recent_topics, question$topic_id)
      practice_state$hint_visible <- FALSE
      practice_state$hints_used_current_question <- 0L
      practice_state$submission_result <- NULL
      practice_state$queued_next_question <- NULL
      practice_state$practice_help <- NULL
      practice_state$practice_help_loading <- FALSE
      practice_state$practice_help_error <- NULL
      practice_state$practice_help_debug <- NULL
      clear_deterministic_tutor_visual()
      practice_state$evidence_cache <- NULL
      practice_state$hint_count_current_question <- 0L
      current_module_id <- normalize_rag_module_id(question$module_id, query = question$question_text) %||% topic_to_rag_module(question$topic_id)
      question$current_module_id <- current_module_id
      practice_state$current_question <- question
      reset_tutor_state(list(
        active_module_id = current_module_id,
        current_module_id = current_module_id,
        active_module_ids = get_selected_modules(practice_state$selected_modules),
        current_question_id = question$question_id,
        question_text = question$question_text,
        answer_choices = question$choices,
        expected_concept_tag = question$concept_tag,
        weak_concept_tag = question$concept_tag
      ))
      message(glue("[timer] loaded practice question state in {practice_timer(load_start)} sec"))
      TRUE
    }
    
    build_next_question <- function(forced_topic = NULL) {
      req(user_info(), practice_state$practice_mode)
      selected_modules <- get_selected_modules(practice_state$selected_modules)
      if (length(selected_modules) == 0) {
        showNotification("Choose at least one module to practice.", type = "warning")
        return(FALSE)
      }

      pool <- get_question_pool() %>%
        filter(module_id %in% selected_modules)
      if (!is.null(forced_topic) && nzchar(forced_topic %||% "")) {
        topic_pool <- pool %>% filter(topic_id == !!forced_topic)
        if (nrow(topic_pool) > 0) {
          pool <- topic_pool
        }
      }

      current_question_id <- if (!is.null(practice_state$current_question)) {
        practice_state$current_question$question_id %||% NULL
      } else {
        NULL
      }
      selection <- choose_next_question(
        question_bank = pool,
        active_module_ids = selected_modules,
        seen_question_ids = practice_state$question_history,
        current_question_id = current_question_id,
        valid_module_ids = MODULES$module_id
      )
      selection_debug <- attr(selection, "selection_debug") %||% list(reason = "unknown")
      practice_state$last_question_selection_debug <- selection_debug

      if (!is.data.frame(selection) || nrow(selection) == 0) {
        reason <- selection_debug$reason %||% "no_question"
        showNotification(
          if (identical(reason, "no_modules_selected")) "Choose at least one module to practice." else "No practice question is available for that selection yet.",
          type = "warning"
        )
        return(FALSE)
      }

      if (isTRUE(selection_debug$seen_reset)) {
        practice_state$question_history <- character()
        practice_state$seen_question_ids <- character()
      }
      next_question <- make_practice_question_from_row(selection)
      if (is.null(next_question)) {
        showNotification("No practice question is available for that selection yet.", type = "warning")
        return(FALSE)
      }
      load_practice_question(next_question)
    }
    
    start_practice_session <- function(module_ids, practice_mode, source_mode = "general", professor_id = NULL) {
      start_timer <- Sys.time()
      message("[timer] Start Practice clicked")
      req(user_info())
      module_ids <- get_selected_modules(module_ids)
      message(glue("[timer] normalized selected modules: {practice_timer(start_timer)} sec"))
      source_mode <- if (source_mode %in% c("general", "professor")) source_mode else "general"
      professor_id <- if (identical(source_mode, "professor")) {
        normalize_scalar_string(professor_id %||% "current_professor") %||% "current_professor"
      } else {
        NULL
      }
      
      if (length(module_ids) == 0) {
        showNotification("Choose at least one module to practice.", type = "warning")
        practice_state$active <- FALSE
        practice_state$selected_modules <- character()
        practice_state$evidence_cache <- NULL
        reset_tutor_state()
        return(invisible(FALSE))
      }

      module_pool <- get_questions_for_module(module_ids)
      message(glue("[timer] filtered question pool: {practice_timer(start_timer)} sec ({nrow(module_pool)} question rows)"))
      if (nrow(module_pool) == 0) {
        showNotification("No practice questions are available for this module yet. Try another module.", type = "warning")
        practice_state$active <- FALSE
        practice_state$selected_modules <- module_ids
        practice_state$evidence_cache <- NULL
        reset_tutor_state()
        return(invisible(FALSE))
      }
      
      practice_state$active <- TRUE
      practice_state$selected_modules <- module_ids
      practice_state$topic_id <- NULL
      practice_state$topic_label <- NULL
      practice_state$practice_mode <- practice_mode
      practice_state$source_mode <- source_mode
      practice_state$professor_id <- professor_id
      practice_state$session_question_number <- 0L
      practice_state$current_level <- 1L
      practice_state$streak_correct <- 0
      practice_state$streak_wrong <- 0L
      practice_state$concept_reminder <- NULL
      practice_state$question_history <- character()
      practice_state$seen_question_ids <- character()
      practice_state$recent_topics <- character()
      practice_state$queued_next_question <- NULL
      practice_state$next_step_message <- NULL
      practice_state$next_step_detail <- NULL
      practice_state$practice_help <- NULL
      practice_state$practice_help_loading <- FALSE
      practice_state$practice_help_error <- NULL
      practice_state$practice_help_debug <- NULL
      practice_state$last_question_selection_debug <- NULL
      clear_deterministic_tutor_visual()
      practice_state$evidence_cache <- NULL
      practice_state$hint_count_current_question <- 0L
      if (!isTRUE(build_next_question())) {
        practice_state$active <- FALSE
      }
      message(glue("[timer] Start Practice total: {practice_timer(start_timer)} sec"))
    }
    
    reset_practice_session <- function() {
      practice_state$active <- FALSE
      practice_state$topic_id <- NULL
      practice_state$topic_label <- NULL
      practice_state$session_question_number <- 0L
      practice_state$current_level <- 1L
      practice_state$streak_correct <- 0
      practice_state$streak_wrong <- 0L
      practice_state$hints_used_current_question <- 0L
      practice_state$current_question <- NULL
      practice_state$submission_result <- NULL
      practice_state$hint_visible <- FALSE
      practice_state$concept_reminder <- NULL
      practice_state$question_history <- character()
      practice_state$recent_topics <- character()
      practice_state$queued_next_question <- NULL
      practice_state$next_step_message <- NULL
      practice_state$next_step_detail <- NULL
      practice_state$source_mode <- "general"
      practice_state$professor_id <- NULL
      practice_state$practice_help <- NULL
      practice_state$practice_help_loading <- FALSE
      practice_state$practice_help_error <- NULL
      practice_state$practice_help_debug <- NULL
      clear_deterministic_tutor_visual()
      practice_state$evidence_cache <- NULL
      practice_state$hint_count_current_question <- 0L
      reset_tutor_state()
    }
    
    observeEvent(input$start_practice, {
      start_practice_session(
        module_ids = practice_state$selected_modules %||% character(),
        practice_mode = "recommended",
        source_mode = "general",
        professor_id = NULL
      )
    })
    
    observeEvent(input$end_practice, {
      reset_practice_session()
    })
    
    observeEvent(input$next_question, {
      req(practice_state$active, !is.null(practice_state$submission_result))
      practice_state$concept_reminder <- NULL
      if (!is.null(practice_state$queued_next_question)) {
        load_practice_question(practice_state$queued_next_question)
      } else {
        build_next_question()
      }
    })
    
    get_current_practice_response <- function(q) {
      if (is.null(q)) {
        return(list(raw = NULL, text = "", has_answer = FALSE))
      }

      raw <- switch(
        q$format,
        multiple_choice = input$practice_response_radio,
        choose_best_answer = input$practice_response_radio,
        fill_in_blank = input$practice_response_text,
        drag_and_drop = {
          interaction_type <- q$interaction_type %||% infer_drag_interaction_type(q$question_text %||% "")
          if (identical(interaction_type, "ordering")) {
            vapply(
              seq_along(q$choices),
              function(i) input[[paste0("practice_response_order_", i)]] %||% "",
              FUN.VALUE = character(1)
            )
          } else if (identical(interaction_type, "categorize")) {
            choice_ids <- get_choice_ids(q$choices)
            stats::setNames(
              vapply(choice_ids, function(choice_id) input[[paste0("practice_response_category_", choice_id)]] %||% "", FUN.VALUE = character(1)),
              choice_ids
            )
          } else {
            input$practice_response_check
          }
        },
        NULL
      )

      text <- if (q$format %in% c("multiple_choice", "choose_best_answer")) {
        get_choice_text_by_id(raw %||% "", q$choices)
      } else if (identical(q$format, "drag_and_drop")) {
        interaction_type <- q$interaction_type %||% infer_drag_interaction_type(q$question_text %||% "")
        if (identical(interaction_type, "ordering")) {
          map_choice_values_to_text(coerce_choice_values(raw), q$choices) %>% paste(collapse = " -> ")
        } else if (identical(interaction_type, "categorize")) {
          if (is.null(raw) || length(raw) == 0) {
            ""
          } else {
            paste0(map_choice_values_to_text(names(raw), q$choices), " -> ", unname(raw)) %>% paste(collapse = "; ")
          }
        } else {
          map_choice_values_to_text(coerce_choice_values(raw), q$choices) %>% paste(collapse = ", ")
        }
      } else {
        practice_value_to_text(raw)
      }

      list(
        raw = raw,
        text = str_squish(text %||% ""),
        has_answer = length(raw %||% character()) > 0 && any(nzchar(as.character(raw %||% "")))
      )
    }

    get_question_attempt_count <- function(user_id, question_id) {
      user_id <- normalize_scalar_string(user_id)
      question_id <- normalize_scalar_string(question_id)
      if (is.null(user_id) || is.null(question_id)) {
        return(0L)
      }
      tryCatch({
        con <- get_db()
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        count <- DBI::dbGetQuery(
          con,
          "SELECT COUNT(*) AS n FROM practice_attempts WHERE user_id = ? AND question_id = ?",
          params = list(user_id, question_id)
        )
        as.integer(count$n[[1]] %||% 0L)
      }, error = function(e) 0L)
    }

    build_contextual_practice_help_context <- function(help_mode) {
      user <- user_info()
      q <- practice_state$current_question
      req(user, q)

      submitted <- practice_state$submission_result
      current_response <- get_current_practice_response(q)
      answer_submitted <- !is.null(submitted)
      student_answer <- if (isTRUE(answer_submitted)) {
        practice_value_to_text(submitted$submitted_answer %||% "")
      } else {
        ""
      }
      attempt_count <- get_question_attempt_count(user$user_id, q$question_id)
      weak_topic <- get_current_weak_topic(
        user_id = user$user_id,
        module_id = q$module_id,
        fallback_to_default = FALSE
      )
      weak_concept <- if (!is.null(submitted) && !isTRUE(submitted$is_correct)) {
        q$concept_tag
      } else if (is_valid_topic_id(weak_topic, require_known = TRUE)) {
        get_concept_tag_for_topic(weak_topic)
      } else {
        q$concept_tag
      }
      active_module_id <- normalize_rag_module_id(q$module_id, query = q$question_text) %||%
        topic_to_rag_module(q$topic_id)
      active_module_ids <- get_selected_modules(practice_state$selected_modules)

      context <- list(
        active_module_id = active_module_id,
        current_module_id = active_module_id,
        active_module_ids = active_module_ids,
        current_question_id = q$question_id,
        question_text = q$question_text,
        topic_id = q$topic_id,
        module_id = q$module_id,
        answer_choices = q$choices,
        correct_answer = if (!is.null(submitted)) submitted$correct_answer %||% q$correct_answer else q$correct_answer,
        grading_rubric = paste(
          practice_value_to_text(q$grading_values %||% ""),
          practice_value_to_text(q$explanation %||% ""),
          sep = " "
        ) %>% str_squish(),
        hint = q$hint %||% "",
        hint_1 = q$hint_1 %||% q$hint %||% "",
        hint_2 = q$hint_2 %||% "",
        hint_3 = q$hint_3 %||% "",
        hint_ladder = build_hint_ladder(q),
        concept_explanation = q$concept_explanation %||% q$explanation %||% "",
        solution_explanation = q$solution_explanation %||% q$explanation %||% "",
        misconception_notes = q$common_mistake %||% "",
        student_answer = student_answer,
        answer_submitted = answer_submitted,
        attempt_count = attempt_count,
        expected_concept_tag = q$concept_tag,
        weak_concept_tag = weak_concept,
        tutor_visual_ids = q$tutor_visual_ids %||% q$visual_ids %||% character(),
        mode = practice_state$source_mode %||% "general",
        professor_id = practice_state$professor_id %||% NULL
      )
      tutor_state$active_module_id <- context$active_module_id
      tutor_state$current_module_id <- context$current_module_id
      tutor_state$active_module_ids <- context$active_module_ids
      tutor_state$current_question_id <- context$current_question_id
      tutor_state$current_question_text <- context$question_text
      tutor_state$current_answer_choices <- context$answer_choices
      tutor_state$student_answer <- context$student_answer
      tutor_state$answer_submitted <- isTRUE(context$answer_submitted)
      tutor_state$attempt_count <- context$attempt_count
      tutor_state$expected_concept_tag <- context$expected_concept_tag
      tutor_state$weak_concept_tag <- context$weak_concept_tag
      context
    }

    infer_practice_help_mode <- function(help_question, has_submitted_answer = FALSE) {
      normalized_question <- normalize_student_query(help_question %||% "")
      if (length(tutor_state$conversation_history %||% list()) > 0 &&
          str_detect(normalized_question, "\\b(why|simpler|that mean|what does that mean|visually|visual|show me|which formula|formula to use)\\b")) {
        return("followup")
      }
      if (isTRUE(has_submitted_answer) && str_detect(normalized_question, "\\b(wrong|mistake|missed|my answer|answer)\\b")) {
        return("diagnose")
      }
      if (str_detect(normalized_question, "\\b(hint|nudge|start|begin|first step)\\b")) {
        return("hint")
      }
      "concept"
    }

    run_contextual_practice_help <- function(help_question = NULL, forced_help_mode = NULL) {
      req(practice_state$active, !is.null(practice_state$current_question))
      q <- practice_state$current_question
      response <- get_current_practice_response(q)

      help_question <- str_squish(help_question %||% input$practice_help_query %||% "")
      if (!nzchar(help_question)) {
        showNotification("Type a question about this practice item first.", type = "warning")
        return(invisible(FALSE))
      }

      explicit_visual_request <- is_visual_request(help_question)
      has_submitted_answer <- !is.null(practice_state$submission_result)
      help_mode <- forced_help_mode %||% infer_practice_help_mode(help_question, has_submitted_answer = has_submitted_answer)
      help_mode <- if (help_mode %in% c("hint", "concept", "diagnose", "followup")) help_mode else "concept"
      attach_visual_to_turn <- if (exists("should_attach_visual_for_help", mode = "function")) {
        should_attach_visual_for_help(
          help_mode = help_mode,
          current_question = q,
          user_text = help_question
        )
      } else {
        explicit_visual_request
      }
      deterministic_visual_type <- if (isTRUE(explicit_visual_request)) {
        choose_visual_type(help_question, q) %||% if (exists("strict_question_visual_type", mode = "function")) strict_question_visual_type(q) else NULL
      } else if (isTRUE(attach_visual_to_turn) && exists("strict_question_visual_type", mode = "function")) {
        strict_question_visual_type(q)
      } else {
        NULL
      }
      clear_deterministic_tutor_visual()

      practice_state$practice_help_loading <- TRUE
      practice_state$practice_help_error <- NULL
      practice_state$practice_help <- NULL
      practice_state$practice_help_debug <- NULL
      update_tutor_history("student", help_question, help_mode = help_mode)

      tryCatch({
        context <- get_tutor_context(build_contextual_practice_help_context(help_mode))
        result <- withProgress(
          message = "Preparing help for this question...",
          value = 0.2,
          {
            if (isTRUE(explicit_visual_request) && !is.null(deterministic_visual_type)) {
              incProgress(0.75, detail = "Rendering a local visual aid")
              retrieval_query <- build_question_retrieval_query(q)
              current_module_id <- normalize_rag_module_id(q$module_id, query = q$question_text) %||%
                topic_to_rag_module(q$topic_id)
              list(
                answer = visual_response_for_type(deterministic_visual_type),
                help_mode = help_mode,
                retrieval_query = retrieval_query,
                evidence_used = tibble(),
                visuals_used = tibble(),
                confidence = "medium",
                needs_clarification = FALSE,
                hallucination_check = "skipped",
                hallucination_score = NA_real_,
                retrieval_trace = tibble(),
                normalized_query = normalize_student_query(retrieval_query),
                expanded_queries = expand_query(retrieval_query),
                active_module_id = current_module_id,
                current_module_id = current_module_id,
                active_module_ids = get_selected_modules(practice_state$selected_modules),
                inferred_module_id = route_question_to_module(retrieval_query, active_module_ids = get_selected_modules(practice_state$selected_modules)),
                expanded_outside_active = FALSE,
                expanded_outside_selected = FALSE,
                answer_submitted = isTRUE(context$answer_submitted),
                answer_withheld = TRUE,
                current_question_id = context$current_question_id %||% q$question_id,
                expected_concept_tag = context$expected_concept_tag %||% q$concept_tag,
                llm_error = "deterministic_visual",
                used_cached_evidence = FALSE,
                llm_calls_count = 0L,
                retrieval_time = 0,
                rerank_time = 0,
                generation_time = 0,
                verifier_time = 0,
                total_time = 0,
                deterministic_visual_type = deterministic_visual_type,
                deterministic_visual_caption = visual_caption_for_type(deterministic_visual_type)
              )
            } else if (identical(help_mode, "hint") && length(build_hint_ladder(q)) > 0) {
              incProgress(0.55, detail = "Showing a stored hint")
              cached <- if (!is.null(practice_state$evidence_cache)) {
                get_cached_question_evidence(q)
              } else {
                retrieval_query <- build_question_retrieval_query(q)
                current_module_id <- normalize_rag_module_id(q$module_id, query = q$question_text) %||%
                  topic_to_rag_module(q$topic_id)
                list(
                  evidence_result = list(
                    query = retrieval_query,
                    normalized_query = normalize_student_query(retrieval_query),
                    expanded_queries = expand_query(retrieval_query),
                    active_module_id = current_module_id,
                    current_module_id = current_module_id,
                    active_module_ids = get_selected_modules(practice_state$selected_modules),
                    inferred_module_id = route_question_to_module(retrieval_query, active_module_ids = get_selected_modules(practice_state$selected_modules)),
                    expanded_outside_active = FALSE,
                    expanded_outside_selected = FALSE,
                    evidence = tibble(),
                    retrieval_trace = tibble()
                  ),
                  visuals = tibble(),
                  retrieval_query = retrieval_query,
                  used_cached_evidence = FALSE,
                  retrieval_time = 0,
                  visual_time = 0
                )
              }
              build_fast_practice_help(
                question = q,
                context = context,
                help_question = help_question,
                help_mode = help_mode,
                cached = cached
              )
            } else {
              incProgress(0.2, detail = "Using cached evidence for the current question")
              cached <- get_cached_question_evidence(q)
              incProgress(0.25, detail = "Building a short grounded tutor response")
              help <- generate_contextual_practice_help(
                help_mode = help_mode,
                practice_context = context,
                help_question = help_question,
                active_module_id = context$active_module_id,
                active_module_ids = context$active_module_ids,
                current_module_id = context$current_module_id,
                mode = context$mode,
                professor_id = context$professor_id,
                use_llm = !identical(help_mode, "hint"),
                evidence_result = cached$evidence_result,
                visual_metadata = cached$visuals,
                run_faithfulness = TRUE
              )
              incProgress(0.3, detail = "Applying answer-safety rules")
              help
            }
          }
        )
        practice_state$practice_help <- result
        practice_state$practice_help_debug <- result
        tutor_state$last_retrieved_evidence <- result$evidence_used %||% NULL
        tutor_state$last_retrieval_trace <- result$retrieval_trace %||% NULL
        tutor_state$last_tutor_answer <- result$answer %||% NULL
        tutor_state$last_help_mode <- result$help_mode %||% help_mode
        assistant_message_id <- new_tutor_message_id("assistant")
        if (isTRUE(explicit_visual_request) && !is.null(deterministic_visual_type)) {
          result$answer <- visual_response_for_type(deterministic_visual_type)
          result$deterministic_visual_type <- deterministic_visual_type
          result$deterministic_visual_caption <- visual_caption_for_type(deterministic_visual_type)
          practice_state$practice_help <- result
          practice_state$practice_help_debug <- result
          tutor_state$last_tutor_answer <- result$answer
        } else if (isTRUE(explicit_visual_request)) {
          result$deterministic_visual_type <- NA_character_
          result$deterministic_visual_caption <- NA_character_
          practice_state$practice_help <- result
          practice_state$practice_help_debug <- result
        }
        message_visuals <- build_tutor_message_visuals(
          result = result,
          visual_requested = attach_visual_to_turn,
          deterministic_visual_type = deterministic_visual_type,
          question = q,
          message_id = assistant_message_id
        )
        result$message_id <- assistant_message_id
        result$message_visuals <- message_visuals
        result$message_visual_ids <- purrr::map_chr(message_visuals, ~ as.character(.x$visual_id %||% "")) %>%
          discard(~ !nzchar(.x))
        practice_state$practice_help <- result
        practice_state$practice_help_debug <- result
        tutor_state$last_visual_refs <- message_visuals %||% result$visuals_used %||% NULL
        update_tutor_history(
          "assistant",
          result$answer %||% "",
          help_mode = result$help_mode %||% help_mode,
          visuals = message_visuals,
          evidence_used = result$evidence_used %||% NULL,
          retrieval_trace = result$retrieval_trace %||% NULL,
          message_id = assistant_message_id
        )
        updateTextAreaInput(session, "practice_help_query", value = "")
        showNotification("Contextual help is ready.", type = "message")
      }, error = function(e) {
        practice_state$practice_help_error <- conditionMessage(e)
        showNotification("The contextual tutor hit an error. Try rephrasing your question about this practice item.", type = "warning")
      }, finally = {
        practice_state$practice_help_loading <- FALSE
      })
      invisible(TRUE)
    }

    observeEvent(input$practice_help_ask, {
      run_contextual_practice_help()
    })

    observeEvent(input$practice_help_hint, {
      run_contextual_practice_help("Give me a hint.", forced_help_mode = "hint")
    })

    observeEvent(input$practice_help_concept, {
      run_contextual_practice_help("Explain this concept.", forced_help_mode = "concept")
    })
    
    output$practice_panel <- renderUI({
      selected_modules <- get_selected_modules(practice_state$selected_modules)
      default_modules <- selected_modules
      
      if (!practice_state$active) {
        return(
          div(
            class = "practice-main-shell",
            card(
              card_header("Introduction to Statistics Practice"),
              div(class = "practice-setup-lead", "Choose a module and start practicing. The app will choose the question type, difficulty, and next step for you."),
              div(
                class = "practice-setup-stack",
                div(
                  class = "practice-setup-section",
                  div(class = "practice-setup-section-title", "Choose module(s) to practice"),
                  div(
                    class = "module-selector",
                    uiOutput(session$ns("practice_module_buttons"))
                  )
                )
              ),
              div(
                class = "practice-setup-summary",
                span(class = "practice-badge", if (length(default_modules) == 0) "Choose at least one module" else get_module_selection_summary(default_modules)),
                span(class = "practice-badge", "Question type and difficulty chosen automatically")
              ),
              if (!file.exists(QUESTION_BANK_PATH)) {
                div(
                  class = "practice-empty-note",
                  "The processed question bank could not be found. The app is using starter questions for this local run. To rebuild the full bank, run: source(\"R/build_question_bank.R\"); build_question_bank()"
                )
              },
              div(
                class = "practice-setup-actions",
                actionButton(session$ns("start_practice"), "Start Practice", class = "btn-primary")
              )
            )
          )
        )
      }
      
      q <- practice_state$current_question
      req(!is.null(q))
      
      div(
        class = "practice-main-shell",
        card(
          card_header("Practice session"),
          p(class = "practice-topic-line", glue("Selected modules: {get_module_selection_summary(practice_state$selected_modules)}")),
          div(
            class = "practice-session-meta",
            span(class = "practice-badge", glue("Question {practice_state$session_question_number}")),
            span(class = "practice-badge", q$module_label),
            span(class = "practice-badge", q$topic_label),
            span(class = "practice-badge", get_concept_label(q$concept_tag))
          ),
          div(class = "practice-question-wrap", uiOutput(session$ns("practice_question"))),
          uiOutput(session$ns("practice_response_ui")),
          if (is.null(practice_state$submission_result)) {
            div(
              class = "practice-actions",
              actionButton(session$ns("submit_answer"), "Submit Answer", class = "btn-primary"),
              actionButton(session$ns("end_practice"), "Change module", class = "btn btn-outline-secondary")
            )
          },
          uiOutput(session$ns("practice_feedback")),
          uiOutput(session$ns("practice_hint"))
        ),
        uiOutput(session$ns("practice_contextual_help")),
        if (!is.null(practice_state$concept_reminder)) {
          card(
            card_header("Concept reminder"),
            p(practice_state$concept_reminder)
          )
        }
      )
    })
    
    output$help_advanced <- renderUI({
      topic_choices <- get_help_topic_choices(user_id = user_info()$user_id %||% NULL)
      
      tags$details(
        class = "help-entry",
        tags$summary("Advanced: choose topic manually"),
        p(class = "small-muted", "Optional. Leave this on auto if you want the app to route the question from your wording."),
        selectInput(
          session$ns("help_topic_manual"),
          "Optional topic override",
          choices = topic_choices,
          selected = "__auto__"
        )
      )
    })
    
    output$practice_question <- renderUI({
      q <- practice_state$current_question
      req(!is.null(q))
      
      tagList(
        if (!identical(q$visual_position %||% "above", "below")) {
          render_question_visuals(q)
        },
        div(class = "practice-question-title", q$question_text),
        p(class = "practice-topic-line", glue("Topic focus: {q$topic_label} | Skill: {get_concept_label(q$concept_tag)}")),
        if (identical(q$visual_position %||% "above", "below")) {
          render_question_visuals(q)
        },
        if (isTRUE(q$is_fallback)) {
          p(class = "small-muted", "Starter-bank fallback item while more module-specific questions are added.")
        }
      )
    })
    
    output$practice_response_ui <- renderUI({
      q <- practice_state$current_question
      req(!is.null(q))
      
      if (q$format %in% c("multiple_choice", "choose_best_answer")) {
        choice_values <- get_choice_ids(q$choices)
        choice_labels <- get_choice_texts(q$choices)
        return(
          div(
            class = "practice-response-shell practice-choice-group",
            radioButtons(
              session$ns("practice_response_radio"),
              "Choose one answer",
              choices = stats::setNames(choice_values, choice_labels),
              selected = character(0)
            )
          )
        )
      }
      
      if (identical(q$format, "fill_in_blank")) {
        return(
          div(
            class = "practice-response-shell",
            textInput(
              session$ns("practice_response_text"),
              "Type your answer",
              value = "",
              placeholder = "Enter your answer"
            )
          )
        )
      }
      
      interaction_type <- q$interaction_type %||% infer_drag_interaction_type(q$question_text %||% "")
      choice_ids <- get_choice_ids(q$choices)
      choice_texts <- get_choice_texts(q$choices)
      
      if (identical(interaction_type, "ordering")) {
        ordering_choices <- c("Choose a step" = "", stats::setNames(choice_ids, choice_texts))
        return(
          div(
            class = "practice-response-shell",
            p(class = "small-muted", "Put the steps in order."),
            div(
              class = "practice-ordering-grid",
              lapply(seq_along(choice_ids), function(i) {
                selectInput(
                  session$ns(paste0("practice_response_order_", i)),
                  glue("Step {i}"),
                  choices = ordering_choices,
                  selected = ""
                )
              })
            )
          )
        )
      }
      
      if (identical(interaction_type, "categorize")) {
        categories <- get_drag_categories(
          correct_answer = q$grading_values,
          choices = q$choices,
          interaction_type = interaction_type,
          question_text = q$question_text
        )
        if (length(categories) == 0) {
          return(
            div(
              class = "practice-response-shell",
              p(class = "small-muted", "This categorize question is missing category labels, so it cannot be answered yet.")
            )
          )
        }
        category_choices <- c("Choose a category" = "", stats::setNames(categories, categories))
        
        return(
          div(
            class = "practice-response-shell",
            p(class = "small-muted", "Sort each item into the right category."),
            div(
              class = "practice-categorize-grid",
              lapply(seq_along(choice_ids), function(i) {
                selectInput(
                  session$ns(paste0("practice_response_category_", choice_ids[[i]])),
                  choice_texts[[i]],
                  choices = category_choices,
                  selected = ""
                )
              })
            )
          )
        )
      }
      
      div(
        class = "practice-response-shell practice-choice-group",
        checkboxGroupInput(
          session$ns("practice_response_check"),
          "Select all that apply",
          choices = stats::setNames(choice_ids, choice_texts),
          selected = character(0)
        )
      )
    })

    output$practice_contextual_help <- renderUI({
      q <- practice_state$current_question
      req(!is.null(q))

      help <- practice_state$practice_help
      debug <- practice_state$practice_help_debug
      evidence_ids <- if (!is.null(debug$evidence_used) && is.data.frame(debug$evidence_used) && nrow(debug$evidence_used) > 0) {
        paste(head(debug$evidence_used$chunk_id, 8), collapse = ", ")
      } else {
        "none"
      }
      trace_view <- if (is_development_mode() && !is.null(debug$retrieval_trace) && is.data.frame(debug$retrieval_trace) && nrow(debug$retrieval_trace) > 0) {
        debug$retrieval_trace %>%
          mutate(
            final_score = round(final_score, 3),
            keyword_score = round(keyword_score, 3),
            semantic_score = round(semantic_score, 3),
            module_match_boost = round(module_match_boost, 3),
            current_module_match_boost = round(current_module_match_boost, 3),
            selected_module_match_boost = round(selected_module_match_boost, 3),
            concept_tag_boost = round(concept_tag_boost, 3),
            current_question_concept_boost = round(current_question_concept_boost, 3),
            source_policy_boost = round(source_policy_boost, 3),
            outside_selected_modules_penalty = round(outside_selected_modules_penalty, 3),
            wrong_module_penalty = round(wrong_module_penalty, 3),
            unrelated_concept_penalty = round(unrelated_concept_penalty, 3)
          ) %>%
          select(
            chunk_id,
            module_id,
            concept_tag,
            source_type,
            module_policy,
            keyword_score,
            semantic_score,
            module_match_boost,
            current_module_match_boost,
            selected_module_match_boost,
            concept_tag_boost,
            current_question_concept_boost,
            source_policy_boost,
            outside_selected_modules_penalty,
            wrong_module_penalty,
            unrelated_concept_penalty,
            final_score
          ) %>%
          head(8)
      } else {
        tibble(note = "No retrieval trace available.")
      }
      history <- tutor_state$conversation_history %||% list()
      history_ui <- if (length(history) == 0) {
        div(class = "help-entry assistant", p("Ask anything about this practice question. I will keep the thread tied to this item."))
      } else {
        tagList(lapply(history, function(turn) {
          role <- turn$role %||% "assistant"
          message_body <- if (identical(role, "student")) {
            p(turn$text %||% "")
          } else {
            div(
              class = "tutor-message",
              markdown_to_ui(turn$text %||% ""),
              render_tutor_message_visuals(turn$visuals %||% list())
            )
          }
          div(
            class = paste("help-entry", if (identical(role, "student")) "user" else "assistant"),
            p(strong(if (identical(role, "student")) "You" else "Tutor")),
            message_body,
            p(class = "small-muted", glue("{turn$help_mode %||% 'follow-up'} | {turn$timestamp %||% ''}"))
          )
        }))
      }
      visuals <- help$visuals_used %||% tibble()
      message_visual_debug <- purrr::map_dfr(history, function(turn) {
        turn_visuals <- if (exists("normalize_tutor_message_visuals", mode = "function")) {
          normalize_tutor_message_visuals(turn$visuals %||% list())
        } else {
          turn$visuals %||% list()
        }
        if (length(turn_visuals) == 0) {
          return(tibble())
        }
        purrr::map_dfr(turn_visuals, function(visual) {
          tibble(
            message_id = turn$message_id %||% NA_character_,
            visual_id = visual$visual_id %||% visual$image_id %||% NA_character_,
            visual_type = visual$visual_type %||% NA_character_,
            source_type = visual$source_type %||% NA_character_,
            concept_tag = visual$concept_tag %||% NA_character_,
            module_id = visual$module_id %||% NA_character_,
            safe_for_deployment = isTRUE(visual$safe_for_deployment),
            display_permission_status = visual$display_permission_status %||% NA_character_
          )
        })
      })

      card(
        card_header("Ask about this question"),
        p(
          class = "small-muted",
          glue("Helping with: {q$module_label %||% 'current module'} / {get_concept_label(q$concept_tag %||% q$topic_id)}. Using current practice question context.")
        ),
        div(class = "help-thread", history_ui),
        div(
          class = "practice-actions",
          actionButton(session$ns("practice_help_hint"), "Give me a hint"),
          actionButton(session$ns("practice_help_concept"), "Explain this concept")
        ),
        textAreaInput(
          session$ns("practice_help_query"),
          label = "Ask a follow-up",
          value = "",
          rows = 2,
          placeholder = "Example: Can you explain that simpler?"
        ),
        div(
          class = "practice-actions",
          actionButton(session$ns("practice_help_ask"), "Ask", class = "btn-primary")
        ),
        if (isTRUE(practice_state$practice_help_loading)) {
          div(class = "help-entry assistant", p(strong("Working on it")), p("Retrieving evidence from the active module and nearby course context."))
        },
        if (!is.null(practice_state$practice_help_error) && nzchar(practice_state$practice_help_error)) {
          p(class = "small-muted", glue("Note: {practice_state$practice_help_error}"))
        },
        if (is_development_mode() && !is.null(help)) {
          div(
            class = "help-entry assistant",
            p(strong("Latest response details")),
            p(class = "small-muted", glue("Confidence: {help$confidence %||% 'unknown'} | Evidence checked: {if (!is.null(help$evidence_used) && is.data.frame(help$evidence_used)) nrow(help$evidence_used) else 0} chunk(s).")),
            if (!is.null(help$total_time)) {
              p(class = "small-muted", glue("Tutor response time: {round(help$total_time %||% 0, 3)}s"))
            },
            if (isTRUE(help$answer_withheld)) {
              p(class = "small-muted", "Final answer withheld so you can keep working through the reasoning.")
            }
          )
        },
        if (is_development_mode() && !is.null(debug)) {
          tags$details(
            class = "help-entry",
            tags$summary("Internal diagnostics: practice tutor"),
            p(class = "small-muted", glue("help_mode: {debug$help_mode %||% 'none'}")),
            p(class = "small-muted", glue("current_question_id: {debug$current_question_id %||% 'none'}")),
            p(class = "small-muted", glue("expected_concept_tag: {debug$expected_concept_tag %||% 'none'}")),
            p(class = "small-muted", glue("stored hint/explanation used: {isTRUE(debug$stored_content_used)}")),
            p(class = "small-muted", glue("concept anchor used: {debug$concept_anchor_used %||% 'none'}")),
            p(class = "small-muted", glue("concept mismatch guardrail: {isTRUE(debug$concept_mismatch_guardrail)}")),
            p(class = "small-muted", glue("active_module_id: {debug$active_module_id %||% 'none'}")),
            p(class = "small-muted", glue("current_module_id: {debug$current_module_id %||% debug$active_module_id %||% 'none'}")),
            p(class = "small-muted", glue("active_module_ids: {paste(debug$active_module_ids %||% character(), collapse = ', ')}")),
            p(class = "small-muted", glue("question pool size: {practice_state$last_question_selection_debug$pool_size %||% NA} | candidate count: {practice_state$last_question_selection_debug$candidate_count %||% NA} | seen reset: {isTRUE(practice_state$last_question_selection_debug$seen_reset)}")),
            p(class = "small-muted", glue("inferred_module_id: {debug$inferred_module_id %||% 'none'}")),
            p(class = "small-muted", glue("expanded outside active module: {isTRUE(debug$expanded_outside_active)}")),
            p(class = "small-muted", glue("expanded outside selected modules: {isTRUE(debug$expanded_outside_selected)}")),
            p(class = "small-muted", glue("answer_submitted: {isTRUE(debug$answer_submitted)}")),
            p(class = "small-muted", glue("final answer withheld: {isTRUE(debug$answer_withheld)}")),
            p(class = "small-muted", glue("used_cached_evidence: {isTRUE(debug$used_cached_evidence)} | llm_calls_count: {debug$llm_calls_count %||% 0}")),
            p(class = "small-muted", glue("timing: retrieval={debug$retrieval_time %||% NA}s | rerank={debug$rerank_time %||% NA}s | generation={debug$generation_time %||% NA}s | verifier={debug$verifier_time %||% NA}s | total={debug$total_time %||% NA}s")),
            p(class = "small-muted", glue("faithfulness: {debug$hallucination_check %||% 'unknown'} | confidence: {debug$confidence %||% 'unknown'}")),
            p(class = "small-muted", glue("normalized query: {debug$normalized_query %||% ''}")),
            p(class = "small-muted", glue("retrieval query: {debug$retrieval_query %||% ''}")),
            p(class = "small-muted", glue("evidence used: {evidence_ids}")),
            p(class = "small-muted", glue("message_id: {debug$message_id %||% 'none'}")),
            p(class = "small-muted", glue("deterministic visual type: {debug$deterministic_visual_type %||% 'none'}")),
            if (nrow(message_visual_debug) > 0) {
              tags$pre(paste(capture.output(print(message_visual_debug, n = Inf)), collapse = "\n"))
            },
            if (is.data.frame(visuals) && nrow(visuals) > 0) {
              tags$pre(paste(capture.output(print(select(visuals, any_of(c("image_id", "source_type", "display_permission_status", "safe_for_deployment", "final_visual_score"))), n = Inf)), collapse = "\n"))
            },
            tags$pre(paste(capture.output(print(trace_view, n = Inf)), collapse = "\n"))
          )
        }
      )
    })
    
    observeEvent(input$submit_answer, {
      req(user_info(), practice_state$active)
      q <- practice_state$current_question
      req(!is.null(q))
      
      if (!is.null(practice_state$submission_result)) {
        showNotification("This question is already graded. Continue to the next question when you're ready.", type = "message")
        return()
      }
      
      response <- switch(
        q$format,
        multiple_choice = input$practice_response_radio,
        choose_best_answer = input$practice_response_radio,
        fill_in_blank = input$practice_response_text,
        drag_and_drop = {
          interaction_type <- q$interaction_type %||% infer_drag_interaction_type(q$question_text %||% "")
          if (identical(interaction_type, "ordering")) {
            vapply(
              seq_along(q$choices),
              function(i) input[[paste0("practice_response_order_", i)]] %||% "",
              FUN.VALUE = character(1)
            )
          } else if (identical(interaction_type, "categorize")) {
            choice_ids <- get_choice_ids(q$choices)
            stats::setNames(
              vapply(choice_ids, function(choice_id) input[[paste0("practice_response_category_", choice_id)]] %||% "", FUN.VALUE = character(1)),
              choice_ids
            )
          } else {
            input$practice_response_check
          }
        },
        NULL
      )
      
      grade <- grade_practice_question(q, response)
      
      if (!isTRUE(grade$is_valid)) {
        showNotification(grade$validation_message %||% "Please answer the question before submitting.", type = "warning")
        return()
      }
      
      practice_state$hint_visible <- TRUE
      practice_state$submission_result <- grade
      
      student_answer_text <- grade$submitted_answer
      if (is.character(student_answer_text) && length(student_answer_text) > 1) {
        student_answer_text <- paste(student_answer_text, collapse = "; ")
      }
      
      record_attempt(
        user_id = user_info()$user_id,
        topic_id = q$topic_id,
        question_format = q$format,
        difficulty = q$difficulty,
        correct = as.integer(grade$is_correct),
        hints_used = practice_state$hints_used_current_question,
        question_id = q$question_id,
        student_answer = student_answer_text %||% NA_character_,
        module_id = q$module_id,
        concept_tag = q$concept_tag
      )
      attempts_refresh(attempts_refresh() + 1L)
      practice_state$next_step_message <- NULL
      practice_state$next_step_detail <- NULL
      
      if (isTRUE(grade$is_correct)) {
        practice_state$streak_wrong <- 0L
        practice_state$concept_reminder <- NULL
        practice_state$streak_correct <- practice_state$streak_correct + if (practice_state$hints_used_current_question == 0L) 1 else 0.5
        
        if (practice_state$streak_correct >= 2) {
          practice_state$current_level <- min(3L, practice_state$current_level + 1L)
          practice_state$streak_correct <- 0
        }
      } else {
        practice_state$streak_correct <- 0
        practice_state$streak_wrong <- practice_state$streak_wrong + 1L
        
        if (practice_state$streak_wrong >= 2L) {
          practice_state$current_level <- max(1L, practice_state$current_level - 1L)
          practice_state$streak_wrong <- 0L
          practice_state$concept_reminder <- extract_concept_sections(q$topic_id)$explanation
        }
      }
      
      next_step <- plan_next_practice_step(
        user_id = user_info()$user_id,
        current_question = q,
        is_correct = isTRUE(grade$is_correct),
        hints_used = practice_state$hints_used_current_question,
        module_ids = practice_state$selected_modules,
        practice_mode = practice_state$practice_mode,
        current_level = practice_state$current_level,
        exclude_question_ids = tail(practice_state$question_history, 5),
        recent_topics = practice_state$recent_topics
      )
      practice_state$queued_next_question <- next_step$question
      practice_state$next_step_message <- next_step$message %||% "Next question is ready."
      practice_state$next_step_detail <- next_step$detail %||% NULL
      
      showNotification(
        if (isTRUE(grade$is_correct)) "Correct. The attempt has been saved." else "Not quite. Review the explanation and keep working on the same skill.",
        type = if (isTRUE(grade$is_correct)) "message" else "warning"
      )
    })
    
    output$practice_hint <- renderUI({
      q <- practice_state$current_question
      req(!is.null(q))
      req(practice_state$hint_visible || !is.null(practice_state$submission_result))
      
      card(
        card_header("Hint"),
        p(q$hint %||% "No hint available yet.")
      )
    })
    
    output$practice_feedback <- renderUI({
      q <- practice_state$current_question
      result <- practice_state$submission_result
      req(!is.null(q), !is.null(result))
      
      user_answer <- result$submitted_answer
      if (is.character(user_answer) && length(user_answer) > 1) {
        user_answer <- if (identical(q$interaction_type %||% "", "ordering")) paste(user_answer, collapse = " -> ") else paste(user_answer, collapse = ", ")
      }
      if (!nzchar(user_answer %||% "")) {
        user_answer <- "No answer recorded"
      }
      
      correct_answer <- result$correct_answer
      if (is.character(correct_answer) && length(correct_answer) > 1) {
        correct_answer <- if (identical(q$interaction_type %||% "", "ordering")) paste(correct_answer, collapse = " -> ") else paste(correct_answer, collapse = ", ")
      }
      
      card(
        class = paste("practice-feedback-card", if (isTRUE(result$is_correct)) "correct" else "incorrect"),
        card_header(if (isTRUE(result$is_correct)) "Correct" else "Not quite"),
        div(
          class = "practice-feedback-grid",
          div(class = "practice-feedback-line", strong("Your answer: "), user_answer),
          div(class = "practice-feedback-line", strong("Correct answer: "), correct_answer),
          div(class = "practice-feedback-line", strong("Explanation: "), markdown_to_ui(get_feedback_explanation(q, result))),
          div(class = "practice-feedback-line", strong("Up next: "), practice_state$next_step_message %||% "The app is choosing the next question."),
          if (!is.null(practice_state$next_step_detail) && nzchar(practice_state$next_step_detail)) {
            div(class = "practice-feedback-line", strong("Note: "), practice_state$next_step_detail)
          }
        ),
        p(class = "small-muted", glue("Hints used on this question: {practice_state$hints_used_current_question}")),
        div(
          class = "practice-actions",
          actionButton(session$ns("next_question"), "Next Question", class = "btn-primary")
        )
      )
    })
    
    observeEvent(input$submit_help, {
      user <- user_info()
      user_id <- normalize_scalar_string(user$user_id %||% NULL)
      query_text <- str_squish(input$help_query %||% "")
      fallback_error_text <- get_help_failure_message()
      
      if (is.null(user_id)) {
        showNotification("The practice session is not ready yet. Reload the app and try again.", type = "warning")
        return()
      }
      if (!nzchar(query_text)) {
        showNotification("Type a question first so I know what you are stuck on.", type = "warning")
        return()
      }
      
      help_state$is_loading <- TRUE
      help_state$latest_error <- NULL
      help_state$route_note <- NULL
      help_state$latest_exchange <- NULL
      help_state$latest_debug <- NULL
      help_state$status_text <- "Working on a grounded explanation for you."
      
      tryCatch({
        help_mode <- input$help_source_mode %||% "general"
        if (!help_mode %in% c("general", "professor")) {
          help_mode <- "general"
        }
        selected_professor_id <- if (identical(help_mode, "professor")) {
          normalize_scalar_string(input$help_professor_id %||% "current_professor") %||% "current_professor"
        } else {
          NULL
        }
        active_module_id <- normalize_rag_module_id(input$help_active_module %||% NULL, query = query_text)
        
        selected_topic <- resolve_help_topic(
          user_id = user_id,
          module_id = NULL,
          topic_choice = input$help_topic_manual
        )
        help_route <- route_help_topic(
          user_id = user_id,
          module_id = NULL,
          topic_choice = selected_topic,
          query_text = query_text
        )
        resolved_topic <- help_route$topic_id
        message(glue("[help] routed topic_id={resolved_topic %||% 'NULL'} source={help_route$route_source} active_module={active_module_id %||% 'auto'} mode={help_mode}"))
        
        concept_context <- build_help_context(resolved_topic, query_text)
        help_state$route_note <- help_route$route_note %||% NULL
        help_state$status_text <- if (help_route$route_source %in% c("general", "default")) {
          "I am routing this through a safe topic match and keeping the explanation concise."
        } else {
          "Building a course-grounded explanation for you."
        }
        
        help_result <- tryCatch(
          withProgress(
            message = "Working on your explanation...",
            value = 0.15,
            {
              incProgress(0.25, detail = "Normalizing notation and routing the module")
              grounded_feedback <- generate_grounded_feedback(
                query = query_text,
                active_module_id = active_module_id,
                mode = help_mode,
                professor_id = selected_professor_id
              )
              incProgress(0.35, detail = "Checking retrieved evidence")
              help_state$latest_debug <- grounded_feedback
              list(
                response_object = grounded_feedback_to_help_response(
                  grounded_feedback,
                  topic_label = help_route$topic_label %||% "Course-grounded help"
                ),
                source = if (!is.na(grounded_feedback$llm_error %||% NA_character_)) "rag_fallback" else "rag",
                error_message = grounded_feedback$llm_error %||% NA_character_,
                feedback = grounded_feedback
              )
            }
          ),
          error = function(e) {
            message(glue("[help] RAG response fallback: {conditionMessage(e)}"))
            legacy_result <- call_claude_help(
              query = query_text,
              topic_id = resolved_topic,
              concept_context = concept_context
            )
            legacy_result$source <- paste("legacy", legacy_result$source %||% "fallback", sep = "_")
            legacy_result$error_message <- legacy_result$error_message %||% conditionMessage(e)
            legacy_result
          }
        )
        
        response_object <- normalize_help_response_object(
          help_result$response_object %||% build_unexpected_help_response(help_route$topic_label),
          routed_topic_label = help_route$topic_label
        )
        response_text <- serialize_help_response_object(response_object)
        
        message(glue("[help] response_source={help_result$source %||% 'unknown'} used_fallback={!identical(help_result$source, 'rag')}"))
        save_result <- safe_record_help_query(
          user_id = user_id,
          topic_id = resolved_topic,
          query_text = query_text,
          response_text = response_text,
          error_message = help_result$error_message %||% NA_character_,
          module_id = help_route$module_id,
          concept_tag = help_route$concept_tag
        )
        
        if (isTRUE(save_result$ok)) {
          help_state$latest_exchange <- NULL
          help_refresh(help_refresh() + 1L)
        } else {
          help_state$latest_exchange <- list(
            query_text = query_text,
            response_object = response_object,
            topic_label = help_route$topic_label %||% "General course question",
            timestamp = current_utc_timestamp(),
            error_note = help_result$error_message %||% save_result$error %||% NULL
          )
        }
        
        updateTextAreaInput(session, "help_query", value = "")
        help_state$latest_error <- help_result$error_message %||% save_result$error %||% NULL
        help_state$status_text <- if (identical(help_result$source, "rag")) {
          "A course-grounded explanation is ready below."
        } else if (identical(help_result$source, "rag_fallback")) {
          "A grounded fallback explanation is ready below because live AI help is limited in this local run."
        } else if (help_route$route_source %in% c("general", "default")) {
          "A safe fallback explanation is ready below. If needed, ask again with a keyword like p-value, margin of error, or z-score."
        } else {
          "A concise fallback explanation is ready below because live AI help is limited in this local run."
        }
        showNotification("Your help response is ready below.", type = "message")
      }, error = function(e) {
        error_message <- conditionMessage(e)
        fallback_route <- tryCatch(
          route_help_topic(
            user_id = user_id,
            module_id = NULL,
            topic_choice = input$help_topic_manual %||% "__auto__",
            query_text = query_text
          ),
          error = function(route_error) {
            default_topic <- sanitize_topic_id("ht_foundations", require_known = TRUE)
            list(
              topic_id = default_topic,
              topic_label = if (!is.null(default_topic)) get_topic_label(default_topic) else "General course help",
              module_id = if (!is.null(default_topic)) get_module_for_topic(default_topic) else NA_character_,
              concept_tag = detect_help_concept_tag(query_text, default_topic),
              route_source = "default",
              route_note = "The help router hit an unexpected error, so I used a safe fallback topic.",
              response_intro = NA_character_
            )
          }
        )
        
        message(glue("[help] unexpected observer error: {error_message}"))
        fallback_response <- build_unexpected_help_response(fallback_route$topic_label %||% "General course help")
        save_result <- safe_record_help_query(
          user_id = user_id,
          topic_id = fallback_route$topic_id,
          query_text = query_text,
          response_text = serialize_help_response_object(fallback_response),
          error_message = error_message,
          module_id = fallback_route$module_id,
          concept_tag = fallback_route$concept_tag
        )
        
        if (isTRUE(save_result$ok)) {
          help_state$latest_exchange <- NULL
          help_refresh(help_refresh() + 1L)
        } else {
          help_state$latest_exchange <- list(
            query_text = query_text,
            response_object = fallback_response,
            topic_label = fallback_route$topic_label %||% "General course question",
            timestamp = current_utc_timestamp(),
            error_note = save_result$error %||% error_message
          )
        }
        
        help_state$latest_error <- error_message
        help_state$route_note <- fallback_route$route_note %||% NULL
        help_state$status_text <- "I hit an unexpected error, but I showed a safe fallback response below."
        showNotification(fallback_error_text, type = "warning", duration = 8)
      }, finally = {
        help_state$is_loading <- FALSE
      })
    })
    
    output$help_status <- renderUI({
      if (isTRUE(help_state$is_loading)) {
        return(
          div(
            class = "help-entry assistant",
            p(strong("Working on it")),
            p(help_state$status_text %||% "Building a course-grounded explanation for you...")
          )
        )
      }
      
      if (!is.null(help_state$latest_error) && nzchar(help_state$latest_error)) {
        return(
          div(
            p(class = "small-muted", help_state$status_text %||% "A fallback explanation was used."),
            if (!is.null(help_state$route_note)) p(class = "small-muted", help_state$route_note),
            p(class = "small-muted", glue("Note: {help_state$latest_error}"))
          )
        )
      }
      
      if (!is.null(help_state$status_text)) {
        return(
          div(
            p(class = "small-muted", help_state$status_text),
            if (!is.null(help_state$route_note)) p(class = "small-muted", help_state$route_note)
          )
        )
      }
      
      p(class = "small-muted", "Ask a question and the app will respond with a grounded explanation.")
    })
    
    output$help_debug_panel <- renderUI({
      if (!is_development_mode()) {
        return(NULL)
      }
      debug <- help_state$latest_debug
      if (is.null(debug)) {
        return(NULL)
      }
      trace <- debug$retrieval_trace %||% tibble()
      trace_view <- if (is.data.frame(trace) && nrow(trace) > 0) {
        trace %>%
          mutate(
            final_score = round(final_score, 3),
            keyword_score = round(keyword_score, 3),
            semantic_score = round(semantic_score, 3),
            module_match_boost = round(module_match_boost, 3),
            source_policy_boost = round(source_policy_boost, 3),
            source_priority_boost = round(source_priority_boost, 3),
            wrong_module_penalty = round(wrong_module_penalty, 3)
          ) %>%
          select(
            chunk_id,
            module_id,
            topic_id,
            concept_tag,
            source_type,
            source_scope,
            module_policy,
            keyword_score,
            semantic_score,
            module_match_boost,
            source_policy_boost,
            source_priority_boost,
            wrong_module_penalty,
            final_score
          ) %>%
          head(8)
      } else {
        tibble(note = "No retrieval trace available.")
      }
      evidence_ids <- if (!is.null(debug$evidence_used) && is.data.frame(debug$evidence_used) && nrow(debug$evidence_used) > 0) {
        paste(head(debug$evidence_used$chunk_id, 8), collapse = ", ")
      } else {
        "none"
      }
      
      tags$details(
        class = "help-entry",
        tags$summary("Internal diagnostics"),
        p(class = "small-muted", glue("active_module_id: {debug$active_module_id %||% 'none'}")),
        p(class = "small-muted", glue("inferred_module_id: {debug$inferred_module_id %||% 'none'}")),
        p(class = "small-muted", glue("expanded outside active module: {isTRUE(debug$expanded_outside_active)}")),
        p(class = "small-muted", glue("faithfulness: {debug$hallucination_check %||% 'unknown'} | confidence: {debug$confidence %||% 'unknown'}")),
        p(class = "small-muted", glue("normalized query: {debug$normalized_query %||% ''}")),
        p(class = "small-muted", glue("evidence used: {evidence_ids}")),
        tags$ul(lapply(debug$expanded_queries %||% character(), tags$li)),
        tags$pre(paste(capture.output(print(trace_view, n = Inf)), collapse = "\n"))
      )
    })
    
    output$help_thread <- renderUI({
      req(user_info())
      help_refresh()
      latest_exchange <- help_state$latest_exchange
      queries <- tryCatch(
        get_user_help_queries(user_info()$user_id),
        error = function(e) {
          message(glue("[help] failed to load help thread: {conditionMessage(e)}"))
          tibble(
            id = integer(),
            user_id = character(),
            topic_id = character(),
            module_id = character(),
            concept_tag = character(),
            query_text = character(),
            response_text = character(),
            error_message = character(),
            ts = character()
          )
        }
      )
      
      build_help_entry <- function(query_text, response, topic_label, timestamp, error_note = NULL) {
        tagList(
          div(
            class = "help-entry user",
            p(strong("You asked")),
            p(query_text),
            p(class = "small-muted", glue("Topic: {topic_label} | {timestamp}"))
          ),
          div(
            class = "help-entry assistant",
            p(strong("Help response")),
            render_help_response_content(response),
            div(
              class = "help-response-meta",
              if (!is.null(error_note) && !is.na(error_note) && nzchar(error_note)) span(glue("Note: {error_note}"))
            )
          )
        )
      }
      
      if (nrow(queries) == 0 && is.null(latest_exchange)) {
        return(div(class = "help-entry assistant", "No help questions yet. Ask about a concept you want clarified."))
      }
      
      rows <- list()
      if (!is.null(latest_exchange)) {
        rows <- append(
          rows,
          list(
            build_help_entry(
              query_text = latest_exchange$query_text %||% "",
              response = normalize_help_response_object(
                latest_exchange$response_object %||% build_unexpected_help_response(latest_exchange$topic_label %||% "General course question"),
                routed_topic_label = latest_exchange$topic_label %||% "General course question"
              ),
              topic_label = latest_exchange$topic_label %||% "General course question",
              timestamp = latest_exchange$timestamp %||% current_utc_timestamp(),
              error_note = latest_exchange$error_note %||% "Saved locally in the session because the database write failed."
            )
          )
        )
      }
      
      if (nrow(queries) > 0) {
        rows <- c(
          rows,
          lapply(seq_len(min(10, nrow(queries))), function(i) {
            topic_id <- queries$topic_id[[i]]
            topic_label <- if (is_valid_topic_id(topic_id, require_known = TRUE)) get_topic_label(topic_id) else "General course question"
            response <- deserialize_help_response_object(
              response_text = queries$response_text[[i]] %||% NA_character_,
              routed_topic_label = topic_label,
              topic_id = topic_id,
              query_text = queries$query_text[[i]]
            )
            error_note <- queries$error_message[[i]] %||% NULL
            
            build_help_entry(
              query_text = queries$query_text[[i]],
              response = response,
              topic_label = topic_label,
              timestamp = queries$ts[[i]],
              error_note = error_note
            )
          })
        )
      }
      
      div(class = "help-thread", do.call(tagList, rows))
    })
    
    output$review_sheet_ui <- renderUI({
      req(user_info())
      attempts_refresh()
      help_refresh()
      weak <- get_weak_concepts(user_info()$user_id)
      
      if (nrow(weak) == 0) {
        return(
          card(
            card_header("My Review Sheet"),
            p("Nothing major on your review sheet right now. Keep practicing.")
          )
        )
      }
      
      weak_bullets <- weak %>%
        rowwise() %>%
        mutate(review_bullets = list(build_review_bullets(cur_data()))) %>%
        ungroup()
      
      grouped <- split(weak_bullets, weak_bullets$module_id)
      
      tagList(
        card(
          card_header("My Review Sheet"),
          p("Short exam reminders based only on the concepts that are currently weak.")
        ),
        div(
          class = "review-sheet-stack",
          do.call(tagList, lapply(grouped, function(group) {
            card(
              class = "review-module-card",
              card_header(group$module_label[[1]]),
              div(
                class = "review-module-stack",
                do.call(tagList, lapply(seq_len(nrow(group)), function(i) {
                  div(
                    class = "review-topic-block",
                    div(class = "review-topic-title", get_concept_label(group$concept_tag[[i]])),
                    if (!identical(get_concept_label(group$concept_tag[[i]]), group$student_label[[i]])) {
                      div(class = "small-muted", group$student_label[[i]])
                    },
                    tags$ul(
                      class = "review-bullet-list",
                      lapply(group$review_bullets[[i]], tags$li)
                    )
                  )
                }))
              )
            )
          }))
        )
      )
    })
    
    output$progress_dashboard_ui <- renderUI({
      req(user_info())
      attempts_refresh()
      help_refresh()
      
      user_id <- user_info()$user_id
      summary <- get_overall_progress_summary(user_id)
      modules <- get_module_progress(user_id) %>%
        mutate(
          status = pmap_chr(list(avg_mastery, avg_accuracy, total_attempts), get_progress_status),
          progress_width = paste0(pmax(0, pmin(100, round(avg_mastery, 0))), "%")
        )
      weak <- get_weak_concepts(user_info()$user_id)
      
      tagList(
        div(
          class = "progress-summary-grid",
          card(
            class = "summary-metric-card",
            div(class = "summary-metric-label", "Overall mastery"),
            div(class = "summary-metric-value", if (is.na(summary$overall_mastery[[1]])) "No data" else format_percent_label(summary$overall_mastery[[1]])),
            div(class = "summary-metric-note", if (is.na(summary$overall_mastery[[1]])) "Complete a few questions to unlock this score." else "Based on topics you have already practiced.")
          ),
          card(
            class = "summary-metric-card",
            div(class = "summary-metric-label", "Weak concepts"),
            div(class = "summary-metric-value", summary$weak_count[[1]]),
            div(class = "summary-metric-note", if (summary$weak_count[[1]] == 0) "No active weak concepts flagged right now." else "Concepts currently needing extra review.")
          ),
          card(
            class = "summary-metric-card",
            div(class = "summary-metric-label", "Attempts completed"),
            div(class = "summary-metric-value", summary$attempts_completed[[1]]),
            div(class = "summary-metric-note", "Each submitted answer updates this dashboard automatically.")
          ),
          card(
            class = "summary-metric-card",
            div(class = "summary-metric-label", "Recommended next"),
            div(class = "summary-metric-value recommended-next", summary$recommended_module[[1]]),
            div(class = "summary-metric-focus", glue("Focus: {summary$recommended_topic[[1]]}")),
            div(class = "summary-metric-reason", glue("Reason: {summary$recommended_reason[[1]]}"))
          )
        ),
        card(
          card_header("Module progress"),
          if (nrow(modules) == 0) {
            div(class = "module-progress-card empty-state-card", "No module progress yet. Start practicing to build this view.")
          } else {
            div(
              class = "progress-module-list",
              do.call(tagList, lapply(seq_len(nrow(modules)), function(i) {
                module <- modules[i, ]
                status_class <- tolower(gsub(" ", "-", module$status[[1]]))
                
                div(
                  class = "module-progress-card",
                  div(
                    class = "module-progress-top",
                    div(
                      div(class = "module-progress-title", module$module_label[[1]]),
                      div(
                        class = "module-progress-meta",
                        span(glue("{module$attempted_topics[[1]]} of {module$topics_in_module[[1]]} topics attempted")),
                        span(glue("{module$total_attempts[[1]]} attempts"))
                      )
                    ),
                    span(class = paste("status-badge", status_class), module$status[[1]])
                  ),
                  div(
                    class = "progress-track",
                    div(class = "progress-fill", style = paste0("width: ", module$progress_width[[1]], ";"))
                  ),
                  div(
                    class = "module-progress-meta",
                    span(glue("Mastery: {round(module$avg_mastery[[1]], 0)}%")),
                    span(if (is.na(module$avg_accuracy[[1]])) "Accuracy: No data yet" else glue("Accuracy: {format_percent_label(module$avg_accuracy[[1]])}"))
                  ),
                  div(
                    class = "module-progress-action",
                    if (identical(module$module_id[[1]], summary$recommended_module_id[[1]])) "Recommended next module" else "Practice this module from the Practice tab"
                  )
                )
              }))
            )
          }
        ),
        card(
          card_header("Weak concepts"),
          if (nrow(weak) == 0) {
            div(
              class = "weak-concept-card empty-state-card",
              p("No weak concepts are currently flagged."),
              p(class = "small-muted", "Keep practicing and this area will surface topics that need review when patterns appear.")
            )
          } else {
            div(
              class = "progress-weak-grid",
              do.call(tagList, lapply(seq_len(min(6, nrow(weak))), function(i) {
                weak_row <- weak[i, ]
                weak_status <- if (weak_row$weakness_score[[1]] >= 6) "Needs review" else "Almost there"
                status_class <- if (weak_status == "Needs review") "needs-review" else "improving"
                
                div(
                  class = "weak-concept-card",
                  div(
                    class = "weak-card-top",
                    div(
                      div(class = "weak-card-title", get_concept_label(weak_row$concept_tag[[1]])),
                      div(class = "small-muted", glue("{weak_row$module_label[[1]]} | {weak_row$student_label[[1]]}"))
                    ),
                    span(class = paste("status-badge", status_class), weak_status)
                  ),
                  div(class = "weak-card-body", p(strong("Why it is weak: "), weak_row$reason[[1]])),
                  div(class = "weak-card-meta", span(glue("Module: {weak_row$module_label[[1]]}")), span(condense_review_text(weak_row$common_mistake[[1]], max_chars = 110))),
                  div(class = "action-pill", weak_row$next_action[[1]])
                )
              }))
            )
          }
        )
      )
    })
  })
}

instructor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$analytics_table <- renderTable({
      summary <- get_instructor_summary()
      
      summary %>%
        transmute(
          Module = module_label,
          Topic = student_label,
          Attempts = attempts,
          Accuracy = round(accuracy, 2),
          Avg_Hints = round(avg_hints, 2),
          Weakness_Index = weakness_index
        )
    })
    
    output$module_summary_table <- renderTable({
      summary <- get_instructor_summary()
      
      summary %>%
        group_by(module_label) %>%
        summarise(
          Attempts = sum(attempts, na.rm = TRUE),
          Accuracy = round(mean(accuracy, na.rm = TRUE), 2),
          Avg_Hints = round(mean(avg_hints, na.rm = TRUE), 2),
          Weakness_Index = round(mean(weakness_index, na.rm = TRUE), 1),
          .groups = "drop"
        )
    })
    
    output$concept_pages_table <- renderTable({
      get_pages_ordered() %>%
        transmute(
          Module = module_label,
          Topic_ID = topic_id,
          Topic = student_label,
          Status = status,
          File = file_name
        )
    })
  })
}

ui <- page_fluid(
  tags$head(tags$style(HTML(stable_css))),
  div(class = "app-shell", uiOutput("root_ui"))
)

server <- function(input, output, session) {
  if (dir.exists("data/visuals")) {
    tryCatch(
      shiny::addResourcePath("local_visuals", normalizePath("data/visuals", winslash = "/", mustWork = TRUE)),
      error = function(e) message(glue("[visuals] local visual resource path not added: {conditionMessage(e)}"))
    )
  }
  init_db()
  
  current_user <- reactiveVal(anonymous_demo_user())
  
  output$root_ui <- renderUI({
    student_ui("student_app")
  })
  
  student_server("student_app", user_info = current_user)
  if (isTRUE(getOption("stat2331.show_admin", FALSE))) {
    instructor_server("instructor_app")
  }
}

shinyApp(ui, server)
