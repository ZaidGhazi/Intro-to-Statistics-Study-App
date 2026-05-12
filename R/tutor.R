library(dplyr)
library(glue)
library(purrr)
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
if (!exists("normalize_rag_module_ids", mode = "function") && file.exists("R/retrieval.R")) {
  source("R/retrieval.R")
}
if (!exists("detect_visual_request", mode = "function") && file.exists("R/images.R")) {
  source("R/images.R")
}

RUN_FAITHFULNESS_ON_EVERY_HELP <- tolower(Sys.getenv("INTRO_STATS_FAITHFULNESS_EVERY_HELP", unset = Sys.getenv("STAT2331_FAITHFULNESS_EVERY_HELP", unset = "true"))) %in%
  c("1", "true", "yes", "on")

elapsed_seconds <- function(start_time) {
  round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 3)
}

skipped_faithfulness <- function(reason = "skipped_for_live_latency") {
  list(result = "skipped", score = NA_real_, notes = reason)
}

get_llm_model_config <- function(purpose = c("general", "hint", "practice", "strong")) {
  purpose <- match.arg(purpose)
  list(
    purpose = purpose,
    anthropic_model = switch(
      purpose,
      hint = Sys.getenv("ANTHROPIC_FAST_TUTOR_MODEL", unset = Sys.getenv("ANTHROPIC_MODEL", unset = "claude-haiku-4-5")),
      practice = Sys.getenv("ANTHROPIC_FAST_TUTOR_MODEL", unset = Sys.getenv("ANTHROPIC_MODEL", unset = "claude-haiku-4-5")),
      strong = Sys.getenv("ANTHROPIC_STRONG_MODEL", unset = Sys.getenv("ANTHROPIC_MODEL", unset = "claude-sonnet-4-6")),
      Sys.getenv("ANTHROPIC_MODEL", unset = "claude-haiku-4-5")
    ),
    openai_model = switch(
      purpose,
      hint = Sys.getenv("OPENAI_FAST_TUTOR_MODEL", unset = Sys.getenv("OPENAI_MODEL", unset = "gpt-4.1-mini")),
      practice = Sys.getenv("OPENAI_FAST_TUTOR_MODEL", unset = Sys.getenv("OPENAI_MODEL", unset = "gpt-4.1-mini")),
      strong = Sys.getenv("OPENAI_STRONG_MODEL", unset = Sys.getenv("OPENAI_MODEL", unset = "gpt-4.1")),
      Sys.getenv("OPENAI_MODEL", unset = "gpt-4.1-mini")
    )
  )
}

evidence_has_primary_support <- function(evidence, mode = "general", professor_id = NULL) {
  if (!is.data.frame(evidence) || nrow(evidence) == 0) {
    return(FALSE)
  }
  if (identical(mode, "professor") && !is.null(professor_id) && nzchar(professor_id)) {
    return(any(evidence$source_type %in% c("textbook", "concept_page") |
      (evidence$source_type == "professor_notes" & evidence$professor_id == professor_id)))
  }
  any(evidence$source_type %in% c("textbook", "concept_page"))
}

evidence_confidence <- function(evidence, mode = "general", professor_id = NULL) {
  if (!is.data.frame(evidence) || nrow(evidence) == 0) {
    return("low")
  }
  top_score <- suppressWarnings(max(evidence$final_score, na.rm = TRUE))
  if (is.infinite(top_score) || is.na(top_score)) {
    top_score <- 0
  }
  primary <- evidence_has_primary_support(evidence, mode, professor_id)
  if (primary && top_score >= 7) {
    "high"
  } else if (primary && top_score >= 3) {
    "medium"
  } else {
    "low"
  }
}

clean_evidence_snippet <- function(text, max_chars = 520L) {
  text %>%
    as.character() %>%
    str_replace_all("^---[\\s\\S]*?---\\s*", "") %>%
    str_replace_all("#+\\s*", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish() %>%
    str_sub(1, max_chars)
}

build_grounded_prompt <- function(query, evidence_result, mode = "general", professor_id = NULL) {
  evidence <- evidence_result$evidence %||% tibble()
  evidence_packet <- if (nrow(evidence) == 0) {
    "No evidence retrieved."
  } else {
    pmap_chr(
      list(seq_len(nrow(evidence)), evidence$chunk_id, evidence$module_id, evidence$content_type, evidence$text),
      function(i, chunk_id, module_id, content_type, text) {
        glue("[E{i}] chunk_id={chunk_id}; module_id={module_id}; content_type={content_type}\n{clean_evidence_snippet(text, 900)}")
      }
    ) %>%
      paste(collapse = "\n\n")
  }

  mode_instruction <- if (identical(mode, "professor")) {
    "Professor mode: explain the textbook/course concept first, then use the selected section overlay only for notation, emphasis, or examples."
  } else {
    "General mode: use universal textbook/concept evidence first. Mention notation differences only if the retrieved evidence makes that relevant."
  }

  paste(
    "You are a introductory statistics study tutor.",
    "Answer only from the retrieved evidence below.",
    "If the evidence is insufficient, say the course documents do not give enough support and ask a clarifying question.",
    "Do not reveal professor names, source file names, or private source identities.",
    "Do not simply give final homework, quiz, or test answers; give nudges, setup guidance, and conceptual feedback.",
    "Format the response in clean Markdown with short paragraphs. Use bullets only when they help.",
    "Do not include internal labels such as concept_tag, module_id, source ids, retrieval traces, or chunk ids.",
    mode_instruction,
    glue("Student question: {query}"),
    glue("Active module: {evidence_result$active_module_id %||% 'none'}"),
    glue("Inferred module: {evidence_result$inferred_module_id %||% 'none'}"),
    "Retrieved evidence:",
    evidence_packet,
    "Return a concise Markdown answer. Include a brief connection note if the evidence came from a related module.",
    sep = "\n\n"
  )
}

maybe_refuse_or_clarify <- function(query, evidence_result, mode = "general", professor_id = NULL) {
  evidence <- evidence_result$evidence %||% tibble()
  intent <- evidence_result$intent %||% classify_query_intent(query)

  if (identical(intent, "direct_answer_request")) {
    return(list(
      answer = "I can help you set up the reasoning, but I should not give only the final answer for a graded-style problem. Tell me which step is confusing, or share your setup and I will give feedback.",
      confidence = "medium",
      needs_clarification = TRUE,
      reason = "direct_answer_safety"
    ))
  }

  if (!is.data.frame(evidence) || nrow(evidence) == 0) {
    return(list(
      answer = "I could not find support for that in the local course documents yet. Try adding a keyword, formula, module, or a short excerpt from the problem so I can route it more precisely.",
      confidence = "low",
      needs_clarification = TRUE,
      reason = "no_evidence"
    ))
  }

  top_score <- suppressWarnings(max(evidence$final_score, na.rm = TRUE))
  if (is.infinite(top_score) || is.na(top_score)) {
    top_score <- 0
  }

  if (top_score < 1.5 || !evidence_has_primary_support(evidence, mode, professor_id)) {
    return(list(
      answer = "I found only weak support in the course documents for that wording. Could you add the module, the statistic or formula you are using, or the exact phrase from the problem?",
      confidence = "low",
      needs_clarification = TRUE,
      reason = "weak_primary_support"
    ))
  }

  NULL
}

select_evidence_sentences <- function(query, evidence, max_sentences = 3L) {
  tokens <- tokenize_rag_text(query)
  sentences <- evidence$text %>%
    head(4) %>%
    paste(collapse = " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_split("(?<=[.!?])\\s+") %>%
    unlist(use.names = FALSE) %>%
    str_squish()
  sentences <- sentences[nzchar(sentences)]
  if (length(sentences) == 0) {
    sentences <- clean_evidence_snippet(evidence$text[[1]], 600)
  }

  scored <- tibble(sentence = sentences) %>%
    mutate(
      normalized = normalize_chunk_text(sentence),
      score = map_int(normalized, ~ sum(tokens %in% str_split(.x, "\\s+")[[1]]))
    ) %>%
    arrange(desc(score)) %>%
    slice_head(n = max_sentences)

  scored$sentence
}

module_connection_note <- function(evidence_result) {
  active <- evidence_result$active_module_id
  if (is.null(active) || is.na(active) || !nzchar(active) || !isTRUE(evidence_result$expanded_outside_active)) {
    return("")
  }
  related_used <- evidence_result$evidence %>%
    filter(module_id != !!active, module_id %in% evidence_result$related_modules) %>%
    distinct(module_id) %>%
    pull(module_id)
  if (length(related_used) == 0) {
    return("")
  }
  labels <- get_rag_module_table() %>%
    filter(module_id %in% related_used) %>%
    arrange(module_order) %>%
    pull(module_label)
  glue("This also connects back to {paste(labels, collapse = ', ')}.")
}

extractive_grounded_answer <- function(query, evidence_result, mode = "general") {
  evidence <- evidence_result$evidence
  sentences <- select_evidence_sentences(query, evidence)
  connection <- module_connection_note(evidence_result)
  mode_note <- if (identical(mode, "professor")) {
    "For your selected section, use the course notation emphasized in the retrieved overlay when it differs from the general concept."
  } else {
    ""
  }

  answer <- paste(
    "The course evidence points to this idea:",
    paste(sentences, collapse = " "),
    if (nzchar(connection)) connection else NULL,
    if (nzchar(mode_note)) mode_note else NULL,
    "A good next move is to identify the parameter or statistic first, then match the notation and conditions before doing arithmetic.",
    sep = " "
  )

  str_squish(answer)
}

call_grounded_llm <- function(prompt, model_purpose = c("general", "hint", "practice", "strong")) {
  model_purpose <- match.arg(model_purpose)
  model_config <- get_llm_model_config(model_purpose)
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    return(list(answer = NULL, error = "The ellmer package is not installed."))
  }

  anthropic_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nzchar(anthropic_key)) {
    model <- model_config$anthropic_model
    return(tryCatch(
      {
        chat <- ellmer::chat_anthropic(
          model = model,
          api_key = anthropic_key,
          system_prompt = "You are a grounded course tutor. Follow the user's evidence-only instructions exactly."
        )
        list(answer = chat$chat(prompt), error = NA_character_)
      },
      error = function(e) list(answer = NULL, error = conditionMessage(e))
    ))
  }

  openai_key <- Sys.getenv("OPENAI_API_KEY")
  if (nzchar(openai_key) && "chat_openai" %in% getNamespaceExports("ellmer")) {
    model <- model_config$openai_model
    return(tryCatch(
      {
        chat <- ellmer::chat_openai(
          model = model,
          api_key = openai_key,
          system_prompt = "You are a grounded course tutor. Follow the user's evidence-only instructions exactly."
        )
        list(answer = chat$chat(prompt), error = NA_character_)
      },
      error = function(e) list(answer = NULL, error = conditionMessage(e))
    ))
  }

  list(answer = NULL, error = "No supported LLM API key is configured.")
}

verify_faithfulness <- function(answer, evidence_used) {
  if (!is.data.frame(evidence_used) || nrow(evidence_used) == 0 || !nzchar(answer %||% "")) {
    return(list(result = "fail", score = 0, notes = "Missing answer or evidence."))
  }
  answer_tokens <- tokenize_rag_text(answer)
  evidence_tokens <- tokenize_rag_text(paste(evidence_used$text, collapse = " "))
  if (length(answer_tokens) == 0 || length(evidence_tokens) == 0) {
    return(list(result = "fail", score = 0, notes = "Could not tokenize answer or evidence."))
  }
  overlap <- mean(answer_tokens %in% evidence_tokens)
  list(
    result = if (overlap >= 0.18) "pass" else "fail",
    score = overlap,
    notes = if (overlap >= 0.18) "Answer vocabulary overlaps retrieved evidence." else "Low overlap with retrieved evidence; review manually."
  )
}

generate_grounded_feedback <- function(query,
                                       active_module_id = NULL,
                                       active_module_ids = NULL,
                                       current_module_id = NULL,
                                       mode = "general",
                                       professor_id = NULL,
                                       top_k = 8L,
                                       use_llm = TRUE) {
  mode <- match.arg(mode, c("general", "professor"))
  evidence_result <- retrieve_evidence(
    query = query,
    active_module_id = active_module_id,
    active_module_ids = active_module_ids,
    current_module_id = current_module_id,
    mode = mode,
    professor_id = professor_id,
    top_k = top_k
  )

  refusal <- maybe_refuse_or_clarify(query, evidence_result, mode = mode, professor_id = professor_id)
  if (!is.null(refusal)) {
    faithfulness <- verify_faithfulness(refusal$answer, evidence_result$evidence)
    return(list(
      answer = refusal$answer,
      evidence_used = evidence_result$evidence,
      confidence = refusal$confidence,
      needs_clarification = refusal$needs_clarification,
      hallucination_check = faithfulness$result,
      retrieval_trace = evidence_result$retrieval_trace,
      normalized_query = evidence_result$normalized_query,
      expanded_queries = evidence_result$expanded_queries,
      active_module_id = evidence_result$active_module_id,
      current_module_id = evidence_result$current_module_id,
      active_module_ids = evidence_result$active_module_ids %||% character(),
      inferred_module_id = evidence_result$inferred_module_id,
      expanded_outside_active = evidence_result$expanded_outside_active,
      expanded_outside_selected = evidence_result$expanded_outside_selected,
      llm_error = refusal$reason
    ))
  }

  prompt <- build_grounded_prompt(query, evidence_result, mode = mode, professor_id = professor_id)
  llm_result <- if (isTRUE(use_llm)) call_grounded_llm(prompt) else list(answer = NULL, error = "LLM disabled.")
  answer <- llm_result$answer
  if (is.null(answer) || !nzchar(str_squish(answer))) {
    answer <- extractive_grounded_answer(query, evidence_result, mode = mode)
  }
  answer <- clean_tutor_markdown(answer)
  faithfulness <- verify_faithfulness(answer, evidence_result$evidence)
  confidence <- evidence_confidence(evidence_result$evidence, mode = mode, professor_id = professor_id)
  if (identical(faithfulness$result, "fail") && identical(confidence, "high")) {
    confidence <- "medium"
  }

  list(
    answer = answer,
    evidence_used = evidence_result$evidence,
    confidence = confidence,
    needs_clarification = FALSE,
    hallucination_check = faithfulness$result,
    hallucination_score = faithfulness$score,
    retrieval_trace = evidence_result$retrieval_trace,
    normalized_query = evidence_result$normalized_query,
    expanded_queries = evidence_result$expanded_queries,
    active_module_id = evidence_result$active_module_id,
    current_module_id = evidence_result$current_module_id,
    active_module_ids = evidence_result$active_module_ids %||% character(),
    inferred_module_id = evidence_result$inferred_module_id,
    expanded_outside_active = evidence_result$expanded_outside_active,
    expanded_outside_selected = evidence_result$expanded_outside_selected,
    llm_error = llm_result$error %||% NA_character_
  )
}

generate_practice_feedback <- function(query,
                                       student_answer = NULL,
                                       active_module_id = NULL,
                                       mode = "general",
                                       professor_id = NULL) {
  prompt <- paste(
    query,
    if (!is.null(student_answer)) glue("Student answer: {student_answer}") else NULL,
    "Give feedback as a nudge rather than only the final answer.",
    sep = "\n"
  )
  generate_grounded_feedback(
    query = prompt,
    active_module_id = active_module_id,
    mode = mode,
    professor_id = professor_id
  )
}

practice_value_to_text <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return("")
  }
  if (is.list(x) && !is.data.frame(x)) {
    return(paste(map_chr(x, practice_value_to_text), collapse = "; ") %>% str_squish())
  }
  value <- as.character(x)
  value <- value[!is.na(value) & nzchar(str_squish(value))]
  if (length(value) == 0) {
    return("")
  }
  paste(value, collapse = "; ") %>% str_squish()
}

practice_choices_to_text <- function(answer_choices) {
  if (is.null(answer_choices) || length(answer_choices) == 0 || all(is.na(answer_choices))) {
    return("")
  }
  if (is.character(answer_choices) && is.null(names(answer_choices))) {
    return(paste(answer_choices, collapse = " | ") %>% str_squish())
  }
  if (is.list(answer_choices) && !is.data.frame(answer_choices)) {
    return(map_chr(answer_choices, function(choice) {
      if (is.list(choice)) {
        label <- practice_value_to_text(choice$text %||% choice$label %||% choice$value %||% choice$id)
        id <- practice_value_to_text(choice$id %||% "")
        if (nzchar(id) && nzchar(label)) glue("{id}: {label}") else label
      } else {
        practice_value_to_text(choice)
      }
    }) %>% discard(~ !nzchar(.x)) %>% paste(collapse = " | ") %>% str_squish())
  }
  practice_value_to_text(answer_choices)
}

summarize_recent_tutor_context <- function(conversation_history = list(), max_turns = 6L) {
  if (is.null(conversation_history) || length(conversation_history) == 0) {
    return("")
  }
  recent <- tail(conversation_history, max_turns)
  lines <- purrr::map_chr(recent, function(turn) {
    role <- turn$role %||% "unknown"
    text <- turn$text %||% turn$answer %||% turn$message %||% ""
    text <- stringr::str_squish(as.character(text))
    if (!nzchar(text)) {
      return("")
    }
    paste0(role, ": ", stringr::str_sub(text, 1, 360))
  })
  stringr::str_squish(paste(lines[nzchar(lines)], collapse = "\n"))
}

build_practice_retrieval_query <- function(help_mode = c("hint", "concept", "diagnose", "followup"),
                                           practice_context = list(),
                                           help_question = NULL) {
  help_mode <- match.arg(help_mode)
  answer_submitted <- is_practice_answer_submitted(practice_context)
  recent_context <- summarize_recent_tutor_context(practice_context$conversation_history %||% list(), max_turns = 4L)
  anchor_family <- practice_anchor_family(practice_context)
  anchor_terms <- if (identical(anchor_family, "resistant_measures")) {
    "resistant measures median mean outliers skewed right-skewed measure of center extreme values nonresistant"
  } else {
    ""
  }
  parts <- c(
    glue("Student help mode: {help_mode}"),
    glue("Answer submitted: {answer_submitted}"),
    if (nzchar(practice_context$question_text %||% "")) glue("Practice question: {practice_context$question_text}") else NULL,
    if (nzchar(practice_choices_to_text(practice_context$answer_choices %||% ""))) glue("Answer choices: {practice_choices_to_text(practice_context$answer_choices %||% '')}") else NULL,
    if (nzchar(practice_context$expected_concept_tag %||% "")) glue("Expected concept: {practice_context$expected_concept_tag}") else NULL,
    if (nzchar(practice_context$topic_id %||% "")) glue("Topic: {practice_context$topic_id}") else NULL,
    if (nzchar(practice_context$current_module_id %||% practice_context$active_module_id %||% "")) glue("Current module: {practice_context$current_module_id %||% practice_context$active_module_id}") else NULL,
    if (nzchar(anchor_terms)) glue("Concept anchor terms: {anchor_terms}") else NULL,
    if (nzchar(practice_context$concept_explanation %||% "")) glue("Stored concept explanation: {practice_context$concept_explanation}") else NULL,
    if (nzchar(practice_context$solution_explanation %||% "")) glue("Stored solution explanation: {practice_context$solution_explanation}") else NULL,
    if (nzchar(help_question %||% "")) glue("Student help request: {help_question}") else NULL,
    if (nzchar(practice_context$weak_concept_tag %||% "")) glue("Weak concept: {practice_context$weak_concept_tag}") else NULL,
    if (isTRUE(answer_submitted) && nzchar(practice_context$student_answer %||% "")) glue("Submitted student answer: {practice_context$student_answer}") else NULL,
    if (nzchar(practice_context$last_tutor_answer %||% "")) glue("Previous tutor answer: {practice_context$last_tutor_answer}") else NULL,
    if (nzchar(recent_context)) glue("Recent tutor conversation: {recent_context}") else NULL,
    if (isTRUE(answer_submitted) && nzchar(practice_context$grading_rubric %||% "")) glue("Rubric/explanation: {practice_context$grading_rubric}") else NULL
  )
  str_squish(paste(parts, collapse = "\n"))
}

practice_direct_answer_request <- function(help_question = NULL) {
  q <- normalize_student_query(help_question %||% "")
  str_detect(q, "just give|give me the answer|final answer|answer only|solve it for me|what option is correct|which option is correct|correct option")
}

is_practice_answer_submitted <- function(practice_context = list()) {
  isTRUE(practice_context$answer_submitted %||% practice_context$submitted_attempt %||% FALSE)
}

should_withhold_practice_answer <- function(help_mode,
                                            attempt_count = 0L,
                                            help_question = NULL,
                                            answer_submitted = FALSE) {
  attempts <- suppressWarnings(as.integer(attempt_count %||% 0L))
  if (is.na(attempts)) attempts <- 0L
  identical(help_mode, "hint") ||
    !isTRUE(answer_submitted) ||
    attempts < 2L ||
    isTRUE(practice_direct_answer_request(help_question))
}

strip_tutor_markdown <- function(answer) {
  answer %||% "" %>%
    as.character() %>%
    str_replace_all("\\*\\*([^*]+)\\*\\*", "\\1") %>%
    str_replace_all("\\*([^*]+)\\*", "\\1") %>%
    str_replace_all("`([^`]+)`", "\\1") %>%
    str_replace_all("^\\s*[-*]\\s+", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
}

clean_tutor_markdown <- function(text) {
  if (is.null(text) || length(text) == 0 || all(is.na(text))) {
    return("")
  }
  lines <- paste(as.character(text), collapse = "\n") %>%
    str_replace_all("\\r\\n?", "\n") %>%
    str_split("\n", simplify = FALSE) %>%
    .[[1]]

  lines <- str_replace(
    lines,
    regex("^\\s*Following up on\\s+[A-Za-z0-9_\\-]+:\\s*", ignore_case = TRUE),
    ""
  )
  internal_label <- regex(
    "^\\s*(concept_tag|topic_id|module_id|source_id|source_name|chunk_id|professor_id|retrieval_trace)\\s*:",
    ignore_case = TRUE
  )
  evidence_label <- regex("^\\s*\\[E\\d+\\]\\s*chunk_id\\s*=", ignore_case = TRUE)
  lines <- lines[!str_detect(lines, internal_label) & !str_detect(lines, evidence_label)]

  paste(lines, collapse = "\n") %>%
    str_replace_all(regex("Following up on\\s+[A-Za-z0-9_\\-]+:\\s*", ignore_case = TRUE), "") %>%
    str_replace_all(regex("\\b(on|from|in)\\s+the\\s+concept\\s+page\\b", ignore_case = TRUE), "") %>%
    str_replace_all(regex("\\bconcept\\s+page\\b", ignore_case = TRUE), "course material") %>%
    str_replace_all(regex("\\bretrieved\\s+chunk\\b|\\bsource\\s+chunk\\b|\\bretrieval\\s+evidence\\b", ignore_case = TRUE), "course evidence") %>%
    str_replace_all("\n{3,}", "\n\n") %>%
    str_trim()
}

collapse_tutor_lines <- function(...) {
  lines <- unlist(list(...), use.names = FALSE)
  lines <- lines[!is.na(lines)]
  paste(lines, collapse = "\n")
}

normalize_tutor_message_visuals <- function(visuals = NULL) {
  if (is.null(visuals) || length(visuals) == 0) {
    return(list())
  }
  if (is.data.frame(visuals)) {
    return(lapply(seq_len(nrow(visuals)), function(i) as.list(visuals[i, , drop = FALSE])))
  }
  if (is.list(visuals) && !is.data.frame(visuals)) {
    if (length(visuals) > 0 && !is.null(names(visuals)) && any(names(visuals) %in% c("visual_id", "image_id", "visual_type", "file_path", "src"))) {
      return(list(visuals))
    }
    return(visuals)
  }
  list()
}

create_tutor_message <- function(role,
                                 text,
                                 help_mode = NULL,
                                 visuals = list(),
                                 evidence_used = NULL,
                                 retrieval_trace = NULL,
                                 visual_caption = NULL,
                                 message_id = NULL,
                                 timestamp = NULL) {
  role <- as.character(role %||% "assistant")[[1]]
  message_id <- message_id %||% paste0(
    role,
    "_",
    format(Sys.time(), "%Y%m%d%H%M%OS3"),
    "_",
    sample.int(999999L, 1L)
  )
  message_id <- str_replace_all(message_id, "[^A-Za-z0-9_\\-]", "_")
  raw_text <- as.character(text %||% "")[[1]]
  display_text <- if (role %in% c("assistant", "tutor")) clean_tutor_markdown(raw_text) else str_squish(raw_text)
  message_visuals <- normalize_tutor_message_visuals(visuals)
  visual_ids <- purrr::map_chr(message_visuals, function(visual) {
    as.character(visual$visual_id %||% visual$image_id %||% visual$id %||% "")
  }) %>%
    discard(~ !nzchar(.x))

  list(
    message_id = message_id,
    role = role,
    text = display_text,
    timestamp = timestamp %||% format(Sys.time(), "%I:%M %p"),
    help_mode = help_mode %||% NA_character_,
    visual_ids = visual_ids,
    visuals = message_visuals,
    visual_caption = visual_caption %||% if (length(message_visuals) > 0) message_visuals[[1]]$caption %||% NULL else NULL,
    evidence_used = evidence_used,
    retrieval_trace = retrieval_trace
  )
}

format_tutor_concept_label <- function(concept_tag = NULL) {
  label <- as.character(concept_tag %||% "this idea")[[1]]
  if (is.na(label) || !nzchar(str_squish(label))) {
    return("this idea")
  }
  label %>%
    str_replace_all("_", " ") %>%
    str_replace_all("-", " ") %>%
    str_squish() %>%
    str_to_sentence() %>%
    str_replace_all(regex("\\bp value\\b", ignore_case = TRUE), "p-value") %>%
    str_replace_all(regex("\\bp hat\\b", ignore_case = TRUE), "p-hat")
}

student_safe_concept_summary <- function(practice_context = list(), concept_label = "this idea") {
  question_text <- practice_context$question_text %||% ""
  choices_text <- practice_choices_to_text(practice_context$answer_choices %||% "")
  combined <- str_to_lower(paste(
    concept_label,
    question_text,
    choices_text,
    practice_context$expected_concept_tag %||% "",
    practice_context$weak_concept_tag %||% "",
    practice_context$concept_explanation %||% "",
    practice_context$solution_explanation %||% "",
    collapse = " "
  ))

  if (str_detect(combined, "voluntary response|call in|call-in|self selected|self-selected|sampling bias|biased sample|nonresponse|undercoverage|sampling method")) {
    return("This item is about whether the sample represents the population. A call-in or self-selected sample can be biased because people with strong opinions are more likely to respond, so the sample may not reflect all listeners or all people in the population.")
  }
  if (str_detect(combined, "lurking|confound|third variable|hidden variable|association.*caus|correlation.*caus|causation")) {
    return("A lurking variable is a hidden third variable that can help explain an observed relationship. The key move is to ask whether the two variables are directly connected, or whether another factor could be affecting both.")
  }
  if (str_detect(combined, "middle 50|iqr|interquartile|quartile|q1|q3|five[- ]?number")) {
    return("The middle 50% of the data lies between the first quartile, Q1, and the third quartile, Q3. The spread of that middle half is measured by Q3 minus Q1.")
  }
  if (str_detect(combined, "density curve|area under|under.*curve|normal curve|shaded area|probability.*curve")) {
    return("For a density curve, probability is represented by area under the curve. A proportion between two values corresponds to the area over that interval.")
  }
  if (str_detect(combined, "residual|observed.*predicted|predicted.*observed")) {
    return("A residual is the difference between an observed response and the value predicted by a regression line or model. It is usually described as observed minus predicted.")
  }
  if (str_detect(combined, "resistant|nonresistant|non resistant|sensitive|mean|median|measure of center|skew|outlier|income|extreme value")) {
    return("A measure is resistant if extreme values do not change it very much. The median depends on the middle position, while the mean uses every value and can be pulled toward a long tail or outliers.")
  }
  if (str_detect(combined, "variable|categorical|quantitative|nominal|ordinal|discrete|continuous")) {
    return("First decide what the values represent. If they are group labels, treat the variable as categorical; if they are measured or counted numbers where arithmetic makes sense, treat it as quantitative.")
  }
  if (str_detect(combined, "graph|bar|histogram|boxplot|scatterplot")) {
    return("Match the graph to the variable type. Categories usually call for separated bars, while quantitative values are shown with distribution displays like histograms or boxplots.")
  }
  if (str_detect(combined, "scatterplot|correlation|association|regression|slope|explanatory|response")) {
    return("For two quantitative variables, focus on the direction, form, and strength of the relationship. A pattern can support prediction, but it does not by itself prove cause and effect.")
  }
  if (str_detect(combined, "p_hat|p hat|sample proportion|p_0|hypothesized proportion")) {
    return("Keep sample information separate from the null claim. The sample proportion is computed from the data, while the hypothesized proportion belongs to the null model.")
  }
  if (str_detect(combined, "p-value|p value|hypothesis|reject|null")) {
    return("A p-value is about how unusual the sample result would be if the null hypothesis were true. It is not the probability that the null itself is true.")
  }
  if (str_detect(combined, "confidence|margin of error|interval")) {
    return("A confidence interval uses sample information to estimate a population parameter. The margin of error describes how far the estimate extends on each side.")
  }
  if (str_detect(combined, "z[- ]?score|standard normal|standardize")) {
    return("A z-score measures location in standard-deviation units. It tells how far a value is from the mean and in which direction.")
  }
  if (str_detect(combined, "binomial|success|trial|independent")) {
    return("A binomial setting has a fixed number of trials, two outcomes on each trial, the same probability of success each time, and independent trials.")
  }

  "Use the exact wording of the current question and the answer choices to identify the statistical idea being tested, then connect that idea back to the setup."
}

format_hint_response <- function(concept_label = "this idea",
                                 explanation = NULL,
                                 guiding_question = NULL,
                                 direct_note = NULL) {
  explanation <- explanation %||% glue("Focus on **{concept_label}** before doing any calculation.")
  guiding_question <- guiding_question %||% "What quantity, variable type, or condition should you identify first?"
  clean_tutor_markdown(collapse_tutor_lines(
    if (!is.null(direct_note)) direct_note else NULL,
    "### Hint",
    explanation,
    "",
    "- Name the key quantity in the question.",
    "- Decide whether it comes from the sample, a null claim, a variable type, or a distribution shape.",
    "",
    glue("**Try this:** {guiding_question}")
  ))
}

format_concept_response <- function(concept_label = "this idea",
                                    explanation = NULL,
                                    guiding_question = NULL,
                                    direct_note = NULL,
                                    application_note = NULL,
                                    common_trap = NULL) {
  concept_label <- format_tutor_concept_label(concept_label)
  explanation <- explanation %||% glue("This question is mainly about **{concept_label}**.")
  application_note <- application_note %||% "Look at the wording of the current question and decide which part names the variable, statistic, condition, graph, or distribution shape being tested."
  common_trap <- common_trap %||% "Do not jump straight to the answer choice or final blank. First name the idea the question is testing."
  guiding_question <- guiding_question %||% "Which phrase in the prompt points to this concept?"
  clean_tutor_markdown(collapse_tutor_lines(
    if (!is.null(direct_note)) direct_note else NULL,
    "### The concept",
    explanation,
    "",
    "### How it applies here",
    application_note,
    "",
    "### Common trap",
    common_trap,
    "",
    glue("**Quick check:** {guiding_question}")
  ))
}

format_visual_fallback_response <- function(concept_label = "this idea",
                                            explanation = NULL,
                                            bullets = NULL,
                                            guiding_question = NULL,
                                            direct_note = NULL) {
  concept_label <- format_tutor_concept_label(concept_label)
  explanation <- explanation %||% glue("Imagine a simple visual for **{concept_label}** that highlights the variable, statistic, area, or pattern the question is asking about.")
  bullets <- bullets %||% c(
    "Look for what is being counted, measured, shaded, or compared.",
    "Connect that visual feature back to the wording of the practice question."
  )
  guiding_question <- guiding_question %||% "What would the picture need to label for this question to make sense?"
  clean_tutor_markdown(collapse_tutor_lines(
    if (!is.null(direct_note)) direct_note else NULL,
    "### Picture it this way",
    explanation,
    "",
    paste0("- ", bullets, collapse = "\n"),
    "",
    glue("**Quick check:** {guiding_question}")
  ))
}

practice_anchor_family <- function(practice_context = list()) {
  combined <- str_to_lower(paste(
    practice_context$expected_concept_tag %||% "",
    practice_context$weak_concept_tag %||% "",
    practice_context$question_text %||% "",
    practice_context$concept_explanation %||% "",
    practice_context$solution_explanation %||% "",
    practice_context$grading_rubric %||% "",
    collapse = " "
  ))
  case_when(
    str_detect(combined, "resistant|nonresistant|non resistant|sensitive|mean|median|measure of center|skew|outlier|income|extreme value") ~ "resistant_measures",
    TRUE ~ NA_character_
  )
}

clean_student_help_text <- function(text) {
  clean_tutor_markdown(text %||% "") %>%
    str_replace_all(regex("\\bRecall\\s+which\\s+", ignore_case = TRUE), "Think about which ") %>%
    str_replace_all(regex("\\bdescribed\\s+as\\b", ignore_case = TRUE), "is") %>%
    str_squish()
}

stored_practice_hint <- function(practice_context = list()) {
  hints <- c(
    practice_context$hint_1 %||% "",
    practice_context$hint_2 %||% "",
    practice_context$hint_3 %||% "",
    practice_context$hint %||% "",
    practice_context$hint_ladder %||% character()
  ) %>%
    as.character() %>%
    map_chr(clean_student_help_text) %>%
    discard(~ !nzchar(.x))
  if (length(hints) == 0) {
    return("")
  }
  hints[[1]]
}

build_context_anchored_practice_answer <- function(help_mode,
                                                   practice_context = list(),
                                                   help_question = NULL,
                                                   answer_withheld = TRUE) {
  answer_submitted <- is_practice_answer_submitted(practice_context)
  family <- practice_anchor_family(practice_context)
  concept_label <- format_tutor_concept_label(practice_context$expected_concept_tag %||% practice_context$weak_concept_tag %||% "this concept")
  direct_note <- if (isTRUE(practice_direct_answer_request(help_question))) {
    "I should not give only the final result, but I can help you reason toward it."
  } else {
    NULL
  }

  if (identical(family, "resistant_measures")) {
    if (identical(help_mode, "hint")) {
      hint <- stored_practice_hint(practice_context)
      if (!nzchar(hint) || str_detect(str_to_lower(hint), "concept page")) {
        hint <- "Focus on the phrase **resistant to extreme values**. Which measure of center is based on the middle position instead of using every value?"
      }
      return(list(
        answer = format_hint_response(
          concept_label = "resistant measures",
          explanation = hint,
          guiding_question = "Which measure stays closer to the middle when a few values are extremely large?",
          direct_note = direct_note
        ),
        concept_anchor_used = family,
        stored_content_used = nzchar(stored_practice_hint(practice_context))
      ))
    }

    if (identical(help_mode, "diagnose") && !isTRUE(answer_submitted)) {
      return(list(
        answer = clean_tutor_markdown(collapse_tutor_lines(
          if (!is.null(direct_note)) direct_note else NULL,
          "### Hint before diagnosis",
          "I can diagnose your answer after you submit one. For now, focus on this phrase: **resistant to extreme values**.",
          "",
          "Ask yourself: which measure of center stays closer to the middle when a few values are extremely large?"
        )),
        concept_anchor_used = family,
        stored_content_used = TRUE
      ))
    }

    if (identical(help_mode, "concept")) {
      return(list(
        answer = format_concept_response(
          concept_label = "resistant measures",
          explanation = "A **resistant measure** changes very little when a few values are extremely small or extremely large.",
          application_note = "Here the prompt describes a strongly right-skewed distribution with large outliers. That points to comparing a center based on **middle position** with the **mean**, which uses every value and gets pulled toward the long tail.",
          common_trap = "A common mistake is to choose the mean just because it is a familiar average. For skewed data with outliers, ask which summary resists those extreme values.",
          guiding_question = "Which phrase in the question tells you that outliers should matter?",
          direct_note = direct_note
        ),
        concept_anchor_used = family,
        stored_content_used = nzchar(practice_context$concept_explanation %||% "")
      ))
    }
  }

  # Do not return stored concept explanations directly. They are used as context
  # for the grounded tutor prompt, then checked for answer leakage and faithfulness.

  NULL
}

visual_fallback_for_practice <- function(practice_context = list(), concept_label = "this idea", direct_note = NULL) {
  combined <- str_to_lower(paste(
    concept_label,
    practice_context$question_text %||% "",
    practice_context$expected_concept_tag %||% "",
    practice_context$weak_concept_tag %||% "",
    collapse = " "
  ))

  if (str_detect(combined, "variable|categorical|quantitative|bar|histogram")) {
    return(format_visual_fallback_response(
      concept_label = concept_label,
      explanation = "Imagine a bar chart where each bar is one category, such as Instagram, TikTok, Twitter, or Facebook.",
      bullets = c(
        "A **categorical variable** uses group labels.",
        "A **bar chart** compares counts across groups.",
        "A **histogram** is different because it groups numerical values into intervals."
      ),
      guiding_question = "Are the values names of groups, or measured numbers?",
      direct_note = direct_note
    ))
  }

  if (str_detect(combined, "mean|median|resistant|skew|outlier|income")) {
    return(format_visual_fallback_response(
      concept_label = concept_label,
      explanation = "Picture a right-skewed income distribution with most households clustered together and a few very large values far to the right.",
      bullets = c(
        "The **mean** moves toward the long right tail.",
        "The **median** stays closer to the middle of the main cluster.",
        "That is why the median is more resistant for skewed data with outliers."
      ),
      guiding_question = "Which measure better represents a typical household in that picture?",
      direct_note = direct_note
    ))
  }

  if (str_detect(combined, "p-value|p value|hypothesis|reject|null")) {
    return(format_visual_fallback_response(
      concept_label = concept_label,
      explanation = "Picture the null model as a curve. The p-value is the tail area showing results at least as extreme as the sample result.",
      bullets = c(
        "A **small tail area** means the sample result would be unusual if the null were true.",
        "The p-value is evidence against the null model, not the probability that the null is true."
      ),
      guiding_question = "Would a smaller shaded tail area make you more or less skeptical of the null claim?",
      direct_note = direct_note
    ))
  }

  format_visual_fallback_response(
    concept_label = concept_label,
    explanation = NULL,
    bullets = NULL,
    guiding_question = NULL,
    direct_note = direct_note
  )
}

practice_answer_evaluation_language <- function(answer) {
  str_detect(
    str_to_lower(answer %||% ""),
    "good try|you('?re| are) right|you('?re| are) correct|you('?re| are) wrong|your answer (is|was)|you chose|you selected|that answer (is|was)|incorrect|correct answer"
  )
}


practice_correct_answer_terms <- function(practice_context = list()) {
  terms <- c(
    practice_value_to_text(practice_context$correct_answer %||% ""),
    practice_value_to_text(practice_context$accepted_answers %||% ""),
    practice_value_to_text(practice_context$grading_values %||% "")
  )
  terms <- unlist(str_split(terms, ";|\\||,"), use.names = FALSE)
  terms <- terms %>%
    as.character() %>%
    str_squish() %>%
    discard(~ is.na(.x) || !nzchar(.x)) %>%
    discard(~ nchar(.x) < 3) %>%
    unique()
  # Avoid redacting very broad words that often appear in concept explanations.
  generic <- c("true", "false", "all", "none", "some", "yes", "no")
  terms[!str_to_lower(terms) %in% generic]
}

escape_for_regex <- function(x) {
  stringr::str_replace_all(x, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1")
}

redact_practice_answer_leaks <- function(answer,
                                         practice_context = list(),
                                         answer_withheld = TRUE) {
  answer <- as.character(answer %||% "")[[1]]
  if (!isTRUE(answer_withheld) || !nzchar(answer)) {
    return(list(answer = answer, leaked = FALSE, leaked_terms = character()))
  }

  terms <- practice_correct_answer_terms(practice_context)
  if (length(terms) == 0) {
    return(list(answer = answer, leaked = FALSE, leaked_terms = character()))
  }

  leaked <- character()
  redacted <- answer
  for (term in terms) {
    escaped <- escape_for_regex(term)
    pattern <- if (str_detect(term, "^[A-Za-z0-9_ -]+$")) {
      regex(paste0("\\b", escaped, "\\b"), ignore_case = TRUE)
    } else {
      regex(escaped, ignore_case = TRUE)
    }
    if (str_detect(redacted, pattern)) {
      leaked <- c(leaked, term)
      placeholder <- if (str_detect(term, "\\s")) "the matching idea" else "the key term"
      redacted <- str_replace_all(redacted, pattern, placeholder)
    }
  }

  list(
    answer = redacted,
    leaked = length(leaked) > 0,
    leaked_terms = unique(leaked)
  )
}

build_grounded_guardrail_fallback <- function(help_mode,
                                              practice_context = list(),
                                              evidence_result = list(),
                                              answer_withheld = TRUE,
                                              help_question = NULL,
                                              reason = "grounding_check") {
  evidence <- evidence_result$evidence %||% tibble()
  concept_label <- format_tutor_concept_label(practice_context$expected_concept_tag %||% practice_context$weak_concept_tag %||% "this concept")
  direct_note <- if (isTRUE(practice_direct_answer_request(help_question))) {
    "I should not give only the final result, but I can help you reason toward it."
  } else {
    NULL
  }

  if (identical(help_mode, "hint")) {
    hint <- stored_practice_hint(practice_context)
    if (!nzchar(hint)) {
      hint <- student_safe_concept_summary(practice_context, concept_label)
    }
    return(format_hint_response(
      concept_label = concept_label,
      explanation = hint,
      guiding_question = "What phrase in the question tells you which idea to use?",
      direct_note = direct_note
    ))
  }

  evidence_sentence <- "The retrieved course material was not specific enough for a richer explanation."
  if (is.data.frame(evidence) && nrow(evidence) > 0) {
    evidence_sentence <- select_evidence_sentences(
      practice_context$question_text %||% help_question %||% concept_label,
      evidence,
      max_sentences = 2L
    ) %>% paste(collapse = " ")
  }

  answer <- format_concept_response(
    concept_label = concept_label,
    explanation = str_squish(evidence_sentence),
    application_note = "For this practice item, use that idea to interpret the wording of the question before choosing or typing an answer. I am intentionally not naming the final answer before you submit.",
    common_trap = "A common mistake is to recognize a familiar word and then jump to the answer, instead of checking what the question is specifically asking.",
    guiding_question = "Which phrase in the question should control your setup?",
    direct_note = direct_note
  )

  redaction <- redact_practice_answer_leaks(answer, practice_context, answer_withheld)
  clean_tutor_markdown(redaction$answer)
}

practice_refusal_or_clarification <- function(help_mode,
                                              practice_context,
                                              help_question,
                                              evidence_result) {
  has_question_context <- nzchar(practice_context$question_text %||% "") ||
    nzchar(practice_context$current_question_id %||% "")
  vague_request <- str_detect(
    normalize_student_query(help_question %||% ""),
    "why is this wrong|what did i do wrong|help|hint|explain"
  ) && !has_question_context

  if (isTRUE(vague_request)) {
    return(list(
      answer = "I need the current practice question or your setup before I can diagnose that. Add the question text, your answer, or the module concept and I will give a grounded nudge.",
      confidence = "low",
      needs_clarification = TRUE,
      reason = "missing_practice_context"
    ))
  }

  selected_modules <- normalize_rag_module_ids(practice_context$active_module_ids %||% character())
  inferred_from_help <- route_question_to_module(help_question %||% "", active_module_ids = selected_modules)
  if (!has_question_context &&
      length(selected_modules) > 0 &&
      !is.na(inferred_from_help) &&
      nzchar(inferred_from_help) &&
      !(inferred_from_help %in% selected_modules) &&
      nzchar(str_squish(help_question %||% ""))) {
    return(list(
      answer = "I need a current practice question or a selected module before I can give grounded help. Paste the question text or start a module practice item and I will help without giving away the answer.",
      confidence = "low",
      needs_clarification = TRUE,
      reason = "outside_selected_modules"
    ))
  }

  evidence_refusal <- maybe_refuse_or_clarify(
    help_question %||% practice_context$question_text %||% "",
    evidence_result,
    mode = practice_context$mode %||% "general",
    professor_id = practice_context$professor_id %||% NULL
  )
  if (!is.null(evidence_refusal) && !identical(evidence_refusal$reason, "direct_answer_safety")) {
    return(evidence_refusal)
  }

  NULL
}

build_conversational_tutor_prompt <- function(help_question = NULL,
                                              practice_context = list(),
                                              evidence_result,
                                              visual_metadata = NULL,
                                              answer_withheld = TRUE,
                                              help_mode = "followup") {
  recent_context <- summarize_recent_tutor_context(practice_context$conversation_history %||% list(), max_turns = 8L)
  base_prompt <- build_practice_help_prompt(
    help_mode = help_mode,
    practice_context = practice_context,
    evidence_result = evidence_result,
    visual_metadata = visual_metadata,
    answer_withheld = answer_withheld,
    help_question = help_question
  )
  paste(
    base_prompt,
    "Conversation continuity: treat the student message as part of the same practice-help thread, not as a brand-new standalone Q&A.",
    if (nzchar(recent_context)) glue("Recent conversation window:\n{recent_context}") else "No prior turns are available.",
    "Use the prior answer only for continuity. Course claims still need support from retrieved evidence.",
    sep = "\n\n"
  )
}

supports_multimodal_explanations <- function() {
  enabled <- tolower(Sys.getenv("STAT2331_MULTIMODAL_VISUAL_EXPLANATIONS", unset = "false")) %in%
    c("1", "true", "yes", "on")
  enabled && (nzchar(Sys.getenv("ANTHROPIC_API_KEY")) || nzchar(Sys.getenv("OPENAI_API_KEY")))
}

get_tutor_visual_context <- function(visual_metadata = NULL) {
  if (exists("get_visual_explanation_context", mode = "function")) {
    return(get_visual_explanation_context(visual_metadata))
  }
  if (!is.data.frame(visual_metadata) || nrow(visual_metadata) == 0) {
    return("")
  }
  paste(visual_metadata$caption %||% visual_metadata$vision_description %||% "", collapse = "\n")
}

build_visual_help_prompt <- function(help_question = NULL,
                                     practice_context = list(),
                                     visual_metadata = NULL,
                                     evidence_result = NULL) {
  evidence <- evidence_result$evidence %||% tibble()
  evidence_text <- if (is.data.frame(evidence) && nrow(evidence) > 0) {
    paste(head(clean_evidence_snippet(evidence$text, 320), 3), collapse = "\n")
  } else {
    "No additional evidence text was retrieved."
  }
  paste(
    "You are explaining a visual aid for a introductory statistics practice question.",
    "Use the visual metadata, practice context, and retrieved evidence only.",
    "Do not claim to see details that are not in the metadata unless multimodal image input is explicitly available.",
    "Format the response in clean Markdown with one short heading, 2 to 4 bullets when useful, and one guiding question.",
    "Do not include internal metadata labels, source ids, retrieval traces, or raw concept-page headings.",
    glue("Student request: {help_question %||% 'visual help'}"),
    glue("Current question: {practice_context$question_text %||% 'none'}"),
    glue("Current module: {practice_context$current_module_id %||% practice_context$active_module_id %||% 'none'}"),
    glue("Expected concept: {practice_context$expected_concept_tag %||% 'none'}"),
    "Visual metadata:",
    get_tutor_visual_context(visual_metadata),
    "Retrieved evidence:",
    evidence_text,
    "Explain what the student should notice in the visual, then ask one small guiding question.",
    sep = "\n\n"
  )
}

explain_visual_from_metadata <- function(visual_metadata = NULL,
                                         practice_context = list(),
                                         help_question = NULL) {
  if (!is.data.frame(visual_metadata) || nrow(visual_metadata) == 0) {
    return(format_visual_fallback_response(
      concept_label = practice_context$expected_concept_tag %||% practice_context$weak_concept_tag %||% "this concept",
      explanation = "I do not have a ready visual for this exact concept here, so picture a simple graph that labels the statistic or quantity the question is asking about.",
      bullets = c(
        "Identify what the horizontal axis or categories would represent.",
        "Mark the statistic, area, interval, or pattern that matches the question.",
        "Use the visual to decide what setup comes before any arithmetic."
      ),
      guiding_question = "What would you label first in the picture?"
    ))
  }
  visual <- visual_metadata[1, , drop = FALSE]
  caption <- visual$caption[[1]] %||% "this visual"
  description <- visual$vision_description[[1]] %||% visual$nearby_text[[1]] %||% ""
  concept <- practice_context$expected_concept_tag %||% visual$concept_tag[[1]] %||% "this concept"
  format_visual_fallback_response(
    concept_label = concept,
    explanation = glue("Use the selected visual as a way to think about **{format_tutor_concept_label(concept)}**. The caption is: {caption}."),
    bullets = c(
      if (nzchar(description)) description else "Focus on what the picture labels, shades, or compares.",
      "Connect that visual feature back to the quantity the question asks about."
    ),
    guiding_question = "What part of the visual matches the wording of the question?"
  )
}

explain_visual_with_llm <- function(visual_metadata = NULL,
                                    practice_context = list(),
                                    evidence_result = NULL,
                                    help_question = NULL,
                                    use_llm = TRUE) {
  if (!isTRUE(use_llm)) {
    return(list(answer = explain_visual_from_metadata(visual_metadata, practice_context, help_question), error = "LLM disabled."))
  }
  prompt <- build_visual_help_prompt(
    help_question = help_question,
    practice_context = practice_context,
    visual_metadata = visual_metadata,
    evidence_result = evidence_result
  )
  result <- call_grounded_llm(prompt, model_purpose = "practice")
  if (is.null(result$answer) || !nzchar(str_squish(result$answer))) {
    result$answer <- explain_visual_from_metadata(visual_metadata, practice_context, help_question)
  }
  result
}

generate_visual_explanation <- function(visual_metadata = NULL,
                                        practice_context = list(),
                                        evidence_result = NULL,
                                        help_question = NULL,
                                        use_llm = TRUE) {
  explain_visual_with_llm(
    visual_metadata = visual_metadata,
    practice_context = practice_context,
    evidence_result = evidence_result,
    help_question = help_question,
    use_llm = use_llm
  )
}

build_practice_help_prompt <- function(help_mode = c("hint", "concept", "diagnose", "followup"),
                                       practice_context = list(),
                                       evidence_result,
                                       visual_metadata = NULL,
                                       answer_withheld = TRUE,
                                       help_question = NULL) {
  help_mode <- match.arg(help_mode)
  answer_submitted <- is_practice_answer_submitted(practice_context)
  evidence <- evidence_result$evidence %||% tibble()
  evidence_packet <- if (nrow(evidence) == 0) {
    "No evidence retrieved."
  } else {
    pmap_chr(
      list(seq_len(nrow(evidence)), evidence$chunk_id, evidence$module_id, evidence$content_type, evidence$text),
      function(i, chunk_id, module_id, content_type, text) {
        glue("[E{i}] chunk_id={chunk_id}; module_id={module_id}; content_type={content_type}\n{clean_evidence_snippet(text, 850)}")
      }
    ) %>%
      paste(collapse = "\n\n")
  }

  choices_text <- practice_choices_to_text(practice_context$answer_choices %||% "")
  student_answer_text <- if (isTRUE(answer_submitted)) {
    practice_value_to_text(practice_context$student_answer %||% "")
  } else {
    "[not submitted]"
  }
  rubric_text <- if (isTRUE(answer_submitted)) {
    practice_context$grading_rubric %||% "none"
  } else {
    "[withheld until an answer is submitted]"
  }
  correct_answer_text <- if (isTRUE(answer_withheld)) {
    "[withheld from model instructions for learning safety]"
  } else {
    practice_value_to_text(practice_context$correct_answer %||% "")
  }
  visual_text <- if (nzchar(practice_context$requested_visual_caption %||% "")) {
    glue("Requested visual template: {practice_context$requested_visual_type %||% 'visual'} — {practice_context$requested_visual_caption}")
  } else if (is.data.frame(visual_metadata) && nrow(visual_metadata) > 0) {
    paste(head(visual_metadata$caption %||% visual_metadata$vision_description %||% "", 3), collapse = " | ")
  } else {
    "No visual metadata selected."
  }
  mode_instruction <- switch(
    help_mode,
    hint = paste(
      "Help mode: hint.",
      "The student is asking before submitting an answer unless Answer submitted is TRUE.",
      "If Answer submitted is FALSE, do not evaluate the student's response; the student has not attempted the question yet.",
      "Do not say Good try, You're right, Your answer, You chose, correct, incorrect, or any similar assessment language.",
      "Give one small nudge or guiding question.",
      "Do not reveal the final answer, correct option, or full solution."
    ),
    concept = paste(
      "Help mode: concept.",
      "Explain the relevant concept in simple language using retrieved course evidence.",
      "Use this structure: ### The concept, ### How it applies here, ### Common trap, then one **Quick check** question.",
      "Tie the explanation directly to the current question wording, but do not fill in blanks, name the correct option, or give the final answer before submission.",
      "Do not evaluate the student's answer unless Answer submitted is TRUE.",
      "Do not use generic filler like 'identify the concept first' unless you also explain the actual concept."
    ),
    diagnose = paste(
      "Help mode: diagnose.",
      "Use the student's answer only if Answer submitted is TRUE.",
      "If Answer submitted is FALSE, say you can diagnose after an answer is submitted, then give a hint instead of diagnosis.",
      "Compare the student's answer to the rubric at a conceptual level.",
      "Identify the likely misconception and give the next step.",
      "Do not simply reveal the final answer on early attempts."
    ),
    followup = paste(
      "Help mode: follow-up.",
      "Answer the student's exact typed question first, then connect it back to the current practice question.",
      "Use the current question text and answer choices as context. If the student asks about an answer choice or a term inside an answer choice, define or explain that term without saying whether it is correct.",
      "Do not assume the student attempted the question unless Answer submitted is TRUE.",
      "If the follow-up is vague but practice context exists, connect it to that context.",
      "If the follow-up asks for a visual, explain what the displayed visual helps the student notice in this specific question. Do not provide generic graph advice; tie the explanation to the current question and answer choices."
    )
  )

  paste(
    "You are the embedded introductory statistics practice tutor.",
    "Use the retrieved evidence and the current practice context below. The current question text and answer choices are valid context for tutoring.",
    "The current practice question context is the priority. If retrieved evidence is broad or conflicting, prefer the current question wording and answer choices.",
    "If the student asks about a term in the current question or answer choices, explain that term directly and then connect it back to the current question without saying whether it is the correct answer.",
    "Do not expose source file names, professor names, private notes, or copyrighted excerpts beyond short grounded paraphrases.",
    "Format the response in clean Markdown.",
    "Use short paragraphs. Use bullets only when they help. Use bold for key terms.",
    "Do not dump raw concept-page sections into the answer.",
    "Do not include internal labels such as concept_tag, topic_id, module_id, source ids, retrieval traces, or chunk ids.",
    "Do not start with phrases like 'Following up on variable_classification'.",
    "For normal tutor help, prefer concise teaching with clear headings and one guiding follow-up question.",
    "Before the student submits, never fill in a blank, identify the correct answer choice, or state 'the answer is ...'.",
    "If you are not sure the retrieved evidence supports a claim, say the evidence is not strong enough and ask a clarifying question.",
    "Do not show any developer/debug labels, internal routing language, or module-switch prompts to the student.",
    "If relevant visual metadata or a requested visual template is provided, explain what the displayed visual helps the student notice in this specific question rather than saying you do not have access to diagrams.",
    mode_instruction,
    if (isTRUE(practice_direct_answer_request(help_question))) {
      "The student asked for only the answer. Redirect to a guided explanation and do not give only the final answer."
    } else {
      NULL
    },
    glue("Student help request: {help_question %||% 'none'}"),
    glue("Current question module: {practice_context$current_module_id %||% practice_context$active_module_id %||% evidence_result$current_module_id %||% evidence_result$active_module_id %||% 'none'}"),
    glue("Selected practice modules: {paste(practice_context$active_module_ids %||% evidence_result$active_module_ids %||% character(), collapse = ', ')}"),
    glue("Question ID: {practice_context$current_question_id %||% 'none'}"),
    glue("Question text: {practice_context$question_text %||% 'none'}"),
    glue("Answer choices: {choices_text %||% 'none'}"),
    glue("Answer submitted: {answer_submitted}"),
    glue("Student answer: {student_answer_text}"),
    glue("Attempt count: {practice_context$attempt_count %||% 0}"),
    glue("Expected concept: {practice_context$expected_concept_tag %||% 'none'}"),
    glue("Weak concept: {practice_context$weak_concept_tag %||% 'none'}"),
    glue("Correct answer: {correct_answer_text}"),
    glue("Rubric/explanation: {rubric_text}"),
    glue("Relevant visual metadata: {visual_text}"),
    "Retrieved evidence:",
    evidence_packet,
    "Return concise student-facing Markdown. Prefer a guiding question for hints. If the evidence is weak but the current question context is clear, still give a contextual explanation from the question and answer choices rather than a generic setup reminder.",
    sep = "\n\n"
  )
}

practice_help_fallback_answer <- function(help_mode,
                                          practice_context,
                                          evidence_result,
                                          answer_withheld = TRUE,
                                          help_question = NULL) {
  answer_submitted <- is_practice_answer_submitted(practice_context)
  anchored <- build_context_anchored_practice_answer(
    help_mode = help_mode,
    practice_context = practice_context,
    help_question = help_question,
    answer_withheld = answer_withheld
  )
  if (!is.null(anchored)) {
    return(anchored$answer)
  }
  evidence <- evidence_result$evidence %||% tibble()
  if (!is.data.frame(evidence) || nrow(evidence) == 0) {
    return(clean_tutor_markdown(paste(
      "### I need one more clue",
      "",
      "I cannot find enough support for this in the course documents yet.",
      "",
      "- Add the formula, module keyword, or a short part of your setup.",
      "- Then I can give a grounded nudge tied to the course materials.",
      "",
      "**Try this:** What phrase from the problem statement seems most important?",
      sep = "\n"
    )))
  }
  concept_raw <- practice_context$expected_concept_tag %||% practice_context$weak_concept_tag %||% "this concept"
  concept_label <- format_tutor_concept_label(concept_raw)
  direct_note <- if (isTRUE(practice_direct_answer_request(help_question))) {
    "I should not give only the final result, but I can help you reason toward it."
  } else {
    NULL
  }
  visual_requested <- str_detect(normalize_student_query(help_question %||% ""), "visual|visually|graph|plot|diagram|picture|draw|show")
  if (isTRUE(visual_requested)) {
    return(visual_fallback_for_practice(practice_context, concept_label, direct_note = direct_note))
  }

  if (identical(help_mode, "hint") && !isTRUE(answer_submitted)) {
    return(format_hint_response(
      concept_label = concept_label,
      explanation = glue("Focus on **{concept_label}**. {student_safe_concept_summary(practice_context, concept_label)}"),
      guiding_question = "What quantity, variable type, graph choice, or condition should you identify before doing any calculation?",
      direct_note = direct_note
    ))
  }

  concept_summary <- student_safe_concept_summary(practice_context, concept_label)

  answer <- switch(
    help_mode,
    hint = format_hint_response(
      concept_label = concept_label,
      explanation = glue("Focus on **{concept_label}**. {concept_summary}"),
      guiding_question = "What quantity is the problem asking you to identify before doing any calculation?",
      direct_note = direct_note
    ),
    concept = format_concept_response(
      concept_label = concept_label,
      explanation = concept_summary,
      guiding_question = "Which word or phrase in the question tells you this is the concept being tested?",
      direct_note = direct_note
    ),
    diagnose = clean_tutor_markdown(collapse_tutor_lines(
      if (!is.null(direct_note)) direct_note else NULL,
      if (isTRUE(answer_submitted) && nzchar(practice_context$student_answer %||% "")) {
        "### Diagnosis"
      } else {
        "### Hint before diagnosis"
      },
      if (isTRUE(answer_submitted) && nzchar(practice_context$student_answer %||% "")) {
        glue("I am using your submitted answer to look for the misconception. The likely issue is with **{concept_label}**.")
      } else {
        "I can diagnose your answer after you submit one. For now, here is a hint."
      },
      "",
      glue("- {concept_summary}"),
      "- Compare your setup with the exact quantity named in the question.",
      if (isTRUE(answer_submitted)) "- Revise one step before jumping to a final result." else "- Identify the quantity or variable type before calculating.",
      "",
      "**Next step:** What part of your setup should change first?"
    )),
    followup = format_concept_response(
      concept_label = concept_label,
      explanation = paste(
        concept_summary,
        if (nzchar(practice_context$last_tutor_answer %||% "")) {
          "Said more simply, connect the previous hint to the exact wording in this question."
        } else {
          "Use this idea to connect your follow-up back to the current practice item."
        }
      ),
      guiding_question = "What part of the setup becomes clearer if you name the concept first?",
      direct_note = direct_note
    )
  )
  if (!isTRUE(answer_withheld) && nzchar(practice_value_to_text(practice_context$correct_answer %||% ""))) {
    answer <- paste(answer, "\n\nAfter your attempts, a fuller explanation can connect this reasoning to the recorded answer.")
  }
  clean_tutor_markdown(answer)
}

generate_contextual_practice_help <- function(help_mode = c("hint", "concept", "diagnose", "followup"),
                                              practice_context = list(),
                                              help_question = NULL,
                                              active_module_id = NULL,
                                              active_module_ids = NULL,
                                              current_module_id = NULL,
                                              mode = NULL,
                                              professor_id = NULL,
                                              top_k = 8L,
                                              use_llm = TRUE,
                                              evidence_result = NULL,
                                              visual_metadata = NULL,
                                              run_faithfulness = isTRUE(getOption("stat2331.run_faithfulness_on_every_help", RUN_FAITHFULNESS_ON_EVERY_HELP))) {
  total_start <- Sys.time()
  retrieval_time <- 0
  visual_time <- 0
  generation_time <- 0
  verifier_time <- 0
  llm_calls_count <- 0L
  help_mode <- match.arg(help_mode)
  mode <- mode %||% practice_context$mode %||% "general"
  mode <- if (mode %in% c("general", "professor")) mode else "general"
  professor_id <- professor_id %||% practice_context$professor_id %||% NULL
  selected_module_ids <- normalize_rag_module_ids(active_module_ids %||% practice_context$active_module_ids %||% character())
  current_module_id <- normalize_rag_module_id(
    current_module_id %||% active_module_id %||% practice_context$current_module_id %||% practice_context$active_module_id %||% NULL,
    query = practice_context$question_text %||% help_question %||% ""
  )
  active_module_id <- current_module_id
  answer_submitted <- is_practice_answer_submitted(practice_context)
  retrieval_query <- build_practice_retrieval_query(
    help_mode = help_mode,
    practice_context = practice_context,
    help_question = help_question
  )
  if (!nzchar(retrieval_query)) {
    retrieval_query <- help_question %||% practice_context$question_text %||% ""
  }

  help_inferred_module_id <- route_question_to_module(help_question %||% "", active_module_ids = selected_module_ids)
  help_targets_other_selected_module <- !is.na(help_inferred_module_id) &&
    nzchar(help_inferred_module_id) &&
    help_inferred_module_id %in% selected_module_ids &&
    !identical(help_inferred_module_id, current_module_id %||% "")
  retrieval_module_id <- if (isTRUE(help_targets_other_selected_module)) help_inferred_module_id else current_module_id
  if (isTRUE(help_targets_other_selected_module)) {
    retrieval_query <- paste(
      glue("Student help request: {help_question %||% ''}"),
      glue("Route this follow-up to selected module: {help_inferred_module_id}"),
      glue("Current practice question context: {practice_context$question_text %||% ''}"),
      sep = "\n"
    ) %>% str_squish()
    evidence_result <- NULL
    visual_metadata <- NULL
  }

  used_cached_evidence <- is.list(evidence_result) &&
    is.data.frame(evidence_result$evidence %||% NULL) &&
    !is.null(evidence_result$current_question_id %||% practice_context$current_question_id %||% NULL)

  if (!is.list(evidence_result) || is.null(evidence_result$evidence)) {
    retrieval_start <- Sys.time()
    evidence_result <- retrieve_evidence(
      query = retrieval_query,
      active_module_id = retrieval_module_id,
      active_module_ids = selected_module_ids,
      current_module_id = retrieval_module_id,
      mode = mode,
      professor_id = professor_id,
      top_k = top_k,
      expected_concept_tag = practice_context$expected_concept_tag %||% practice_context$weak_concept_tag %||% NULL
    )
    retrieval_time <- elapsed_seconds(retrieval_start)
    used_cached_evidence <- FALSE
  }

  visuals <- visual_metadata
  if (!is.data.frame(visuals)) {
    visual_start <- Sys.time()
    visuals <- if (exists("retrieve_relevant_visuals", mode = "function")) {
      tryCatch(
        retrieve_relevant_visuals(
          query = retrieval_query,
          concept_tag = practice_context$expected_concept_tag %||% practice_context$weak_concept_tag %||% NULL,
          module_id = retrieval_module_id,
          active_module_id = retrieval_module_id,
          top_k = 3L
        ),
        error = function(e) tibble()
      )
    } else {
      tibble()
    }
    visual_time <- elapsed_seconds(visual_start)
  } else {
    visual_time <- 0
  }

  answer_withheld <- should_withhold_practice_answer(
    help_mode = help_mode,
    attempt_count = practice_context$attempt_count %||% 0L,
    help_question = help_question,
    answer_submitted = answer_submitted
  )

  anchored_answer <- build_context_anchored_practice_answer(
    help_mode = help_mode,
    practice_context = practice_context,
    help_question = help_question,
    answer_withheld = answer_withheld
  )
  if (!is.null(anchored_answer)) {
    anchored_text <- clean_tutor_markdown(anchored_answer$answer)
    leak_check <- redact_practice_answer_leaks(
      answer = anchored_text,
      practice_context = practice_context,
      answer_withheld = answer_withheld
    )
    anchored_text <- clean_tutor_markdown(leak_check$answer)
    verifier_start <- Sys.time()
    faithfulness <- if (isTRUE(run_faithfulness)) verify_faithfulness(anchored_text, evidence_result$evidence) else skipped_faithfulness("practice_context_anchor")
    verifier_time <- elapsed_seconds(verifier_start)
    return(list(
      answer = anchored_text,
      help_mode = help_mode,
      retrieval_query = retrieval_query,
      evidence_used = evidence_result$evidence,
      visuals_used = visuals,
      confidence = if (isTRUE(anchored_answer$stored_content_used)) "medium" else evidence_confidence(evidence_result$evidence, mode = mode, professor_id = professor_id),
      needs_clarification = FALSE,
      hallucination_check = faithfulness$result,
      hallucination_score = faithfulness$score,
      retrieval_trace = evidence_result$retrieval_trace,
      normalized_query = evidence_result$normalized_query,
      expanded_queries = evidence_result$expanded_queries,
      active_module_id = evidence_result$active_module_id,
      current_module_id = evidence_result$current_module_id %||% current_module_id,
      active_module_ids = evidence_result$active_module_ids %||% selected_module_ids,
      inferred_module_id = evidence_result$inferred_module_id,
      expanded_outside_active = evidence_result$expanded_outside_active,
      expanded_outside_selected = evidence_result$expanded_outside_selected,
      answer_submitted = answer_submitted,
      answer_withheld = answer_withheld,
      current_question_id = practice_context$current_question_id %||% NA_character_,
      expected_concept_tag = practice_context$expected_concept_tag %||% NA_character_,
      llm_error = "practice_context_anchor",
      used_cached_evidence = used_cached_evidence,
      llm_calls_count = llm_calls_count,
      retrieval_time = retrieval_time,
      rerank_time = evidence_result$rerank_time %||% NA_real_,
      generation_time = 0,
      verifier_time = verifier_time,
      total_time = elapsed_seconds(total_start),
      stored_content_used = isTRUE(anchored_answer$stored_content_used),
      concept_anchor_used = anchored_answer$concept_anchor_used %||% NA_character_,
      concept_mismatch_guardrail = TRUE
    ))
  }

  refusal <- practice_refusal_or_clarification(
    help_mode = help_mode,
    practice_context = practice_context,
    help_question = help_question,
    evidence_result = evidence_result
  )
  if (!is.null(refusal)) {
    refusal$answer <- clean_tutor_markdown(refusal$answer)
    verifier_start <- Sys.time()
    faithfulness <- if (isTRUE(run_faithfulness)) verify_faithfulness(refusal$answer, evidence_result$evidence) else skipped_faithfulness()
    verifier_time <- elapsed_seconds(verifier_start)
    return(list(
      answer = refusal$answer,
      help_mode = help_mode,
      retrieval_query = retrieval_query,
      evidence_used = evidence_result$evidence,
      visuals_used = visuals,
      confidence = refusal$confidence,
      needs_clarification = refusal$needs_clarification,
      hallucination_check = faithfulness$result,
      hallucination_score = faithfulness$score,
      retrieval_trace = evidence_result$retrieval_trace,
      normalized_query = evidence_result$normalized_query,
      expanded_queries = evidence_result$expanded_queries,
      active_module_id = evidence_result$active_module_id,
      current_module_id = evidence_result$current_module_id %||% current_module_id,
      active_module_ids = evidence_result$active_module_ids %||% selected_module_ids,
      inferred_module_id = evidence_result$inferred_module_id,
      expanded_outside_active = evidence_result$expanded_outside_active,
      expanded_outside_selected = evidence_result$expanded_outside_selected,
      answer_submitted = answer_submitted,
      answer_withheld = answer_withheld,
      current_question_id = practice_context$current_question_id %||% NA_character_,
      expected_concept_tag = practice_context$expected_concept_tag %||% NA_character_,
      llm_error = refusal$reason,
      used_cached_evidence = used_cached_evidence,
      llm_calls_count = llm_calls_count,
      retrieval_time = retrieval_time,
      rerank_time = evidence_result$rerank_time %||% NA_real_,
      generation_time = generation_time,
      verifier_time = verifier_time,
      total_time = elapsed_seconds(total_start)
    ))
  }

  prompt <- if (identical(help_mode, "followup")) {
    build_conversational_tutor_prompt(
      help_question = help_question,
      practice_context = practice_context,
      evidence_result = evidence_result,
      visual_metadata = visuals,
      answer_withheld = answer_withheld,
      help_mode = help_mode
    )
  } else {
    build_practice_help_prompt(
      help_mode = help_mode,
      practice_context = practice_context,
      evidence_result = evidence_result,
      visual_metadata = visuals,
      answer_withheld = answer_withheld,
      help_question = help_question
    )
  }
  generation_start <- Sys.time()
  llm_result <- if (isTRUE(use_llm)) {
    llm_calls_count <- 1L
    call_grounded_llm(prompt, model_purpose = if (identical(help_mode, "hint")) "hint" else "practice")
  } else {
    list(answer = NULL, error = "LLM disabled.")
  }
  generation_time <- elapsed_seconds(generation_start)
  answer <- llm_result$answer
  if (is.null(answer) || !nzchar(str_squish(answer))) {
    answer <- practice_help_fallback_answer(
      help_mode = help_mode,
      practice_context = practice_context,
      evidence_result = evidence_result,
      answer_withheld = answer_withheld,
      help_question = help_question
    )
  }
  answer <- clean_tutor_markdown(answer)
  visual_requested <- if (exists("detect_visual_request", mode = "function")) {
    detect_visual_request(help_question %||% retrieval_query)
  } else {
    str_detect(normalize_student_query(help_question %||% retrieval_query), "visual|graph|plot|chart|diagram|figure|draw")
  }
  if (isTRUE(visual_requested) && is.data.frame(visuals) && nrow(visuals) > 0 &&
      !str_detect(str_to_lower(answer), "visual|graph|chart|plot|curve|picture")) {
    visual_note <- generate_visual_explanation(
      visual_metadata = visuals,
      practice_context = practice_context,
      evidence_result = evidence_result,
      help_question = help_question,
      use_llm = FALSE
    )$answer
    answer <- clean_tutor_markdown(paste(answer, visual_note, sep = "\n\n"))
  }
  if (!isTRUE(answer_submitted) &&
      help_mode %in% c("hint", "diagnose", "concept", "followup") &&
      practice_answer_evaluation_language(answer)) {
    answer <- practice_help_fallback_answer(
      help_mode = help_mode,
      practice_context = practice_context,
      evidence_result = evidence_result,
      answer_withheld = answer_withheld,
      help_question = help_question
    ) %>%
      clean_tutor_markdown()
    llm_result$error <- "pre_submit_evaluation_language_removed"
  }
  leak_check <- redact_practice_answer_leaks(
    answer = answer,
    practice_context = practice_context,
    answer_withheld = answer_withheld
  )
  if (isTRUE(leak_check$leaked)) {
    answer <- clean_tutor_markdown(leak_check$answer)
    llm_result$error <- paste(c(llm_result$error %||% NA_character_, "answer_leak_redacted"), collapse = "; ")
  }

  verifier_start <- Sys.time()
  faithfulness <- if (isTRUE(run_faithfulness)) verify_faithfulness(answer, evidence_result$evidence) else skipped_faithfulness()
  verifier_time <- elapsed_seconds(verifier_start)
  confidence <- evidence_confidence(evidence_result$evidence, mode = mode, professor_id = professor_id)
  if (isTRUE(run_faithfulness) && identical(faithfulness$result, "fail")) {
    answer <- build_grounded_guardrail_fallback(
      help_mode = help_mode,
      practice_context = practice_context,
      evidence_result = evidence_result,
      answer_withheld = answer_withheld,
      help_question = help_question,
      reason = "faithfulness_fail"
    )
    leak_check <- redact_practice_answer_leaks(answer, practice_context, answer_withheld)
    answer <- clean_tutor_markdown(leak_check$answer)
    faithfulness <- verify_faithfulness(answer, evidence_result$evidence)
    verifier_time <- elapsed_seconds(verifier_start)
    llm_result$error <- paste(c(llm_result$error %||% NA_character_, "faithfulness_guardrail_fallback"), collapse = "; ")
  }
  if (identical(faithfulness$result, "fail") && identical(confidence, "high")) {
    confidence <- "medium"
  }

  list(
    answer = answer,
    help_mode = help_mode,
    retrieval_query = retrieval_query,
    evidence_used = evidence_result$evidence,
    visuals_used = visuals,
    confidence = confidence,
    needs_clarification = FALSE,
    hallucination_check = faithfulness$result,
    hallucination_score = faithfulness$score,
    retrieval_trace = evidence_result$retrieval_trace,
    normalized_query = evidence_result$normalized_query,
    expanded_queries = evidence_result$expanded_queries,
    active_module_id = evidence_result$active_module_id,
    current_module_id = evidence_result$current_module_id %||% current_module_id,
    active_module_ids = evidence_result$active_module_ids %||% selected_module_ids,
    inferred_module_id = evidence_result$inferred_module_id,
    expanded_outside_active = evidence_result$expanded_outside_active,
    expanded_outside_selected = evidence_result$expanded_outside_selected,
    answer_submitted = answer_submitted,
    answer_withheld = answer_withheld,
    current_question_id = practice_context$current_question_id %||% NA_character_,
    expected_concept_tag = practice_context$expected_concept_tag %||% NA_character_,
    llm_error = llm_result$error %||% NA_character_,
    used_cached_evidence = used_cached_evidence,
    llm_calls_count = llm_calls_count,
    retrieval_time = retrieval_time,
    rerank_time = evidence_result$rerank_time %||% NA_real_,
    generation_time = generation_time,
    verifier_time = verifier_time,
    total_time = elapsed_seconds(total_start)
  )
}

generate_followup_response <- function(help_question,
                                       practice_context = list(),
                                       active_module_id = NULL,
                                       active_module_ids = NULL,
                                       current_module_id = NULL,
                                       mode = NULL,
                                       professor_id = NULL,
                                       top_k = 8L,
                                       use_llm = TRUE) {
  generate_contextual_practice_help(
    help_mode = "followup",
    practice_context = practice_context,
    help_question = help_question,
    active_module_id = active_module_id,
    active_module_ids = active_module_ids,
    current_module_id = current_module_id,
    mode = mode,
    professor_id = professor_id,
    top_k = top_k,
    use_llm = use_llm
  )
}

generate_general_grounded_answer <- function(query,
                                             active_module_id = NULL,
                                             active_module_ids = NULL,
                                             current_module_id = NULL,
                                             mode = "general",
                                             professor_id = NULL,
                                             top_k = 8L,
                                             use_llm = TRUE) {
  generate_grounded_feedback(
    query = query,
    active_module_id = active_module_id,
    active_module_ids = active_module_ids,
    current_module_id = current_module_id,
    mode = mode,
    professor_id = professor_id,
    top_k = top_k,
    use_llm = use_llm
  )
}

grounded_feedback_to_help_response <- function(feedback, topic_label = "Course-grounded help") {
  answer <- feedback$answer %||% "I could not find enough grounded evidence to answer confidently."
  evidence <- feedback$evidence_used %||% tibble()
  module_label_for <- function(module_id) {
    row <- get_rag_module_table() %>% filter(module_id == !!module_id) %>% slice_head(n = 1)
    if (nrow(row) == 0) module_id else row$module_label[[1]]
  }
  remember <- c(
    if (!is.null(feedback$active_module_id) && nzchar(feedback$active_module_id %||% "")) glue("Active module: {module_label_for(feedback$active_module_id)}") else NULL,
    if (!is.null(feedback$inferred_module_id) && nzchar(feedback$inferred_module_id %||% "")) glue("Question routed toward: {module_label_for(feedback$inferred_module_id)}") else NULL,
    if (nrow(evidence) > 0) glue("Course evidence checked: {min(nrow(evidence), 8)} relevant chunk(s).") else "No strong course evidence was found."
  )

  list(
    routed_topic_label = topic_label,
    direct_answer = answer,
    remember_bullets = remember,
    analogy = if (isTRUE(feedback$expanded_outside_active)) "This topic is linked across modules, so the app used nearby course context after checking the active module." else "Think of this as matching the question to the closest course evidence before explaining it.",
    common_mistake = "Do not switch notation or methods before checking what parameter, statistic, and module the question is using.",
    next_step = if (isTRUE(feedback$needs_clarification)) "Add one keyword, formula, or problem phrase so the tutor can retrieve stronger evidence." else "Try a practice item in the same module to use the idea right away."
  )
}
