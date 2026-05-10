library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(fs)
library(glue)
library(jsonlite)
library(ellmer)

evidence_index_path <- "data/processed/topic_evidence_index.csv"
topic_map_path <- "data/processed/topic_map.csv"

concept_pages_dir <- "data/wiki/concept_pages"
conflict_queue_path <- "data/processed/conflict_queue.csv"

ensure_concept_pages_dir <- function() {
  dir_create(concept_pages_dir)
}

# -------------------------
# Claude setup
# -------------------------

make_claude_chat <- function() {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  
  if (api_key == "") {
    stop(
      "ANTHROPIC_API_KEY is not set. Add it to .Renviron and restart RStudio.",
      call. = FALSE
    )
  }
  
  # Current explicit Sonnet model name from Anthropic docs.
  ellmer::chat_anthropic(
    model = "claude-sonnet-4-6",
    api_key = api_key
  )
}

# -------------------------
# Helpers
# -------------------------

read_text <- function(path) {
  if (is.na(path) || !file.exists(path)) return("")
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

safe_file_name <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_remove("^_") %>%
    str_remove("_$")
}

trim_chars <- function(x, max_chars = 3500) {
  x <- str_squish(x)
  if (nchar(x) <= max_chars) return(x)
  str_sub(x, 1, max_chars)
}

load_evidence <- function(topic_id) {
  evidence_index <- read_csv(evidence_index_path, show_col_types = FALSE)
  topic_map <- read_csv(topic_map_path, show_col_types = FALSE)
  
  topic_info <- topic_map %>%
    filter(topic_id == !!topic_id)
  
  if (nrow(topic_info) == 0) {
    stop("Unknown topic_id: ", topic_id, call. = FALSE)
  }
  
  evidence <- evidence_index %>%
    filter(topic_id == !!topic_id) %>%
    arrange(priority, source_role, desc(score)) %>%
    mutate(evidence_text = map_chr(evidence_file, read_text))
  
  list(topic_info = topic_info, evidence = evidence)
}

make_evidence_packet <- function(evidence, max_items = 10, max_chars_per_item = 3500) {
  # Keep strongest evidence. Exclude low_priority by default unless there are too few sources.
  main_evidence <- evidence %>%
    filter(source_role != "low_priority") %>%
    arrange(priority, desc(score)) %>%
    slice_head(n = max_items)
  
  if (nrow(main_evidence) < 3) {
    main_evidence <- evidence %>%
      arrange(priority, desc(score)) %>%
      slice_head(n = max_items)
  }
  
  packet <- pmap_chr(
    list(
      main_evidence$source_role,
      main_evidence$source_name,
      main_evidence$doc_type,
      main_evidence$file_name,
      main_evidence$evidence_text
    ),
    function(source_role, source_name, doc_type, file_name, evidence_text) {
      glue(
        "SOURCE ROLE: {source_role}\n",
        "SOURCE NAME: {source_name}\n",
        "DOCUMENT TYPE: {doc_type}\n",
        "FILE: {file_name}\n",
        "EXCERPT:\n",
        "{trim_chars(evidence_text, max_chars_per_item)}\n",
        "\n---\n"
      )
    }
  )
  
  paste(packet, collapse = "\n")
}

extract_json_block <- function(x) {
  # Claude should return JSON only, but this makes parsing a bit more forgiving.
  x <- str_trim(x)
  
  if (str_starts(x, "```")) {
    x <- x %>%
      str_remove("^```json\\s*") %>%
      str_remove("^```\\s*") %>%
      str_remove("\\s*```$")
  }
  
  x
}

# -------------------------
# Prompt
# -------------------------

make_consolidation_prompt <- function(topic_info, evidence_packet) {
  glue(
    "You are helping build a introductory statistics study app.

TASK:
Create a consolidated concept page for the topic below using the provided evidence.

TOPIC ID:
{topic_info$topic_id}

STUDENT-FACING TOPIC LABEL:
{topic_info$student_label}

TOPIC DESCRIPTION:
{topic_info$description}

AUTHORITY RULES:
1. Formula sheets and exam materials are the highest authority for formulas, notation, and exam-facing summaries.
2. Current professor materials are the authority for default explanations, terminology, topic framing, and notation.
3. Dr. South materials are supplemental only. Use them for alternate explanations, extra examples, and common mistakes, but do not let them override current-professor notation.
4. Do not mention professor names or source identities in student-facing text.
5. If supplemental material introduces a topic, notation, or procedure not clearly supported by current materials, flag it as a review issue.
6. Keep the output accurate for introductory statistics students.

OUTPUT FORMAT:
Return valid JSON only. No markdown fences. No commentary outside JSON.

Use this schema:
{{
  \"topic_id\": \"...\",
  \"student_label\": \"...\",
  \"status\": \"draft_needs_review\",
  \"canonical_explanation\": \"...\",
  \"formula_and_notation_notes\": [
    \"...\"
  ],
  \"when_to_use\": [
    \"...\"
  ],
  \"step_by_step_procedure\": [
    \"...\"
  ],
  \"common_mistakes\": [
    \"...\"
  ],
  \"alternative_explanations\": {{
    \"intuitive\": \"...\",
    \"step_by_step\": \"...\",
    \"exam_focused\": \"...\"
  }},
  \"practice_seed_ideas\": [
    {{
      \"format\": \"multiple_choice\",
      \"idea\": \"...\"
    }},
    {{
      \"format\": \"fill_in_blank\",
      \"idea\": \"...\"
    }},
    {{
      \"format\": \"choose_best_answer\",
      \"idea\": \"...\"
    }},
    {{
      \"format\": \"drag_and_drop\",
      \"idea\": \"...\"
    }}
  ],
  \"review_flags\": [
    {{
      \"flag_type\": \"possible_conflict_or_scope_issue\",
      \"severity\": \"low|medium|high\",
      \"description\": \"...\",
      \"suggested_resolution\": \"...\"
    }}
  ],
  \"source_use_summary\": {{
    \"highest_authority_formula_used\": true,
    \"current_authority_used\": true,
    \"supplemental_used\": true,
    \"notes\": \"...\"
  }}
}}

EVIDENCE:
{evidence_packet}
"
  )
}

# -------------------------
# Save outputs
# -------------------------

write_concept_markdown <- function(result) {
  ensure_concept_pages_dir()
  topic_id <- result$topic_id %||% "unknown_topic"
  student_label <- result$student_label %||% topic_id
  
  out_path <- file.path(concept_pages_dir, paste0(safe_file_name(topic_id), ".md"))
  
  formula_notes <- paste0("- ", unlist(result$formula_and_notation_notes), collapse = "\n")
  when_to_use <- paste0("- ", unlist(result$when_to_use), collapse = "\n")
  procedure <- paste0(seq_along(result$step_by_step_procedure), ". ", unlist(result$step_by_step_procedure), collapse = "\n")
  mistakes <- paste0("- ", unlist(result$common_mistakes), collapse = "\n")
  
  practice <- map_chr(result$practice_seed_ideas, function(x) {
    glue("- **{x$format}**: {x$idea}")
  }) %>% paste(collapse = "\n")
  
  alt <- result$alternative_explanations
  
  md <- glue(
    "---
topic_id: {topic_id}
student_label: {student_label}
status: draft_needs_review
---

# {student_label}

## Canonical explanation

{result$canonical_explanation}

## Formula and notation notes

{formula_notes}

## When to use

{when_to_use}

## Step-by-step procedure

{procedure}

## Common mistakes

{mistakes}

## Alternative explanations

### More intuitive

{alt$intuitive}

### Step by step

{alt$step_by_step}

### Exam focused

{alt$exam_focused}

## Practice seed ideas

{practice}

## Source use summary

{result$source_use_summary$notes}
"
  )
  
  writeLines(md, out_path, useBytes = TRUE)
  out_path
}

write_review_flags <- function(result) {
  flags <- result$review_flags
  
  if (length(flags) == 0) {
    return(invisible(NULL))
  }
  
  flags_df <- map_dfr(flags, function(flag) {
    tibble(
      topic_id = result$topic_id,
      student_label = result$student_label,
      flag_type = flag$flag_type %||% NA_character_,
      severity = flag$severity %||% NA_character_,
      description = flag$description %||% NA_character_,
      suggested_resolution = flag$suggested_resolution %||% NA_character_,
      status = "open",
      reviewed_by = NA_character_,
      reviewer_note = NA_character_
    )
  })
  
  if (file.exists(conflict_queue_path)) {
    old <- read_csv(conflict_queue_path, show_col_types = FALSE)
    flags_df <- bind_rows(old, flags_df)
  }
  
  write_csv(flags_df, conflict_queue_path)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

# -------------------------
# Main function
# -------------------------

consolidate_one_topic <- function(topic_id, save_raw_json = TRUE) {
  loaded <- load_evidence(topic_id)
  topic_info <- loaded$topic_info
  evidence <- loaded$evidence
  
  evidence_packet <- make_evidence_packet(evidence)
  
  prompt <- make_consolidation_prompt(topic_info, evidence_packet)
  
  chat <- make_claude_chat()
  
  message("Calling Claude Sonnet for topic: ", topic_id)
  response <- chat$chat(prompt)
  
  json_text <- extract_json_block(response)
  result <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  
  if (save_raw_json) {
    raw_dir <- file.path("data/wiki", "raw_llm_json")
    dir_create(raw_dir)
    writeLines(
      jsonlite::toJSON(result, pretty = TRUE, auto_unbox = TRUE),
      file.path(raw_dir, paste0(safe_file_name(topic_id), ".json")),
      useBytes = TRUE
    )
  }
  
  concept_path <- write_concept_markdown(result)
  write_review_flags(result)
  
  message("Concept page written to: ", concept_path)
  
  invisible(result)
}

# Convenience function, but don't use it until one-topic tests look good.
consolidate_all_topics <- function() {
  topic_map <- read_csv(topic_map_path, show_col_types = FALSE)
  
  results <- list()
  
  for (topic_id in topic_map$topic_id) {
    results[[topic_id]] <- consolidate_one_topic(topic_id)
    Sys.sleep(1)
  }
  
  invisible(results)
}
