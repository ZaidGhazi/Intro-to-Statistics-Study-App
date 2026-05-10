library(fs)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(glue)
library(jsonlite)

manifest_path <- "data/processed/source_manifest.csv"
wiki_dir <- "data/wiki"
evidence_dir <- file.path("data/processed", "topic_evidence")
topic_map_path <- file.path("data/processed", "topic_map.csv")
evidence_index_path <- file.path("data/processed", "topic_evidence_index.csv")
topic_overrides_path <- file.path("data/processed", "topic_overrides.csv")

ensure_consolidation_dirs <- function() {
  dir_create(wiki_dir)
  dir_create(evidence_dir)
}

# -------------------------
# 1. Official student-facing topic map
# -------------------------

topic_map <- tibble::tribble(
  ~topic_id, ~roadmap_order, ~student_label, ~description, ~keywords,
  
  "data_graphs", 1, "Data Types, Variables, and Graphs",
  "Identify variable types, summarize data, and choose appropriate displays.",
  "data|variable|qualitative|quantitative|categorical|nominal|ordinal|discrete|continuous|bar chart|histogram|boxplot|pie chart|stemplot|frequency|relative frequency|distribution|skewed|symmetric|outlier|sampling frame|exploratory data analysis",
  
  "descriptive_stats", 2, "Descriptive Statistics",
  "Use center, spread, position, and outlier rules to describe data.",
  "mean|median|standard deviation|variance|range|interquartile|IQR|quartile|Q1|Q3|five-number|z-score|percentile|center|spread|outlier|minimum|maximum",
  
  "relationships_regression", 3, "Relationships, Association, and Regression",
  "Describe relationships between variables using scatterplots, correlation, and regression.",
  "scatterplot|association|correlation|regression|linear model|slope|intercept|least squares|residual|r-squared|coefficient|predictor|response|y-hat|ŷ",
  
  "producing_data", 4, "Producing Data: Sampling and Experiments",
  "Understand sampling design, experiments, observational studies, bias, and random assignment.",
  "sampling design|simple random sample|stratified|cluster|systematic|bias|experiment|observational study|random assignment|control group|treatment|placebo|blinding|confounding|replication",
  
  "probability_basics", 5, "Probability Basics",
  "Work with probability rules, randomness, events, independence, and conditional probability.",
  "probability|random|event|sample space|independent|disjoint|mutually exclusive|complement|conditional probability|general addition rule|multiplication rule|tree diagram|law of large numbers",
  
  "normal_dist", 6, "Normal Distributions",
  "Use normal distributions, z-scores, normal curves, and normal probability calculations.",
  "normal distribution|normal curve|bell curve|standard normal|z table|z-score|area under|empirical rule|density|68|95|99.7",
  
  "binomial_dist", 7, "Binomial Distributions",
  "Recognize binomial settings and calculate binomial probabilities.",
  "binomial|binomial distribution|binomial setting|binomial probability|binomial coefficient|success|failure|trials|n choose x|independent trials",
  
  "sampling_dist", 8, "Sampling Distributions",
  "Understand how sample statistics vary across repeated samples.",
  "sampling distribution|sample mean|sample proportion|central limit theorem|CLT|standard error|repeated samples|mean of xbar|mean of p-hat|p̂|x̅",
  
  "ci_prop", 9, "Confidence Intervals for Proportions",
  "Build and interpret confidence intervals for population proportions.",
  "confidence interval|proportion|p-hat|p̂|margin of error|critical value|z star|z*|one proportion|population proportion",
  
  "ci_mean", 10, "Confidence Intervals for Means",
  "Build and interpret confidence intervals for population means.",
  "confidence interval|mean|xbar|x̅|t distribution|t star|t*|standard error|population mean|sample mean",
  
  "ht_foundations", 11, "Hypothesis Testing Foundations",
  "Understand null/alternative hypotheses, p-values, significance, and conclusions.",
  "hypothesis test|null hypothesis|alternative hypothesis|p-value|significance level|alpha|reject|fail to reject|test statistic|claim|evidence|type i error|type ii error",
  
  "ht_prop", 12, "Hypothesis Tests for Proportions",
  "Conduct and interpret hypothesis tests for population proportions.",
  "one proportion test|proportion test|p-hat|p̂|null proportion|z test|test for proportion|population proportion",
  
  "ht_mean", 13, "Hypothesis Tests for Means",
  "Conduct and interpret hypothesis tests for population means.",
  "one mean test|mean test|t test|t statistic|population mean|sample mean|xbar|x̅|t distribution",
  
  "uses_abuses_tests", 14, "Uses and Abuses of Tests",
  "Recognize limitations, misuse, and interpretation issues in statistical testing.",
  "uses and abuses|practical significance|statistical significance|multiple testing|publication bias|p-hacking|misuse|limitations|interpretation|caution",
  
  "final_review", 15, "Cumulative Review",
  "Cumulative review and exam preparation.",
  "final exam|review|conceptual review|practice exam|exam review|formula chart"
)

write_topic_map <- function(path = topic_map_path) {
  ensure_consolidation_dirs()
  write_csv(topic_map, path)
  invisible(path)
}

# -------------------------
# 2. Helper functions
# -------------------------

read_text_file <- function(path) {
  if (is.na(path) || !file_exists(path)) return("")
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

clean_text <- function(x) {
  x %>%
    str_replace_all("\r", "\n") %>%
    str_replace_all("[ \t]+", " ") %>%
    str_replace_all("\n{3,}", "\n\n") %>%
    str_trim()
}

score_topic_match <- function(text, keywords) {
  if (is.na(text) || nchar(text) == 0) return(0L)
  pattern <- regex(keywords, ignore_case = TRUE)
  str_count(text, pattern)
}

assign_topics_to_file <- function(text, topic_map, min_score = 3, max_topics = 3) {
  scores <- topic_map %>%
    mutate(score = map_int(keywords, ~ score_topic_match(text, .x))) %>%
    arrange(desc(score))
  
  top_score <- max(scores$score, na.rm = TRUE)
  
  # Keep only topics that are meaningfully close to the best match.
  assigned <- scores %>%
    filter(
      score >= min_score,
      score >= 0.40 * top_score
    ) %>%
    slice_head(n = max_topics) %>%
    select(topic_id, student_label, score)
  
  if (nrow(assigned) == 0) {
    assigned <- scores %>%
      slice_head(n = 1) %>%
      select(topic_id, student_label, score)
  }
  
  assigned
}
make_safe_filename <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_remove("^_") %>%
    str_remove("_$")
}

source_role <- function(source_name, doc_type) {
  case_when(
    source_name == "exam_materials" & doc_type == "formula_sheet" ~ "highest_authority_formula",
    source_name == "current_professor" ~ "current_authority",
    source_name == "dr_south" ~ "supplemental",
    TRUE ~ "low_priority"
  )
}

# Pull a compact excerpt around keyword hits.
# This keeps evidence packets small enough for later LLM calls.
extract_relevant_excerpt <- function(text, keywords, max_chars = 5000) {
  text <- clean_text(text)
  if (nchar(text) <= max_chars) return(text)
  
  pattern <- regex(keywords, ignore_case = TRUE)
  loc <- str_locate(text, pattern)
  
  if (all(is.na(loc))) {
    return(str_sub(text, 1, max_chars))
  }
  
  center <- loc[1]
  start <- max(1, center - floor(max_chars / 2))
  end <- min(nchar(text), start + max_chars)
  
  str_sub(text, start, end)
}

load_topic_overrides <- function() {
  if (!file.exists(topic_overrides_path)) {
    return(tibble(
      source_name = character(),
      file_name_pattern = character(),
      topic_ids = character(),
      notes = character()
    ))
  }
  
  readr::read_csv(topic_overrides_path, show_col_types = FALSE)
}

assign_topics_with_overrides <- function(source_name, file_name, text, topic_map, overrides) {
  matches <- overrides %>%
    filter(
      source_name == !!source_name,
      str_detect(file_name, regex(file_name_pattern, ignore_case = TRUE))
    )
  
  if (nrow(matches) > 0) {
    topic_ids <- matches$topic_ids %>%
      str_split(";") %>%
      unlist() %>%
      str_trim() %>%
      unique()
    
    return(
      topic_map %>%
        filter(topic_id %in% topic_ids) %>%
        transmute(
          topic_id,
          student_label,
          score = 999L
        )
    )
  }
  
  assign_topics_to_file(text, topic_map)
}

# -------------------------
# 3. Build evidence index
# -------------------------

build_topic_evidence <- function() {
  ensure_consolidation_dirs()
  overrides <- load_topic_overrides()
  manifest <- read_csv(manifest_path, show_col_types = FALSE) %>%
    filter(extracted_ok, char_count > 0) %>%
    mutate(
      source_role = map2_chr(source_name, doc_type, source_role),
      full_text = map_chr(extracted_text_path, read_text_file),
      full_text = map_chr(full_text, clean_text)
    )
  
  evidence_index <- manifest %>%
    mutate(
      assignments = pmap(
        list(source_name, file_name, full_text),
        ~ assign_topics_with_overrides(
          source_name = ..1,
          file_name = ..2,
          text = ..3,
          topic_map = topic_map,
          overrides = overrides
        )
      )
    ) %>%select(-full_text) %>%
    tidyr::unnest(assignments) %>%
    left_join(topic_map %>% select(topic_id, keywords), by = "topic_id") %>%
    mutate(
      evidence_file = file.path(
        evidence_dir,
        paste0(
          roadmap_order_safe(topic_id),
          "_",
          topic_id,
          "__",
          source_name,
          "__",
          make_safe_filename(path_ext_remove(file_name)),
          ".md"
        )
      )
    ) %>%
    arrange(topic_id, priority, desc(score), file_name)
  
  # Write evidence markdown files
  for (i in seq_len(nrow(evidence_index))) {
    src_text <- read_text_file(evidence_index$extracted_text_path[[i]])
    excerpt <- extract_relevant_excerpt(src_text, evidence_index$keywords[[i]])
    
    md <- glue(
      "---\n",
      "topic_id: {evidence_index$topic_id[[i]]}\n",
      "topic_label: {evidence_index$student_label[[i]]}\n",
      "source_name: {evidence_index$source_name[[i]]}\n",
      "source_role: {evidence_index$source_role[[i]]}\n",
      "doc_type: {evidence_index$doc_type[[i]]}\n",
      "priority: {evidence_index$priority[[i]]}\n",
      "file_name: {evidence_index$file_name[[i]]}\n",
      "match_score: {evidence_index$score[[i]]}\n",
      "---\n\n",
      "# Evidence: {evidence_index$student_label[[i]]}\n\n",
      "**Source role:** {evidence_index$source_role[[i]]}\n\n",
      "**File:** {evidence_index$file_name[[i]]}\n\n",
      "## Relevant excerpt\n\n",
      "{excerpt}\n"
    )
    
    writeLines(md, evidence_index$evidence_file[[i]], useBytes = TRUE)
  }
  
  evidence_index %>%
    select(
      topic_id, student_label, source_name, source_role, doc_type, priority,
      file_name, score, extracted_text_path, evidence_file
    ) %>%
    write_csv(evidence_index_path)
  
  evidence_index
}

roadmap_order_safe <- function(topic_id) {
  ord <- topic_map$roadmap_order[match(topic_id, topic_map$topic_id)]
  ifelse(is.na(ord), "99", sprintf("%02d", ord))
}

# -------------------------
# 4. Build draft wiki shells
# -------------------------

build_draft_wiki_shells <- function(evidence_index) {
  draft_dir <- file.path(wiki_dir, "draft_concept_pages")
  dir_create(draft_dir)
  
  for (topic in topic_map$topic_id) {
    topic_info <- topic_map %>% filter(topic_id == topic)
    ev <- evidence_index %>% filter(topic_id == topic)
    
    top_files <- ev %>%
      arrange(priority, desc(score)) %>%
      select(source_name, source_role, doc_type, file_name, score) %>%
      distinct() %>%
      head(12)
    
    source_list <- if (nrow(top_files) == 0) {
      "No evidence files mapped yet."
    } else {
      paste0(
        "- ",
        top_files$source_role,
        " | ",
        top_files$doc_type,
        " | ",
        top_files$file_name,
        " | score: ",
        top_files$score,
        collapse = "\n"
      )
    }
    
    md <- glue(
      "---\n",
      "topic_id: {topic_info$topic_id}\n",
      "roadmap_order: {topic_info$roadmap_order}\n",
      "student_label: {topic_info$student_label}\n",
      "status: draft_needs_llm_consolidation\n",
      "authority_rule: current_formula_sheets_then_current_professor_then_supplemental\n",
      "---\n\n",
      "# {topic_info$student_label}\n\n",
      "## Purpose\n\n",
      "{topic_info$description}\n\n",
      "## Canonical explanation\n\n",
      "_To be generated from current-authority sources and reviewed._\n\n",
      "## Formula / notation notes\n\n",
      "_To be generated primarily from current formula sheets when relevant._\n\n",
      "## Common mistakes\n\n",
      "_To be generated from practice/solution evidence and reviewed._\n\n",
      "## Alternative explanation styles\n\n",
      "_Supplemental material may be used here, but professor/source names should not be shown to students._\n\n",
      "## Evidence files mapped to this topic\n\n",
      "{source_list}\n"
    )
    
    out_path <- file.path(draft_dir, paste0(roadmap_order_safe(topic), "_", topic, ".md"))
    writeLines(md, out_path, useBytes = TRUE)
  }
  
  message("Draft wiki shells written to: ", draft_dir)
}

# -------------------------
# 5. Main runner
# -------------------------

run_consolidation_setup <- function() {
  message("Writing topic map to: ", topic_map_path)
  write_topic_map(topic_map_path)
  
  message("Building topic evidence packets...")
  evidence_index <- build_topic_evidence()
  
  message("Writing draft wiki shells...")
  build_draft_wiki_shells(evidence_index)
  
  message("\nDone.")
  message("Topic map: ", topic_map_path)
  message("Evidence index: ", evidence_index_path)
  message("Evidence packets: ", evidence_dir)
  message("Draft wiki shells: ", file.path(wiki_dir, "draft_concept_pages"))
  
  invisible(evidence_index)
}
