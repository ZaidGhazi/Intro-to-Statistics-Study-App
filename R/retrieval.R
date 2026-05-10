library(dplyr)
library(fs)
library(purrr)
library(readr)
library(stringr)
library(tibble)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

if (!exists("coerce_chunk_schema", mode = "function") && file.exists("R/chunk_schema.R")) {
  source("R/chunk_schema.R")
}
if (!exists("normalize_student_query", mode = "function") && file.exists("R/aliases.R")) {
  source("R/aliases.R")
}
if (!exists("load_concept_pages", mode = "function") && file.exists("R/wiki.R")) {
  source("R/wiki.R")
}
if (!exists("ingest_professor_materials", mode = "function") && file.exists("R/overlays.R")) {
  source("R/overlays.R")
}

.rag_retrieval_cache <- new.env(parent = emptyenv())

retrieval_elapsed_seconds <- function(start_time) {
  round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 3)
}

normalize_rag_module_ids <- function(module_ids = NULL, query = NULL) {
  module_ids <- module_ids %||% character()
  if (length(module_ids) == 0 || all(is.na(module_ids))) {
    return(character())
  }
  module_ids <- unlist(module_ids, use.names = FALSE)
  module_ids <- purrr::map_chr(module_ids, ~ normalize_rag_module_id(.x, query = query) %||% NA_character_)
  unique(stats::na.omit(module_ids))
}

rag_stopwords <- c(
  "a", "an", "and", "are", "as", "at", "be", "but", "by", "can", "do", "does",
  "for", "from", "how", "i", "if", "in", "is", "it", "me", "my", "of", "on",
  "or", "so", "the", "this", "to", "what", "when", "where", "which", "why",
  "with", "you", "your", "mean", "means"
)

tokenize_rag_text <- function(text) {
  text %>%
    normalize_chunk_text() %>%
    str_split("\\s+") %>%
    unlist(use.names = FALSE) %>%
    unique() %>%
    discard(~ !nzchar(.x) || .x %in% rag_stopwords || nchar(.x) < 2)
}

route_question_to_module <- function(query, active_module_ids = NULL) {
  q <- normalize_student_query(query %||% "")
  inferred <- case_when(
    str_detect(q, "chi square|chisquare|expected counts|goodness of fit|independence table") ~ "chi_square",
    str_detect(q, "two proportions|difference in proportions|p_1|p1|p_2|p2") ~ "two_proportions",
    str_detect(q, "regression inference|slope test|slope confidence|linear regression inference") ~ "regression_inference",
    str_detect(q, "p_hat|p_0|one proportion|population proportion|sample proportion") &
      str_detect(q, "hypothesis test|p_value|null hypothesis|alternative hypothesis|reject") ~ "hypothesis_testing",
    str_detect(q, "x_bar|mu_0|one mean|sample mean|population mean") &
      str_detect(q, "hypothesis test|p_value|null hypothesis|alternative hypothesis|reject") ~ "hypothesis_testing",
    str_detect(q, "hypothesis test|p_value|null hypothesis|alternative hypothesis|reject|fail to reject|alpha|significance") ~ "hypothesis_testing",
    str_detect(q, "p_hat|p_0|one proportion|population proportion|sample proportion") ~ "inference_proportion",
    str_detect(q, "x_bar|mu_0|one mean|sample mean|population mean") ~ "inference_mean",
    str_detect(q, "confidence interval|margin of error|z_star|t_star") ~ "confidence_intervals",
    str_detect(q, "standard error|central limit theorem|clt|sampling distribution") ~ "sampling_distributions",
    str_detect(q, "binomial|success failure|at least|at most|n choose") ~ "binomial_distribution",
    str_detect(q, "normal distribution|z score|z-score|standard normal|empirical rule") ~ "normal_distribution",
    str_detect(q, "probability|independent|disjoint|conditional|complement|event") ~ "probability",
    str_detect(q, "sample|sampling|bias|stratified|cluster|systematic") ~ "sampling",
    str_detect(q, "experiment|random assignment|treatment|placebo|blinding|confounding") ~ "experiments",
    str_detect(q, "slope|intercept|residual|correlation|regression|least squares") ~ "regression",
    str_detect(q, "scatterplot|association") ~ "scatterplots_correlation",
    str_detect(q, "median|mean|measure of center|resistant|nonresistant|non resistant|sensitive|quartile|iqr|standard deviation|variance|five number|boxplot|histogram|skew|skewed|outlier|extreme value") ~ "descriptive_stats",
    str_detect(q, "variable type|categorical|quantitative|bar chart|pie chart|graph") ~ "data_graphs",
    TRUE ~ NA_character_
  )
  selected <- normalize_rag_module_ids(active_module_ids, query = query)
  if (!is.na(inferred) && nzchar(inferred)) {
    return(inferred)
  }
  if (length(selected) == 1) {
    return(selected[[1]])
  }
  NA_character_
}

classify_query_intent <- function(query) {
  q <- normalize_student_query(query %||% "")
  case_when(
    str_detect(q, "figure|image|picture|graph|visual|diagram|draw|show me") ~ "visual_request",
    str_detect(q, "just give|give me the answer|final answer|solve it for me|homework answer|quiz answer|test answer|exam answer") ~ "direct_answer_request",
    str_detect(q, "practice|quiz me|another problem|question") ~ "practice_request",
    str_detect(q, "why|explain|what does|what is|how do") ~ "concept_help",
    TRUE ~ "general_help"
  )
}

get_related_modules <- function(module_id) {
  module_id <- normalize_rag_module_id(module_id)
  if (is.null(module_id)) {
    return(character())
  }
  related <- list(
    data_graphs = c("descriptive_stats", "sampling"),
    descriptive_stats = c("data_graphs", "normal_distribution"),
    normal_distribution = c("sampling_distributions", "confidence_intervals", "binomial_distribution"),
    scatterplots_correlation = c("regression", "data_graphs"),
    regression = c("scatterplots_correlation", "regression_inference"),
    sampling = c("experiments", "sampling_distributions", "data_graphs"),
    experiments = c("sampling", "inference_in_practice"),
    probability = c("binomial_distribution", "sampling_distributions"),
    sampling_distributions = c("normal_distribution", "confidence_intervals", "hypothesis_testing", "inference_proportion", "inference_mean"),
    binomial_distribution = c("probability", "normal_distribution", "inference_proportion"),
    confidence_intervals = c("sampling_distributions", "inference_proportion", "inference_mean", "hypothesis_testing"),
    hypothesis_testing = c("sampling_distributions", "confidence_intervals", "inference_proportion", "inference_mean", "inference_in_practice"),
    inference_in_practice = c("hypothesis_testing", "experiments"),
    inference_mean = c("confidence_intervals", "hypothesis_testing", "sampling_distributions"),
    inference_proportion = c("confidence_intervals", "hypothesis_testing", "sampling_distributions", "binomial_distribution"),
    two_proportions = c("inference_proportion", "hypothesis_testing"),
    chi_square = c("hypothesis_testing", "probability"),
    regression_inference = c("regression", "hypothesis_testing")
  )
  related[[module_id]] %||% character()
}

load_textbook_chunks_from_disk <- function() {
  rds_path <- "data/processed/textbook_chunks.rds"
  csv_path <- "data/processed/textbook_chunks.csv"
  if (fs::file_exists(rds_path)) {
    return(coerce_chunk_schema(readRDS(rds_path)))
  }
  if (fs::file_exists(csv_path)) {
    return(coerce_chunk_schema(suppressMessages(readr::read_csv(csv_path, show_col_types = FALSE))))
  }
  empty_chunk_table()
}

load_concept_page_chunks <- function() {
  pages <- tryCatch(
    {
      if (exists("load_concept_pages", mode = "function")) {
        load_concept_pages()
      } else if (fs::dir_exists("data/wiki/concept_pages")) {
        files <- fs::dir_ls("data/wiki/concept_pages", glob = "*.md", recurse = FALSE)
        tibble(
          file_path = as.character(files),
          file_name = fs::path_file(files),
          topic_id = fs::path_ext_remove(fs::path_file(files)),
          student_label = topic_id,
          markdown_body = map_chr(files, ~ paste(readr::read_lines(.x, progress = FALSE), collapse = "\n"))
        )
      } else {
        tibble()
      }
    },
    error = function(e) tibble()
  )
  if (!is.data.frame(pages) || nrow(pages) == 0) {
    return(empty_chunk_table())
  }

  chunks <- pages %>%
    filter(!str_detect(file_name %||% "", regex("^README", ignore_case = TRUE))) %>%
    transmute(
      chunk_id = make_chunk_id("concept_page", topic_to_rag_module(topic_id), topic_id, topic_id, row_number()),
      source_name = "course_concept_pages",
      source_type = "concept_page",
      source_scope = "universal_core",
      source_priority = default_source_priority("concept_page", "universal_core"),
      professor_id = NA_character_,
      chapter = NA_integer_,
      section = NA_character_,
      module_id = topic_to_rag_module(topic_id),
      topic_id = topic_id,
      concept_tag = topic_id,
      content_type = "concept_explanation",
      page_number = NA_integer_,
      slide_number = NA_integer_,
      parent_id = topic_id,
      text = markdown_body,
      normalized_text = normalize_chunk_text(markdown_body),
      aliases_added = map_chr(markdown_body, ~ apply_alias_replacements(.x, return_aliases = TRUE)$aliases_added),
      image_refs = "",
      display_permission_status = "local_only"
    )

  coerce_chunk_schema(chunks)
}

load_existing_vector_chunks <- function(index_path = "data/processed/retrieval_index.rds") {
  if (!fs::file_exists(index_path)) {
    return(empty_chunk_table())
  }
  index <- tryCatch(readRDS(index_path), error = function(e) NULL)
  if (is.null(index) || is.null(index$chunks) || !is.data.frame(index$chunks)) {
    return(empty_chunk_table())
  }
  raw <- tibble::as_tibble(index$chunks)
  if (!all(c("chunk_id", "text") %in% names(raw))) {
    return(empty_chunk_table())
  }
  vector_defaults <- list(
    doc_id = NA_character_,
    module = NA_integer_,
    day = NA_integer_,
    page = NA_integer_,
    text_clean = NA_character_
  )
  for (col in names(vector_defaults)) {
    if (!col %in% names(raw)) {
      raw[[col]] <- rep(vector_defaults[[col]], nrow(raw))
    }
  }

  raw %>%
    mutate(
      module_id = map_chr(text, route_question_to_module),
      module_id = if_else(
        is.na(module_id) | !nzchar(module_id),
        paste0("legacy_module_", if_else(is.na(module), "unknown", as.character(module))),
        module_id
      ),
      topic_id = module_id,
      concept_tag = module_id,
      source_name = "current_section_retrieval_index",
      source_type = "professor_notes",
      source_scope = "professor_specific",
      source_priority = default_source_priority("professor_notes", "professor_specific", professor_id = "current_professor"),
      professor_id = "current_professor",
      chapter = NA_integer_,
      section = paste0(
        "module_",
        if_else(is.na(module), "unknown", as.character(module)),
        "_day_",
        if_else(is.na(day), "unknown", as.character(day))
      ),
      content_type = "concept_explanation",
      page_number = suppressWarnings(as.integer(page)),
      slide_number = NA_integer_,
      parent_id = as.character(doc_id),
      normalized_text = if_else(
        is.na(text_clean) | !nzchar(str_squish(text_clean)),
        normalize_chunk_text(text),
        text_clean
      ),
      aliases_added = map_chr(text, ~ apply_alias_replacements(.x, return_aliases = TRUE)$aliases_added),
      image_refs = "",
      display_permission_status = "local_only"
    ) %>%
    coerce_chunk_schema()
}

load_rag_chunks <- function(include_existing_index = TRUE, refresh = FALSE) {
  cache_key <- paste0("chunks_", isTRUE(include_existing_index))
  if (!isTRUE(refresh) && exists(cache_key, envir = .rag_retrieval_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .rag_retrieval_cache, inherits = FALSE))
  }
  textbook <- load_textbook_chunks_from_disk()
  concept_pages <- load_concept_page_chunks()
  overlays <- tryCatch(ingest_professor_materials(), error = function(e) empty_chunk_table())
  vector_chunks <- if (isTRUE(include_existing_index)) load_existing_vector_chunks() else empty_chunk_table()

  chunks <- bind_rows(textbook, concept_pages, overlays, vector_chunks) %>%
    filter(!is.na(text), nzchar(str_squish(text))) %>%
    coerce_chunk_schema()
  assign(cache_key, chunks, envir = .rag_retrieval_cache)
  chunks
}

load_dense_index_cached <- function(index_path = "data/processed/retrieval_index.rds", refresh = FALSE) {
  cache_key <- paste0("dense_index_", normalizePath(index_path, winslash = "/", mustWork = FALSE))
  if (!isTRUE(refresh) && exists(cache_key, envir = .rag_retrieval_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .rag_retrieval_cache, inherits = FALSE))
  }
  index <- tryCatch(readRDS(index_path), error = function(e) NULL)
  assign(cache_key, index, envir = .rag_retrieval_cache)
  index
}

dense_retrieve <- function(query, chunks = NULL, top_k = 30L, index_path = "data/processed/retrieval_index.rds") {
  if (isTRUE(getOption("stat2331.disable_dense_retrieval", FALSE))) {
    return(tibble(chunk_id = character(), semantic_score = numeric(), retrieval_method = character()))
  }
  if (!fs::file_exists(index_path)) {
    message("dense_retrieve(): no embedding/vector index found; using keyword retrieval only.")
    return(tibble(chunk_id = character(), semantic_score = numeric(), retrieval_method = character()))
  }
  if (!requireNamespace("text2vec", quietly = TRUE)) {
    message("dense_retrieve(): text2vec is not installed; using keyword retrieval only.")
    return(tibble(chunk_id = character(), semantic_score = numeric(), retrieval_method = character()))
  }

  index <- load_dense_index_cached(index_path)
  if (is.null(index) || is.null(index$chunks) || is.null(index$doc_dense) || is.null(index$vectorizer) || is.null(index$tfidf_model) || is.null(index$lsa_fit)) {
    message("dense_retrieve(): vector index exists but is incomplete; using keyword retrieval only.")
    return(tibble(chunk_id = character(), semantic_score = numeric(), retrieval_method = character()))
  }

  normalized <- normalize_student_query(query)
  if (length(tokenize_rag_text(normalized)) == 0) {
    return(tibble(chunk_id = character(), semantic_score = numeric(), retrieval_method = character()))
  }
  scores <- tryCatch(
    {
      iterator <- text2vec::itoken(normalized, tokenizer = text2vec::word_tokenizer, progressbar = FALSE)
      dtm <- suppressWarnings(text2vec::create_dtm(iterator, index$vectorizer))
      if (nrow(dtm) == 0 || ncol(dtm) == 0) {
        numeric()
      } else {
        tfidf <- suppressWarnings(index$tfidf_model$transform(dtm))
        query_dense <- as.matrix(tfidf %*% index$lsa_fit$v)
        doc_dense <- as.matrix(index$doc_dense)
        denom <- sqrt(rowSums(doc_dense ^ 2)) * sqrt(sum(query_dense ^ 2))
        raw_scores <- as.numeric(doc_dense %*% as.numeric(query_dense))
        ifelse(denom > 0, raw_scores / denom, 0)
      }
    },
    error = function(e) {
      message("dense_retrieve(): vector scoring failed; using keyword retrieval only. ", conditionMessage(e))
      numeric()
    }
  )
  if (length(scores) == 0) {
    return(tibble(chunk_id = character(), semantic_score = numeric(), retrieval_method = character()))
  }

  tibble(
    chunk_id = index$chunks$chunk_id,
    semantic_score = pmax(0, scores),
    retrieval_method = "dense_lsa"
  ) %>%
    arrange(desc(semantic_score)) %>%
    slice_head(n = top_k) %>%
    filter(semantic_score > 0)
}

keyword_retrieve <- function(query, chunks = NULL, top_k = 30L) {
  chunks <- chunks %||% load_rag_chunks()
  chunks <- coerce_chunk_schema(chunks)
  if (nrow(chunks) == 0) {
    return(tibble(chunk_id = character(), keyword_score = numeric(), retrieval_method = character()))
  }

  normalized <- normalize_student_query(query)
  tokens <- tokenize_rag_text(normalized)
  if (length(tokens) == 0) {
    return(tibble(chunk_id = character(), keyword_score = numeric(), retrieval_method = character()))
  }

  token_pattern <- paste(stringr::str_replace_all(tokens, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1"), collapse = "|")
  phrase <- str_squish(normalized)

  chunks %>%
    mutate(
      token_hits = str_count(normalized_text, regex(paste0("(?<![[:alnum:]_])(", token_pattern, ")(?![[:alnum:]_])"), ignore_case = TRUE)),
      distinct_hits = map_int(normalized_text, ~ sum(tokens %in% str_split(.x, "\\s+")[[1]])),
      phrase_hit = if_else(nchar(phrase) >= 8 & str_detect(normalized_text, fixed(phrase, ignore_case = TRUE)), 1, 0),
      keyword_score = token_hits * 0.35 + distinct_hits * 0.75 + phrase_hit * 2.0,
      retrieval_method = "keyword"
    ) %>%
    filter(keyword_score > 0) %>%
    arrange(desc(keyword_score)) %>%
    slice_head(n = top_k) %>%
    select(chunk_id, keyword_score, retrieval_method)
}

alias_match_candidates <- function(query, chunks = NULL, top_k = 30L) {
  chunks <- chunks %||% load_rag_chunks()
  chunks <- coerce_chunk_schema(chunks)
  if (nrow(chunks) == 0) {
    return(tibble(chunk_id = character(), notation_match_boost = numeric(), retrieval_method = character()))
  }

  normalized <- normalize_student_query(query)
  canonicals <- build_alias_table()$canonical %>% unique()
  query_aliases <- canonicals[str_detect(normalized, fixed(canonicals, ignore_case = TRUE))]
  if (length(query_aliases) == 0) {
    return(tibble(chunk_id = character(), notation_match_boost = numeric(), retrieval_method = character()))
  }
  pattern <- paste(stringr::str_replace_all(query_aliases, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1"), collapse = "|")

  chunks %>%
    mutate(
      notation_match_boost = str_count(normalized_text, regex(pattern, ignore_case = TRUE)) * 0.6,
      retrieval_method = "alias"
    ) %>%
    filter(notation_match_boost > 0) %>%
    arrange(desc(notation_match_boost)) %>%
    slice_head(n = top_k) %>%
    select(chunk_id, notation_match_boost, retrieval_method)
}

rerank_score_columns <- function() {
  c(
    "semantic_score",
    "keyword_score",
    "module_match_boost",
    "current_module_match_boost",
    "selected_module_match_boost",
    "concept_tag_boost",
    "current_question_concept_boost",
    "source_priority_boost",
    "notation_match_boost",
    "outside_selected_modules_penalty",
    "wrong_module_penalty",
    "unrelated_concept_penalty",
    "duplicate_penalty"
  )
}

coerce_numeric_score_column <- function(x) {
  numeric_x <- suppressWarnings(as.numeric(x))
  numeric_x[is.na(numeric_x)] <- 0
  numeric_x
}

ensure_rerank_score_cols <- function(candidates) {
  if (!is.data.frame(candidates)) {
    return(candidates)
  }
  candidates <- tibble::as_tibble(candidates)
  n <- nrow(candidates)
  for (col in rerank_score_columns()) {
    if (!col %in% names(candidates)) {
      candidates[[col]] <- rep(0, n)
    }
    candidates[[col]] <- coerce_numeric_score_column(candidates[[col]])
  }
  if (!"source_policy_boost" %in% names(candidates)) {
    candidates$source_policy_boost <- rep(0, n)
  }
  candidates$source_policy_boost <- coerce_numeric_score_column(candidates$source_policy_boost)
  if (!"final_score" %in% names(candidates)) {
    candidates$final_score <- rep(0, n)
  }
  candidates$final_score <- coerce_numeric_score_column(candidates$final_score)
  candidates
}

ensure_rerank_metadata_cols <- function(candidates) {
  if (!is.data.frame(candidates)) {
    return(candidates)
  }
  candidates <- tibble::as_tibble(candidates)
  metadata_defaults <- list(
    chunk_id = NA_character_,
    source_name = NA_character_,
    source_type = NA_character_,
    source_scope = NA_character_,
    source_priority = 100,
    module_id = NA_character_,
    topic_id = NA_character_,
    concept_tag = NA_character_,
    normalized_text = NA_character_,
    text = NA_character_,
    active_module_id = NA_character_,
    current_module_id = NA_character_,
    active_module_ids = NA_character_,
    module_policy = "unknown",
    retrieval_methods = "unknown"
  )
  for (col in names(metadata_defaults)) {
    if (!col %in% names(candidates)) {
      candidates[[col]] <- rep(metadata_defaults[[col]], nrow(candidates))
    }
  }
  candidates$source_priority <- coerce_numeric_score_column(candidates$source_priority)
  candidates$source_priority[candidates$source_priority == 0] <- 100
  candidates$normalized_text <- if_else(
    is.na(candidates$normalized_text) | !nzchar(str_squish(candidates$normalized_text)),
    normalize_chunk_text(candidates$text %||% ""),
    candidates$normalized_text
  )
  candidates
}

merge_retrieval_results <- function(results, chunks = NULL) {
  chunks <- chunks %||% load_rag_chunks()
  chunks <- coerce_chunk_schema(chunks)
  results <- purrr::discard(results, ~ is.null(.x) || !is.data.frame(.x) || nrow(.x) == 0)
  if (length(results) == 0) {
    return(empty_chunk_table() %>%
      ensure_rerank_score_cols() %>%
      mutate(retrieval_methods = character()))
  }

  merged_input <- bind_rows(results) %>%
    ensure_rerank_score_cols()
  if (!"retrieval_method" %in% names(merged_input)) {
    merged_input$retrieval_method <- "unknown"
  }

  merged_scores <- merged_input %>%
    mutate(retrieval_method = if_else(is.na(retrieval_method) | !nzchar(retrieval_method), "unknown", retrieval_method)) %>%
    mutate(
      semantic_score = coerce_numeric_score_column(semantic_score),
      keyword_score = coerce_numeric_score_column(keyword_score),
      notation_match_boost = coerce_numeric_score_column(notation_match_boost)
    ) %>%
    group_by(chunk_id) %>%
    summarise(
      semantic_score = max(semantic_score, na.rm = TRUE),
      keyword_score = max(keyword_score, na.rm = TRUE),
      notation_match_boost = max(notation_match_boost, na.rm = TRUE),
      retrieval_methods = paste(unique(retrieval_method %||% "unknown"), collapse = "|"),
      .groups = "drop"
    )

  chunks %>%
    inner_join(merged_scores, by = "chunk_id")
}

apply_source_policy <- function(candidates, mode = c("general", "professor"), professor_id = NULL) {
  mode <- match.arg(mode)
  policy <- get_active_source_policy(mode, professor_id)
  if (!is.data.frame(candidates) || nrow(candidates) == 0) {
    return(candidates)
  }

  candidates %>%
    mutate(
      source_priority_boost = pmax(0, (100 - source_priority) / 40),
      source_policy_boost = case_when(
        mode == "professor" & source_type == "professor_notes" & professor_id == policy$professor_id ~ 4.0,
        mode == "professor" & source_type == "practice_problem" & professor_id == policy$professor_id ~ 2.5,
        mode == "professor" & source_type == "professor_notes" & professor_id != policy$professor_id ~ -0.5,
        mode == "professor" & source_type %in% c("textbook", "concept_page") ~ 1.6,
        mode == "general" & source_type %in% c("textbook", "concept_page") ~ 2.0,
        mode == "general" & source_type == "professor_notes" & source_scope == "professor_specific" ~ 0.2,
        mode == "general" & source_scope == "supplemental" ~ -0.2,
        TRUE ~ 0
      )
    )
}

apply_module_policy <- function(candidates,
                                active_module_id = NULL,
                                active_module_ids = NULL,
                                current_module_id = NULL) {
  if (!is.data.frame(candidates) || nrow(candidates) == 0) {
    return(candidates)
  }

  current <- normalize_rag_module_id(current_module_id %||% active_module_id)
  selected <- normalize_rag_module_ids(active_module_ids)
  if (length(selected) == 0 && !is.null(active_module_id)) {
    selected <- normalize_rag_module_ids(active_module_id)
  }
  if (is.null(current) && length(selected) == 1) {
    current <- selected[[1]]
  }

  related <- get_related_modules(current)
  selected_related <- unique(unlist(purrr::map(selected, get_related_modules), use.names = FALSE))
  selected_label <- paste(selected, collapse = "|")
  current_label <- current %||% NA_character_

  if (is.null(current) && length(selected) == 0) {
    return(
      candidates %>%
        mutate(
          active_module_id = NA_character_,
          current_module_id = NA_character_,
          active_module_ids = NA_character_,
          current_module_match_boost = 0,
          selected_module_match_boost = 0,
          module_match_boost = 0,
          outside_selected_modules_penalty = 0,
          wrong_module_penalty = 0,
          module_policy = "no_selected_modules"
        )
    )
  }

  candidates %>%
    mutate(
      active_module_id = current_label,
      current_module_id = current_label,
      active_module_ids = selected_label,
      current_module_match_boost = if_else(!is.na(module_id) & !is.na(current_label) & module_id == current_label, 3.0, 0),
      selected_module_match_boost = case_when(
        length(selected) == 0 ~ 0,
        module_id %in% selected & module_id != current_label ~ 1.4,
        module_id %in% selected & module_id == current_label ~ 0.8,
        TRUE ~ 0
      ),
      module_match_boost = current_module_match_boost + selected_module_match_boost,
      outside_selected_modules_penalty = case_when(
        length(selected) == 0 ~ 0,
        module_id %in% selected ~ 0,
        module_id %in% related | module_id %in% selected_related ~ 0.5,
        TRUE ~ 1.25
      ),
      wrong_module_penalty = case_when(
        !is.na(current_label) & module_id == current_label ~ 0,
        length(selected) > 0 & module_id %in% selected ~ 0.15,
        !is.na(current_label) & module_id %in% related ~ 0.45,
        TRUE ~ 0
      ),
      wrong_module_penalty = if_else(wrong_module_penalty == 0 & !is.na(current_label) & module_id != current_label & !(module_id %in% selected) & !(module_id %in% related), 1.75, wrong_module_penalty),
      module_policy = case_when(
        !is.na(current_label) & module_id == current_label ~ "current_module",
        length(selected) > 0 & module_id %in% selected ~ "selected_module",
        !is.na(current_label) & module_id %in% related ~ "related_module",
        length(selected) > 0 & !(module_id %in% selected) ~ "outside_selected_modules",
        TRUE ~ "wrong_module"
      )
    )
}

concept_tag_score <- function(query, concept_tag, topic_id, module_id) {
  normalized <- normalize_student_query(query)
  tokens <- tokenize_rag_text(normalized)
  target <- normalize_chunk_text(paste(concept_tag, topic_id, module_id, collapse = " "))
  if (length(tokens) == 0 || !nzchar(target)) {
    return(0)
  }
  sum(tokens %in% str_split(target, "\\s+")[[1]]) * 0.35
}

normalize_concept_id <- function(x) {
  normalize_chunk_text(x %||% "") %>%
    str_replace_all("\\s+", "_") %>%
    str_squish()
}

concept_family_terms <- function(expected_concept_tag = NULL, query = NULL) {
  expected <- normalize_concept_id(expected_concept_tag)
  combined <- normalize_chunk_text(paste(expected, query %||% "", collapse = " "))
  if (str_detect(combined, "resistant|nonresistant|non resistant|sensitive|outlier|skew|measure of center|median|mean")) {
    return(list(
      name = "resistant_measures",
      positive = c("resistant", "nonresistant", "non resistant", "median", "mean", "outlier", "outliers", "skew", "skewed", "right skewed", "measure of center", "extreme values", "tail"),
      negative = c("variable_classification", "variable type", "categorical", "nominal", "ordinal", "bar chart", "pie chart", "graph selection", "histogram for categorical")
    ))
  }
  list(name = expected, positive = str_split(expected, "_")[[1]], negative = character())
}

current_question_concept_match_score <- function(expected_concept_tag = NULL, concept_tag = NULL, topic_id = NULL, text = NULL, query = NULL) {
  expected <- normalize_concept_id(expected_concept_tag)
  if (!nzchar(expected)) {
    return(0)
  }
  target <- normalize_concept_id(paste(concept_tag %||% "", topic_id %||% "", collapse = " "))
  normalized_text <- normalize_chunk_text(text %||% "")
  family <- concept_family_terms(expected, query)
  exact <- target == expected || str_detect(target, fixed(expected))
  positive_hits <- sum(vapply(family$positive, function(term) str_detect(normalized_text, fixed(term, ignore_case = TRUE)) || str_detect(target, fixed(normalize_concept_id(term), ignore_case = TRUE)), logical(1)))
  if (isTRUE(exact)) {
    return(3.0)
  }
  if (positive_hits >= 3) {
    return(2.25)
  }
  if (positive_hits >= 1) {
    return(1.0)
  }
  0
}

unrelated_current_concept_penalty <- function(expected_concept_tag = NULL, concept_tag = NULL, topic_id = NULL, text = NULL, query = NULL) {
  expected <- normalize_concept_id(expected_concept_tag)
  if (!nzchar(expected)) {
    return(0)
  }
  target <- normalize_chunk_text(paste(concept_tag %||% "", topic_id %||% "", text %||% "", collapse = " "))
  family <- concept_family_terms(expected, query)
  if (length(family$negative) == 0) {
    return(0)
  }
  negative_hits <- sum(vapply(family$negative, function(term) str_detect(target, fixed(term, ignore_case = TRUE)), logical(1)))
  positive_hits <- sum(vapply(family$positive, function(term) str_detect(target, fixed(term, ignore_case = TRUE)), logical(1)))
  if (negative_hits > 0 && positive_hits == 0) {
    return(2.5)
  }
  if (negative_hits > 0 && positive_hits < 2) {
    return(1.25)
  }
  0
}

rerank_chunks <- function(candidates,
                          query = NULL,
                          active_module_id = NULL,
                          active_module_ids = NULL,
                          current_module_id = NULL,
                          expected_concept_tag = NULL) {
  if (!is.data.frame(candidates) || nrow(candidates) == 0) {
    return(ensure_rerank_score_cols(candidates))
  }

  if ((length(active_module_ids %||% character()) > 0 || !is.null(active_module_id) || !is.null(current_module_id)) &&
      "module_id" %in% names(candidates)) {
    if (!all(c("current_module_match_boost", "selected_module_match_boost", "outside_selected_modules_penalty", "wrong_module_penalty", "module_policy") %in% names(candidates))) {
      candidates <- apply_module_policy(
        candidates,
        active_module_id = active_module_id,
        active_module_ids = active_module_ids,
        current_module_id = current_module_id
      )
    }
  }

  candidates %>%
    ensure_rerank_metadata_cols() %>%
    ensure_rerank_score_cols() %>%
    mutate(
      concept_tag_boost = pmax(
        concept_tag_boost,
        pmap_dbl(list(concept_tag, topic_id, module_id), ~ concept_tag_score(query %||% "", ..1, ..2, ..3))
      ),
      current_question_concept_boost = pmax(
        current_question_concept_boost,
        pmap_dbl(
          list(concept_tag, topic_id, text),
          ~ current_question_concept_match_score(expected_concept_tag, ..1, ..2, ..3, query = query %||% "")
        )
      ),
      unrelated_concept_penalty = pmax(
        unrelated_concept_penalty,
        pmap_dbl(
          list(concept_tag, topic_id, text),
          ~ unrelated_current_concept_penalty(expected_concept_tag, ..1, ..2, ..3, query = query %||% "")
        )
      ),
      duplicate_key = str_sub(normalized_text, 1, 180),
      duplicate_rank = ave(seq_along(duplicate_key), duplicate_key, FUN = seq_along),
      duplicate_penalty = pmax(duplicate_penalty, if_else(duplicate_rank > 1, 0.75, 0)),
      final_score = semantic_score +
        keyword_score +
        current_module_match_boost +
        selected_module_match_boost +
        concept_tag_boost +
        current_question_concept_boost +
        source_priority_boost +
        notation_match_boost -
        outside_selected_modules_penalty -
        wrong_module_penalty -
        unrelated_concept_penalty -
        duplicate_penalty
    ) %>%
    arrange(desc(final_score), source_priority, desc(keyword_score), desc(semantic_score))
}

test_rerank_missing_score_cols <- function() {
  test_candidates <- tibble::tibble(
    chunk_id = "c1",
    text = "p_hat is the sample proportion.",
    semantic_score = 0.8,
    keyword_score = 0.2
  )
  out <- rerank_chunks(
    test_candidates,
    query = "what does p^ mean?",
    active_module_id = "hypothesis_testing"
  )
  required <- c("notation_match_boost", "final_score")
  missing <- setdiff(required, names(out))
  if (length(missing) > 0) {
    stop("rerank regression failed; missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(out) != 1 || any(is.na(out$final_score))) {
    stop("rerank regression failed; expected one row with a non-missing final_score.", call. = FALSE)
  }
  out
}

retrieve_candidates <- function(query,
                                mode = "general",
                                professor_id = NULL,
                                top_k = 30L,
                                active_module_id = NULL,
                                active_module_ids = NULL,
                                current_module_id = NULL,
                                expected_concept_tag = NULL) {
  total_start <- Sys.time()
  chunk_start <- Sys.time()
  chunks <- load_rag_chunks()
  chunk_load_time <- retrieval_elapsed_seconds(chunk_start)
  if (nrow(chunks) == 0) {
    return(empty_chunk_table() %>% mutate(final_score = numeric()))
  }

  expanded_queries <- expand_query(query)
  candidate_start <- Sys.time()
  keyword_hits <- map_dfr(expanded_queries, ~ keyword_retrieve(.x, chunks = chunks, top_k = top_k))
  dense_hits <- dense_retrieve(query, chunks = chunks, top_k = top_k)
  alias_hits <- alias_match_candidates(query, chunks = chunks, top_k = top_k)
  candidate_retrieval_time <- retrieval_elapsed_seconds(candidate_start)

  rerank_start <- Sys.time()
  out <- merge_retrieval_results(list(keyword_hits, dense_hits, alias_hits), chunks = chunks) %>%
    apply_source_policy(mode = mode, professor_id = professor_id) %>%
    apply_module_policy(
      active_module_id = active_module_id,
      active_module_ids = active_module_ids,
      current_module_id = current_module_id
    ) %>%
    rerank_chunks(query = query, expected_concept_tag = expected_concept_tag) %>%
    slice_head(n = top_k)
  attr(out, "retrieval_timings") <- list(
    chunk_load_time = chunk_load_time,
    candidate_retrieval_time = candidate_retrieval_time,
    rerank_time = retrieval_elapsed_seconds(rerank_start),
    total_candidate_time = retrieval_elapsed_seconds(total_start)
  )
  out
}

expand_parent_context <- function(candidates, chunks = NULL, max_parent_chunks = 2L) {
  if (!is.data.frame(candidates) || nrow(candidates) == 0) {
    return(candidates)
  }
  chunks <- chunks %||% load_rag_chunks()
  chunks <- coerce_chunk_schema(chunks)
  parent_ids <- candidates$parent_id[!is.na(candidates$parent_id) & nzchar(candidates$parent_id)]
  if (length(parent_ids) == 0) {
    return(candidates)
  }

  parent_context <- chunks %>%
    filter(parent_id %in% parent_ids, !chunk_id %in% candidates$chunk_id) %>%
    group_by(parent_id) %>%
    slice_head(n = max_parent_chunks) %>%
    ungroup() %>%
    mutate(
      semantic_score = 0,
      keyword_score = 0,
      notation_match_boost = 0,
      source_priority_boost = 0,
      source_policy_boost = 0,
      module_match_boost = 0,
      current_module_match_boost = 0,
      selected_module_match_boost = 0,
      concept_tag_boost = 0,
      outside_selected_modules_penalty = 0,
      wrong_module_penalty = 0,
      duplicate_penalty = 0,
      final_score = 0,
      retrieval_methods = "parent_context",
      module_policy = "parent_context"
    )

  bind_rows(candidates, parent_context) %>%
    distinct(chunk_id, .keep_all = TRUE)
}

retrieve_evidence <- function(query,
                              active_module_id = NULL,
                              active_module_ids = NULL,
                              current_module_id = NULL,
                              mode = "general",
                              professor_id = NULL,
                              top_k = 8L,
                              expected_concept_tag = NULL) {
  total_start <- Sys.time()
  normalized_query <- normalize_student_query(query)
  expanded_queries <- expand_query(query)
  selected_modules <- normalize_rag_module_ids(active_module_ids, query = query)
  legacy_active <- normalize_rag_module_id(active_module_id, query = query)
  current <- normalize_rag_module_id(current_module_id %||% legacy_active, query = query)
  if (length(selected_modules) == 0 && !is.null(legacy_active)) {
    selected_modules <- legacy_active
  }
  inferred_module_id <- route_question_to_module(query, active_module_ids = selected_modules)
  if (is.null(current)) {
    if (!is.na(inferred_module_id) && nzchar(inferred_module_id) &&
        (length(selected_modules) == 0 || inferred_module_id %in% selected_modules)) {
      current <- inferred_module_id
    } else if (length(selected_modules) == 1) {
      current <- selected_modules[[1]]
    }
  }

  candidates <- retrieve_candidates(
    query = query,
    mode = mode,
    professor_id = professor_id,
    top_k = max(30L, top_k * 4L),
    active_module_id = current,
    active_module_ids = selected_modules,
    current_module_id = current,
    expected_concept_tag = expected_concept_tag
  )
  retrieval_timings <- attr(candidates, "retrieval_timings") %||% list()

  active <- current
  related <- get_related_modules(active)
  active_max <- if (!is.null(active) && nrow(candidates) > 0) {
    suppressWarnings(max(candidates$final_score[candidates$module_id == active], na.rm = TRUE))
  } else {
    NA_real_
  }
  if (is.infinite(active_max)) active_max <- NA_real_
  selected_max <- if (length(selected_modules) > 0 && nrow(candidates) > 0) {
    suppressWarnings(max(candidates$final_score[candidates$module_id %in% selected_modules], na.rm = TRUE))
  } else {
    NA_real_
  }
  if (is.infinite(selected_max)) selected_max <- NA_real_

  expanded_outside_active <- FALSE
  expanded_outside_selected <- FALSE
  if (!is.null(active) && !is.na(active_max) && active_max >= 2.0) {
    selected <- candidates %>%
      filter(module_id == active | (module_id %in% selected_modules & final_score >= (active_max - 1.5)) | (module_id %in% related & final_score >= (active_max - 1.25)))
  } else if (length(selected_modules) > 0 && !is.na(selected_max) && selected_max >= 2.0) {
    expanded_outside_active <- TRUE
    selected <- candidates %>%
      filter(module_id %in% selected_modules | (module_id %in% related & final_score >= (selected_max - 1.25)))
  } else if (length(selected_modules) > 0) {
    expanded_outside_active <- TRUE
    expanded_outside_selected <- TRUE
    selected <- candidates %>%
      filter(module_id %in% selected_modules | row_number() <= top_k)
  } else {
    selected <- candidates
  }

  evidence <- selected %>%
    arrange(desc(final_score)) %>%
    slice_head(n = top_k)

  evidence <- expand_parent_context(evidence) %>%
    arrange(desc(final_score)) %>%
    slice_head(n = top_k)

  evidence <- evidence %>%
    ensure_rerank_metadata_cols() %>%
    ensure_rerank_score_cols()

  list(
    query = query,
    normalized_query = normalized_query,
    expanded_queries = expanded_queries,
    intent = classify_query_intent(query),
    active_module_id = active,
    current_module_id = active,
    active_module_ids = selected_modules,
    inferred_module_id = inferred_module_id,
    related_modules = related,
    expanded_outside_active = expanded_outside_active || any(!is.null(active) & evidence$module_id != active & evidence$module_id %in% related),
    expanded_outside_selected = expanded_outside_selected || any(length(selected_modules) > 0 & !(evidence$module_id %in% selected_modules)),
    evidence = evidence,
    retrieval_time = retrieval_timings$total_candidate_time %||% retrieval_elapsed_seconds(total_start),
    rerank_time = retrieval_timings$rerank_time %||% NA_real_,
    retrieval_trace = evidence %>%
      transmute(
        chunk_id,
        source_name,
        source_type,
        source_scope,
        module_id,
        topic_id,
        concept_tag,
        semantic_score,
        keyword_score,
        module_match_boost,
        current_module_match_boost,
        selected_module_match_boost,
        concept_tag_boost,
        current_question_concept_boost,
        source_priority_boost,
        source_policy_boost,
        notation_match_boost,
        outside_selected_modules_penalty,
        wrong_module_penalty,
        unrelated_concept_penalty,
        duplicate_penalty,
        final_score,
        module_policy,
        retrieval_methods
      )
  )
}
