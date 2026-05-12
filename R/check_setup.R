intro_stats_required_paths <- function() {
  c(
    "app.R",
    "DESCRIPTION",
    "renv.lock",
    "R/aliases.R",
    "R/retrieval.R",
    "R/tutor.R",
    "R/ingest_textbook.R",
    "R/overlays.R",
    "R/images.R",
    "R/visual_helpers.R",
    "R/evals_vitals.R",
    "R/edge_case_tests.R",
    "R/performance_check.R",
    "data/raw",
    "data/processed",
    "data/processed/public_demo_chunks.csv",
    "data/wiki",
    "www",
    ".Renviron.example",
    ".gitignore",
    "AGENTS.md"
  )
}

intro_stats_launch_packages <- function() {
  c(
    "shiny", "bslib", "DBI", "RSQLite", "dplyr", "tidyr", "tibble",
    "purrr", "stringr", "readr", "jsonlite", "glue", "htmltools", "fs",
    "markdown"
  )
}

intro_stats_optional_packages <- function() {
  c("ellmer", "vitals", "pdftools", "officer", "readxl", "digest", "text2vec", "yaml", "DT", "ragnar", "ggplot2")
}

intro_stats_required_functions <- function() {
  list(
    aliases = c("build_alias_table", "normalize_student_query", "normalize_chunk_text", "expand_query"),
    retrieval = c(
      "retrieve_evidence", "retrieve_candidates", "keyword_retrieve", "dense_retrieve",
      "merge_retrieval_results", "rerank_chunks", "apply_source_policy",
      "apply_module_policy", "expand_parent_context", "route_question_to_module",
      "get_related_modules"
    ),
    tutor = c(
      "build_grounded_prompt", "generate_grounded_feedback", "generate_practice_feedback",
      "generate_contextual_practice_help", "build_practice_help_prompt",
      "build_conversational_tutor_prompt", "generate_followup_response",
      "summarize_recent_tutor_context", "verify_faithfulness", "maybe_refuse_or_clarify"
    ),
    vitals = c("build_vitals_dataset", "run_vitals_eval", "open_vitals_view"),
    images = c(
      "create_image_metadata_table", "load_image_metadata",
      "retrieve_relevant_visuals", "choose_visual_for_answer",
      "can_use_local_textbook_visuals", "get_best_available_visual",
      "detect_visual_request"
    ),
    visual_helpers = c(
      "is_visual_request", "choose_visual_type", "plot_mean_vs_median_skew",
      "visual_caption_for_type", "visual_response_for_type", "render_stat2331_visual"
    )
  )
}

intro_stats_function_files <- function() {
  c(
    aliases = "R/aliases.R",
    retrieval = "R/retrieval.R",
    tutor = "R/tutor.R",
    vitals = "R/evals_vitals.R",
    images = "R/images.R",
    visual_helpers = "R/visual_helpers.R"
  )
}

extract_defined_functions <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }
  lines <- readLines(path, warn = FALSE)
  matches <- regexec("^\\s*([A-Za-z0-9_.]+)\\s*<-\\s*function\\s*\\(", lines)
  found <- regmatches(lines, matches)
  unique(vapply(found[lengths(found) > 1], `[[`, character(1), 2))
}

extract_app_sources <- function(path = "app.R") {
  if (!file.exists(path)) {
    return(character())
  }
  lines <- readLines(path, warn = FALSE)
  matches <- regexec("^\\s*source\\(['\"]([^'\"]+)['\"]\\)", lines)
  found <- regmatches(lines, matches)
  vapply(found[lengths(found) > 1], `[[`, character(1), 2)
}

scan_for_potential_secrets <- function(paths = c("app.R", "R", ".Renviron.example", "README.md", "AGENTS.md")) {
  files <- unlist(lapply(paths[file.exists(paths)], function(path) {
    if (dir.exists(path)) {
      list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
    } else {
      path
    }
  }), use.names = FALSE)
  files <- files[!grepl("(^|/|\\\\)\\.Renviron$", files)]
  patterns <- c(
    "sk-[A-Za-z0-9_-]{20,}",
    "Bearer\\s+[A-Za-z0-9._-]{20,}",
    "(ANTHROPIC_API_KEY|OPENAI_API_KEY)\\s*=\\s*['\"][^'\"]{8,}['\"]"
  )
  placeholder_patterns <- c(
    "your-key-here",
    "your[_-]?[a-z0-9_-]*key",
    "api[_-]?key[_-]?here",
    "placeholder",
    "example[_-]?key",
    "YOUR_[A-Z0-9_]+"
  )
  hits <- character()
  for (file in files) {
    lines <- readLines(file, warn = FALSE)
    for (pattern in patterns) {
      idx <- grep(pattern, lines, perl = TRUE)
      if (length(idx) > 0) {
        placeholder_idx <- vapply(lines[idx], function(line) {
          any(grepl(paste(placeholder_patterns, collapse = "|"), line, ignore.case = TRUE, perl = TRUE))
        }, logical(1))
        idx <- idx[!placeholder_idx]
      }
      if (length(idx) > 0) {
        hits <- c(hits, paste0(file, ":", idx))
      }
    }
  }
  unique(hits)
}

intro_stats_data_status <- function() {
  list(
    textbook_chunks = file.exists("data/processed/textbook_chunks.rds") || file.exists("data/processed/textbook_chunks.csv"),
    public_demo_corpus = file.exists("data/processed/public_demo_chunks.csv"),
    retrieval_index = file.exists("data/processed/retrieval_index.rds"),
    concept_page_folder = dir.exists("data/wiki/concept_pages") || dir.exists("data/wiki"),
    question_bank = file.exists("data/processed/question_bank.csv"),
    image_metadata = file.exists("data/processed/image_metadata.rds") || dir.exists("data/visuals") || dir.exists("www/visuals")
  )
}

# Backward-compatible aliases for earlier STAT 2331-specific scripts.
stat2331_required_paths <- intro_stats_required_paths
stat2331_launch_packages <- intro_stats_launch_packages
stat2331_optional_packages <- intro_stats_optional_packages
stat2331_required_functions <- intro_stats_required_functions
stat2331_function_files <- intro_stats_function_files
stat2331_data_status <- intro_stats_data_status

check_setup <- function(verbose = TRUE) {
  required_paths <- intro_stats_required_paths()
  path_status <- stats::setNames(file.exists(required_paths) | dir.exists(required_paths), required_paths)

  app_sources <- extract_app_sources()
  source_status <- stats::setNames(file.exists(app_sources), app_sources)

  launch_packages <- intro_stats_launch_packages()
  optional_packages <- intro_stats_optional_packages()
  launch_missing <- launch_packages[!vapply(launch_packages, requireNamespace, logical(1), quietly = TRUE)]
  optional_missing <- optional_packages[!vapply(optional_packages, requireNamespace, logical(1), quietly = TRUE)]

  function_files <- intro_stats_function_files()
  required_functions <- intro_stats_required_functions()
  function_status <- lapply(names(required_functions), function(group) {
    defined <- extract_defined_functions(function_files[[group]])
    stats::setNames(required_functions[[group]] %in% defined, required_functions[[group]])
  })
  names(function_status) <- names(required_functions)

  parse_files <- c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE))
  parse_status <- stats::setNames(vapply(parse_files, function(file) {
    tryCatch({
      parse(file)
      TRUE
    }, error = function(e) FALSE)
  }, logical(1)), parse_files)

  env_lines <- if (file.exists(".Renviron.example")) readLines(".Renviron.example", warn = FALSE) else character()
  env_status <- c(
    ANTHROPIC_API_KEY = any(grepl("^ANTHROPIC_API_KEY=", env_lines)),
    OPENAI_API_KEY = any(grepl("^OPENAI_API_KEY=", env_lines)),
    STAT2331_DEV_MODE = any(grepl("^STAT2331_DEV_MODE=", env_lines)),
    STAT2331_LOCAL_TEXTBOOK_VISUALS = any(grepl("^STAT2331_LOCAL_TEXTBOOK_VISUALS=", env_lines)),
    STAT2331_MULTIMODAL_VISUAL_EXPLANATIONS = any(grepl("^STAT2331_MULTIMODAL_VISUAL_EXPLANATIONS=", env_lines))
  )

  secret_hits <- scan_for_potential_secrets()
  data_status <- intro_stats_data_status()

  result <- list(
    paths = path_status,
    app_sources = source_status,
    launch_packages_missing = launch_missing,
    optional_packages_missing = optional_missing,
    functions = function_status,
    parse = parse_status,
    env = env_status,
    potential_secret_hits = secret_hits,
    data = data_status,
    ok_to_launch = all(path_status) &&
      all(source_status) &&
      length(launch_missing) == 0 &&
      all(unlist(function_status, use.names = FALSE)) &&
      all(parse_status) &&
      length(secret_hits) == 0
  )

  if (isTRUE(verbose)) {
    cat("introductory statistics setup check\n")
    cat("=====================\n")
    cat("Required paths:", if (all(path_status)) "PASS" else "FAIL", "\n")
    if (any(!path_status)) cat("Missing:", paste(names(path_status)[!path_status], collapse = ", "), "\n")
    cat("app.R source files:", if (all(source_status)) "PASS" else "FAIL", "\n")
    if (any(!source_status)) cat("Missing sourced files:", paste(names(source_status)[!source_status], collapse = ", "), "\n")
    cat("Launch packages:", if (length(launch_missing) == 0) "PASS" else "MISSING", "\n")
    if (length(launch_missing) > 0) {
      cat("Install launch packages with:\n")
      cat("install.packages(c(", paste(shQuote(launch_missing), collapse = ", "), "))\n", sep = "")
    }
    if (length(optional_missing) > 0) {
      cat("Optional packages not installed:", paste(optional_missing, collapse = ", "), "\n")
    }
    cat("Required function definitions:", if (all(unlist(function_status, use.names = FALSE))) "PASS" else "FAIL", "\n")
    cat("Parse check:", if (all(parse_status)) "PASS" else "FAIL", "\n")
    if (any(!parse_status)) cat("Parse failures:", paste(names(parse_status)[!parse_status], collapse = ", "), "\n")
    cat(".Renviron.example placeholders:", if (all(env_status)) "PASS" else "CHECK", "\n")
    if (length(secret_hits) > 0) {
      cat("Potential hard-coded secret hits:", paste(secret_hits, collapse = ", "), "\n")
    } else {
      cat("Potential hard-coded secrets: PASS\n")
    }
    cat("Processed data:\n")
    for (nm in names(data_status)) {
      cat(" - ", nm, ": ", if (isTRUE(data_status[[nm]])) "present" else "missing/empty-safe", "\n", sep = "")
    }
    cat("Overall:", if (isTRUE(result$ok_to_launch)) "PASS" else "CHECK REQUIRED", "\n")
  }

  invisible(result)
}
