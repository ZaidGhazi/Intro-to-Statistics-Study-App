library(dplyr)
library(fs)
library(purrr)
library(stringr)
library(tibble)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

if (!exists("get_textbook_chapter_module_map", mode = "function") && file.exists("R/chunk_schema.R")) {
  source("R/chunk_schema.R")
}
if (!exists("normalize_student_query", mode = "function") && file.exists("R/aliases.R")) {
  source("R/aliases.R")
}
if ((!exists("route_question_to_module", mode = "function") ||
     !exists("normalize_rag_module_id", mode = "function") ||
     !exists("get_related_modules", mode = "function") ||
     !exists("tokenize_rag_text", mode = "function")) &&
    file.exists("R/retrieval.R")) {
  source("R/retrieval.R")
}

empty_image_metadata_table <- function() {
  tibble(
    image_id = character(),
    module_id = character(),
    topic_id = character(),
    concept_tag = character(),
    source_name = character(),
    source_type = character(),
    chapter = integer(),
    section = character(),
    page_number = integer(),
    caption = character(),
    nearby_text = character(),
    vision_description = character(),
    file_path = character(),
    thumbnail_path = character(),
    display_permission_status = character(),
    safe_for_deployment = logical(),
    tags = character(),
    preferred_use = character()
  )
}

default_recreated_visual_metadata <- function() {
  tibble(
    image_id = c(
      "mean_vs_median_skew",
      "outlier_boxplot",
      "recreated_bar_chart_categorical",
      "recreated_histogram_quantitative",
      "recreated_time_plot",
      "recreated_scatterplot_association",
      "recreated_standard_normal_curve",
      "recreated_p_value_tail_area",
      "recreated_confidence_interval_number_line",
      "sampling_distribution_clt"
    ),
    module_id = c(
      "descriptive_statistics",
      "descriptive_statistics",
      "data_graphs",
      "data_graphs",
      "data_graphs",
      "scatterplots_correlation",
      "normal_distribution",
      "hypothesis_testing",
      "confidence_intervals",
      "sampling_distributions"
    ),
    topic_id = c(
      "descriptive_stats",
      "descriptive_stats",
      "data_graphs",
      "data_graphs",
      "data_graphs",
      "relationships_regression",
      "normal_dist",
      "ht_foundations",
      "ci_prop",
      "sampling_dist"
    ),
    concept_tag = c(
      "resistant_measures",
      "skewness_outliers",
      "graph_selection",
      "graph_selection",
      "graph_selection",
      "association",
      "z_score_interpretation",
      "p_value_interpretation",
      "margin_of_error",
      "standard_error_sample_size"
    ),
    source_name = "recreated_visual_placeholder",
    source_type = "recreated_visual",
    chapter = c(2L, 2L, 1L, 1L, 1L, 4L, 3L, 15L, 14L, 11L),
    section = NA_character_,
    page_number = NA_integer_,
    caption = c(
      "Recreated right-skewed distribution showing mean and median",
      "Recreated boxplot showing high outliers",
      "Recreated bar chart showing counts for categorical groups",
      "Recreated histogram showing adjacent bins for quantitative data",
      "Recreated time plot showing connected values over time",
      "Recreated scatterplot showing a positive association",
      "Recreated standard normal curve with mean 0 and z-score markings",
      "Recreated p-value tail area sketch for hypothesis testing",
      "Recreated confidence interval number line with estimate and margin of error",
      "Recreated sampling distributions showing reduced spread with larger n"
    ),
    nearby_text = c(
      "resistant measures mean median skewed right skewed outliers measure of center extreme values",
      "outlier boxplot extreme values resistant nonresistant spread skewness",
      "bar chart categorical variable counts categories graph",
      "histogram quantitative variable bins frequency distribution graph",
      "time plot time series graph measurements over time trend change line plot",
      "scatterplot association explanatory response correlation regression graph",
      "normal curve bell curve standard normal z score density graph",
      "p-value tail area hypothesis test significance alpha normal curve graph",
      "confidence interval margin of error estimate plausible values number line graph",
      "sampling distribution central limit theorem standard error sample size larger samples narrower spread"
    ),
    vision_description = c(
      "A deploy-safe histogram of a right-skewed distribution with mean and median lines. The mean is pulled toward the right tail while the median stays closer to the main cluster.",
      "A deploy-safe boxplot with high outliers separated from the main cluster.",
      "A deploy-safe bar chart with separate bars for categories and count on the y-axis.",
      "A deploy-safe histogram with touching bars that represent quantitative intervals or bins.",
      "A deploy-safe time plot with time on the horizontal axis and connected measurements showing change over time.",
      "A deploy-safe scatterplot with points rising from left to right to show positive association.",
      "A deploy-safe visual placeholder for drawing a bell-shaped standard normal curve centered at 0 with shaded tail regions.",
      "A deploy-safe visual placeholder for drawing a null distribution with a shaded p-value tail area.",
      "A deploy-safe visual placeholder for drawing an estimate with lower and upper confidence interval endpoints.",
      "A deploy-safe sampling distribution visual showing that larger sample sizes have smaller standard error."
    ),
    file_path = c(
      "www/visuals/recreated/mean_vs_median_skew.png",
      "www/visuals/recreated/outlier_boxplot.png",
      "www/visuals/recreated/bar_chart_categorical.svg",
      "www/visuals/recreated/histogram_quantitative.svg",
      "www/visuals/recreated/time_plot.svg",
      "www/visuals/recreated/scatterplot_association.svg",
      "www/visuals/recreated/normal_curve_shading.svg",
      "www/visuals/recreated/p_value_tail_area.svg",
      "www/visuals/recreated/confidence_interval_number_line.svg",
      "www/visuals/recreated/sampling_distribution_clt.png"
    ),
    thumbnail_path = NA_character_,
    display_permission_status = "created_by_us",
    safe_for_deployment = TRUE,
    tags = c(
      "resistant_measures|mean_vs_median|skewness_outliers|measure_of_center|outliers",
      "boxplot|outliers|extreme_values|skewness_outliers",
      "bar_chart|categorical|counts|graph_selection",
      "histogram|quantitative|bins|frequency|graph_selection",
      "time_plot|time_series|trend|over_time|graph_selection",
      "scatterplot|association|correlation|regression",
      "normal_distribution|standard_normal|z_score|normal_curve|density_curve",
      "hypothesis_test|p_value|statistical_significance|normal_distribution",
      "confidence_interval|margin_of_error|estimate",
      "sampling_distribution|central_limit_theorem|standard_error|sample_size"
    ),
    preferred_use = "both"
  )
}

load_saved_image_metadata <- function(path = "data/processed/image_metadata.rds") {
  if (!file.exists(path)) {
    return(empty_image_metadata_table())
  }
  metadata <- tryCatch(readRDS(path), error = function(e) empty_image_metadata_table())
  if (!is.data.frame(metadata)) {
    return(empty_image_metadata_table())
  }
  missing_cols <- setdiff(names(empty_image_metadata_table()), names(metadata))
  for (col in missing_cols) {
      metadata[[col]] <- switch(
        col,
        page_number = NA_integer_,
        chapter = NA_integer_,
        safe_for_deployment = FALSE,
        preferred_use = "both",
        NA_character_
      )
  }
  if ("concept_tags" %in% names(metadata) &&
      (!"tags" %in% names(metadata) || all(is.na(metadata$tags) | !nzchar(as.character(metadata$tags %||% ""))))) {
    metadata$tags <- metadata$concept_tags
  }
  if ("concept_tags" %in% names(metadata) &&
      (!"concept_tag" %in% names(metadata) || all(is.na(metadata$concept_tag) | !nzchar(as.character(metadata$concept_tag %||% ""))))) {
    metadata$concept_tag <- map_chr(str_split(as.character(metadata$concept_tags %||% ""), "\\|"), ~ .x[[1]] %||% NA_character_)
  }
  metadata %>%
    select(any_of(names(empty_image_metadata_table())))
}

infer_image_permission <- function(path) {
  path <- as.character(path %||% "")
  case_when(
    str_detect(path, regex("created|recreated|ours|www", ignore_case = TRUE)) ~ "created_by_us",
    str_detect(path, regex("open_license|cc_by|creative_commons", ignore_case = TRUE)) ~ "open_license",
    str_detect(path, regex("textbook|raw|extracted", ignore_case = TRUE)) ~ "local_only",
    TRUE ~ "unknown"
  )
}

create_image_metadata_table <- function(paths = c("data/visuals", "www/visuals"),
                                        include_placeholders = TRUE,
                                        saved_metadata_path = "data/processed/image_metadata.rds") {
  saved_metadata <- load_saved_image_metadata(saved_metadata_path)
  existing_dirs <- paths[fs::dir_exists(paths)]
  if (length(existing_dirs) == 0) {
    image_table <- empty_image_metadata_table()
    if (isTRUE(include_placeholders)) {
      image_table <- bind_rows(image_table, default_recreated_visual_metadata())
    }
    return(bind_rows(saved_metadata, image_table) %>% distinct(image_id, .keep_all = TRUE))
  }

  image_files <- map(existing_dirs, ~ fs::dir_ls(.x, recurse = TRUE, type = "file", regexp = "\\.(png|jpg|jpeg|webp|gif|svg)$")) %>%
    unlist(use.names = FALSE) %>%
    as.character()
  if (length(image_files) == 0) {
    image_table <- empty_image_metadata_table()
    if (isTRUE(include_placeholders)) {
      image_table <- bind_rows(image_table, default_recreated_visual_metadata())
    }
    return(bind_rows(saved_metadata, image_table) %>% distinct(image_id, .keep_all = TRUE))
  }

  scanned_metadata <- tibble(file_path = image_files) %>%
    mutate(
      image_id = paste0("img_", str_replace_all(fs::path_ext_remove(fs::path_file(file_path)), "[^A-Za-z0-9]+", "_")),
      page_number = suppressWarnings(as.integer(str_match(file_path, regex("p(?:age)?_?([0-9]+)", ignore_case = TRUE))[, 2])),
      chapter = suppressWarnings(as.integer(str_match(file_path, regex("ch(?:apter)?_?([0-9]+)", ignore_case = TRUE))[, 2])),
      module_id = if_else(!is.na(chapter), get_textbook_chapter_module_map()$module_id[match(chapter, get_textbook_chapter_module_map()$chapter)], NA_character_),
      topic_id = NA_character_,
      concept_tag = NA_character_,
      source_name = case_when(
        str_detect(file_path, regex("textbook", ignore_case = TRUE)) ~ "course_textbook",
        str_detect(file_path, regex("www", ignore_case = TRUE)) ~ "app_visual",
        TRUE ~ "local_visual"
      ),
      source_type = case_when(
        str_detect(file_path, regex("textbook|extracted|local_only", ignore_case = TRUE)) ~ "textbook_figure",
        str_detect(file_path, regex("recreated|created", ignore_case = TRUE)) ~ "recreated_visual",
        str_detect(file_path, regex("open_license|cc_by|creative_commons", ignore_case = TRUE)) ~ "open_license_visual",
        TRUE ~ "other"
      ),
      section = NA_character_,
      caption = str_replace_all(fs::path_ext_remove(fs::path_file(file_path)), "[_-]+", " ") %>% str_squish(),
      nearby_text = "",
      vision_description = "",
      thumbnail_path = NA_character_,
      display_permission_status = map_chr(file_path, infer_image_permission),
      safe_for_deployment = display_permission_status %in% c("created_by_us", "open_license"),
      tags = "",
      preferred_use = "both"
    ) %>%
    select(
      image_id, module_id, topic_id, concept_tag, source_name, source_type,
      chapter, section, page_number, caption, nearby_text,
      vision_description, file_path, thumbnail_path,
      display_permission_status, safe_for_deployment, tags, preferred_use
    )

  if (isTRUE(include_placeholders)) {
    scanned_metadata <- bind_rows(scanned_metadata, default_recreated_visual_metadata())
  }
  bind_rows(saved_metadata, scanned_metadata) %>%
    distinct(image_id, .keep_all = TRUE)
}

tag_image_to_concept <- function(image_table,
                                 image_id,
                                 module_id = NULL,
                                 topic_id = NULL,
                                 concept_tags = NULL,
                                 caption = NULL,
                                 vision_description = NULL,
                                 display_permission_status = NULL) {
  image_table <- image_table %||% create_image_metadata_table()
  if (!is.data.frame(image_table) || nrow(image_table) == 0) {
    return(image_table)
  }
  target_id <- image_id
  new_module_id <- module_id
  new_concept_tags <- if (!is.null(concept_tags)) paste(concept_tags, collapse = "|") else NULL
  new_caption <- caption
  new_vision_description <- vision_description
  new_permission <- display_permission_status
  row_matches <- image_table$image_id == target_id
  if (!is.null(new_module_id)) {
    image_table$module_id[row_matches] <- new_module_id
  }
  if (!is.null(topic_id) && "topic_id" %in% names(image_table)) {
    image_table$topic_id[row_matches] <- topic_id
  }
  if (!is.null(new_concept_tags)) {
    image_table$tags[row_matches] <- new_concept_tags
    image_table$concept_tag[row_matches] <- str_split(new_concept_tags, "\\|")[[1]][[1]] %||% NA_character_
  }
  if (!is.null(new_caption)) {
    image_table$caption[row_matches] <- new_caption
  }
  if (!is.null(new_vision_description)) {
    image_table$vision_description[row_matches] <- new_vision_description
  }
  if (!is.null(new_permission)) {
    image_table$display_permission_status[row_matches] <- new_permission
  }
  image_table$safe_for_deployment <- image_table$display_permission_status %in% c("created_by_us", "open_license")
  image_table
}

load_image_metadata <- function(path = "data/processed/image_metadata.rds",
                                include_placeholders = TRUE,
                                paths = c("data/visuals", "www/visuals")) {
  create_image_metadata_table(
    paths = paths,
    include_placeholders = include_placeholders,
    saved_metadata_path = path
  )
}

can_use_local_textbook_visuals <- function() {
  value <- getOption(
    "stat2331.local_textbook_visuals",
    Sys.getenv("STAT2331_LOCAL_TEXTBOOK_VISUALS", unset = "true")
  )
  if (is.logical(value)) {
    return(isTRUE(value))
  }
  tolower(as.character(value)) %in% c("1", "true", "yes", "on", "local")
}

is_visual_safe_to_show <- function(visual_row, local_textbook_visuals = can_use_local_textbook_visuals()) {
  if (!is.data.frame(visual_row) || nrow(visual_row) == 0) {
    return(FALSE)
  }
  permission <- visual_row$display_permission_status[[1]] %||% "unknown"
  safe <- isTRUE(visual_row$safe_for_deployment[[1]] %||% FALSE)
  if (safe || permission %in% c("created_by_us", "open_license")) {
    return(TRUE)
  }
  if (isTRUE(local_textbook_visuals) && permission %in% c("local_only", "unknown")) {
    return(TRUE)
  }
  FALSE
}

get_visual_path <- function(visual_row, prefer_thumbnail = FALSE) {
  if (!is.data.frame(visual_row) || nrow(visual_row) == 0) {
    return(NA_character_)
  }
  path <- if (isTRUE(prefer_thumbnail) && "thumbnail_path" %in% names(visual_row) &&
              nzchar(visual_row$thumbnail_path[[1]] %||% "")) {
    visual_row$thumbnail_path[[1]]
  } else {
    visual_row$file_path[[1]] %||% NA_character_
  }
  if (is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }
  path
}

visual_file_exists <- function(visual_row) {
  path <- get_visual_path(visual_row)
  !is.na(path) && nzchar(path) && fs::file_exists(path)
}

detect_visual_request <- function(message) {
  normalized <- normalize_student_query(message %||% "")
  str_detect(normalized, "visual|graph|plot|chart|diagram|figure|picture|draw|show this|show me")
}

visual_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(default)
  }
  value <- str_squish(as.character(x[[1]]))
  if (!nzchar(value) || is.na(value)) {
    return(default)
  }
  value
}

score_visual_match <- function(query, image_table, concept_tag = NULL, module_id = NULL, current_question = NULL) {
  query <- paste(
    visual_scalar(query, ""),
    visual_scalar(current_question$question_text, ""),
    visual_scalar(concept_tag %||% current_question$concept_tag, ""),
    visual_scalar(module_id %||% current_question$module_id, ""),
    collapse = " "
  )
  normalized_query <- normalize_student_query(query)
  tokens <- tokenize_rag_text(normalized_query)
  image_text <- normalize_chunk_text(paste(image_table$caption, image_table$nearby_text, image_table$vision_description, image_table$concept_tag, image_table$tags))
  map_dbl(image_text, function(text) {
    if (length(tokens) == 0) return(0)
    sum(tokens %in% str_split(text, "\\s+")[[1]])
  })
}

retrieve_relevant_visuals <- function(query = NULL,
                                      current_question = NULL,
                                      concept_tag = NULL,
                                      module_id = NULL,
                                      active_module_id = NULL,
                                      image_table = NULL,
                                      top_k = 3L,
                                      deployment_safe_only = FALSE,
                                      prefer_safe = !can_use_local_textbook_visuals()) {
  image_table <- image_table %||% load_image_metadata()
  if (!is.data.frame(image_table) || nrow(image_table) == 0) {
    return(empty_image_metadata_table())
  }
  current_module <- visual_scalar(module_id %||% current_question$module_id %||% active_module_id, NA_character_)
  active <- normalize_rag_module_id(current_module, query = query) %||%
    route_question_to_module(paste(visual_scalar(query, ""), visual_scalar(current_question$question_text, ""), collapse = " "))
  related <- get_related_modules(active)
  requested_concept <- visual_scalar(concept_tag %||% current_question$concept_tag, NA_character_)
  requested_concept_norm <- if (!is.na(requested_concept) && nzchar(requested_concept)) {
    normalize_chunk_text(requested_concept)
  } else {
    NA_character_
  }
  concept_has_request <- !is.na(requested_concept_norm) && nzchar(requested_concept_norm)
  linked_ids <- unique(c(
    current_question$visual_id %||% character(),
    current_question$visual_ids %||% character(),
    current_question$tutor_visual_ids %||% character()
  ))
  linked_ids <- linked_ids[!is.na(linked_ids) & nzchar(linked_ids)]
  visual_scores <- score_visual_match(query, image_table, concept_tag = requested_concept, module_id = active, current_question = current_question)

  scored <- image_table %>%
    mutate(
      visual_score = visual_scores,
      linked_boost = if_else(image_id %in% linked_ids, 8, 0),
      concept_text = normalize_chunk_text(paste(coalesce(.data$concept_tag, ""), coalesce(.data$tags, ""))),
      concept_boost = if (isTRUE(concept_has_request)) {
        if_else(str_detect(.data$concept_text, fixed(requested_concept_norm, ignore_case = TRUE)), 3, 0)
      } else {
        0
      },
      module_boost = 0,
      deployment_penalty = if_else((isTRUE(deployment_safe_only) | isTRUE(prefer_safe)) & !safe_for_deployment, 99, 0),
      final_visual_score = visual_score + linked_boost + concept_boost + module_boost - deployment_penalty
    ) %>%
    select(-concept_text)

  if (!is.null(active) && !is.na(active) && nzchar(active)) {
    scored <- scored %>%
      mutate(
        module_boost = case_when(
          module_id == active ~ 2,
          module_id %in% related ~ 0.75,
          TRUE ~ 0
        ),
        final_visual_score = visual_score + linked_boost + concept_boost + module_boost - deployment_penalty
      )
  }

  filtered <- scored %>%
    filter(final_visual_score > 0) %>%
    arrange(desc(final_visual_score))
  if (!is.data.frame(filtered) || nrow(filtered) == 0) {
    return(empty_image_metadata_table())
  }
  keep <- map_lgl(seq_len(nrow(filtered)), ~ is_visual_safe_to_show(filtered[.x, , drop = FALSE]))
  filtered[keep, , drop = FALSE] %>%
    slice_head(n = top_k)
}

choose_visual_for_answer <- function(visual_candidates = NULL,
                                     query = NULL,
                                     active_module_id = NULL,
                                     evidence = NULL,
                                     image_table = NULL,
                                     prefer_safe = !can_use_local_textbook_visuals(),
                                     deployment_safe_only = FALSE) {
  visuals <- if (is.data.frame(visual_candidates)) {
    visual_candidates
  } else {
    retrieve_relevant_visuals(
      query = query,
      active_module_id = active_module_id,
      image_table = image_table,
      top_k = 3L,
      deployment_safe_only = deployment_safe_only,
      prefer_safe = prefer_safe
    )
  }
  if (!is.data.frame(visuals) || nrow(visuals) == 0) {
    return(NULL)
  }
  if (!"final_visual_score" %in% names(visuals)) {
    visuals$final_visual_score <- if ("visual_score" %in% names(visuals)) visuals$visual_score else 0
  }
  visuals %>%
    arrange(desc(safe_for_deployment), desc(final_visual_score)) %>%
    slice_head(n = 1)
}

get_best_available_visual <- function(query = NULL,
                                      current_question = NULL,
                                      concept_tag = NULL,
                                      module_id = NULL,
                                      prefer_safe = !can_use_local_textbook_visuals(),
                                      top_k = 3L) {
  candidates <- retrieve_relevant_visuals(
    query = query,
    current_question = current_question,
    concept_tag = concept_tag,
    module_id = module_id,
    top_k = top_k,
    prefer_safe = prefer_safe
  )
  choose_visual_for_answer(candidates, prefer_safe = prefer_safe)
}

get_visual_explanation_context <- function(visuals) {
  if (!is.data.frame(visuals) || nrow(visuals) == 0) {
    return("")
  }
  pmap_chr(
    list(visuals$image_id, visuals$caption, visuals$vision_description, visuals$nearby_text),
    ~ str_squish(paste("visual_id:", ..1, "| caption:", ..2, "| description:", ..3, "| context:", ..4))
  ) %>%
    paste(collapse = "\n")
}

# TODO: Add figure extraction from textbook pages only after confirming local-only
# storage paths and deployment permission status. The table above is intentionally
# ready for extracted captions, recreated visuals, and future vision descriptions.

# -----------------------------------------------------------------------------
# 2026-05 Intro-statistics recreated visual metadata extension.
# This overrides the earlier default metadata function while preserving the same
# schema. All visuals listed here are deploy-safe recreated aids, not textbook art.
# -----------------------------------------------------------------------------
default_recreated_visual_metadata <- function() {
  tibble(
    image_id = c(
      "mean_vs_median_skew",
      "outlier_boxplot",
      "recreated_bar_chart_categorical",
      "recreated_histogram_quantitative",
      "recreated_time_plot",
      "recreated_scatterplot_association",
      "recreated_standard_normal_curve",
      "recreated_p_value_tail_area",
      "recreated_confidence_interval_number_line",
      "sampling_distribution_clt",
      "binomial_distribution_bars",
      "experiment_randomization_diagram",
      "two_way_table_segmented_bar",
      "regression_residual_plot",
      "comparing_groups_boxplots"
    ),
    module_id = c(
      "module_1", "module_1", "module_1", "module_1", "module_1", "module_2", "module_5",
      "module_8", "module_7", "module_6", "module_5", "module_3", "module_2",
      "module_2", "module_9"
    ),
    topic_id = c(
      "descriptive_stats", "descriptive_stats", "data_graphs", "data_graphs",
      "data_graphs", "relationships_regression", "normal_dist", "ht_foundations", "ci_prop",
      "sampling_dist", "binomial_dist", "producing_data", "relationships_regression",
      "relationships_regression", "uses_abuses_tests"
    ),
    concept_tag = c(
      "resistant_measures", "skewness_outliers", "graph_selection", "graph_selection",
      "graph_selection", "association", "z_score_interpretation", "p_value_interpretation", "margin_of_error",
      "standard_error_sample_size", "binomial_setting", "experimental_design", "two_way_tables",
      "residuals", "comparing_groups"
    ),
    source_name = "recreated_intro_stats_visual",
    source_type = "recreated_visual",
    chapter = c(2L, 2L, 1L, 1L, 1L, 4L, 3L, 15L, 14L, 11L, 13L, 9L, 6L, 5L, 19L),
    section = NA_character_,
    page_number = NA_integer_,
    caption = c(
      "Recreated right-skewed distribution showing mean and median",
      "Recreated boxplot showing high outliers",
      "Recreated bar chart showing counts for categorical groups",
      "Recreated histogram showing adjacent bins for quantitative data",
      "Recreated time plot showing connected values over time",
      "Recreated scatterplot showing a positive association",
      "Recreated standard normal curve with mean 0 and z-score markings",
      "Recreated p-value tail area sketch for hypothesis testing",
      "Recreated confidence interval number line with estimate and margin of error",
      "Recreated sampling distributions showing reduced spread with larger n",
      "Recreated binomial distribution bar chart for counts of successes",
      "Recreated random-assignment diagram for experiments",
      "Recreated segmented bar chart for comparing conditional distributions",
      "Recreated regression plot showing residuals as vertical gaps",
      "Recreated side-by-side boxplots for comparing groups"
    ),
    nearby_text = c(
      "resistant measures mean median skewed right skewed outliers measure of center extreme values",
      "outlier boxplot extreme values resistant nonresistant spread skewness",
      "bar chart categorical variable counts categories graph",
      "histogram quantitative variable bins frequency distribution graph",
      "time plot time series graph measurements over time trend change line plot",
      "scatterplot association explanatory response correlation regression graph",
      "normal curve bell curve standard normal z score density graph",
      "p-value tail area hypothesis test significance alpha normal curve graph",
      "confidence interval margin of error estimate plausible values number line graph",
      "sampling distribution central limit theorem standard error sample size larger samples narrower spread",
      "binomial distribution successes trials probability bars BINS",
      "experiment random assignment treatment control group placebo randomized comparative experiment",
      "two way table conditional distribution segmented bar graph categorical variables",
      "regression residual observed predicted least squares line scatterplot",
      "side by side boxplots comparing groups center spread outliers"
    ),
    vision_description = c(
      "A deploy-safe histogram of a right-skewed distribution with mean and median lines. The mean is pulled toward the right tail while the median stays closer to the main cluster.",
      "A deploy-safe boxplot with high outliers separated from the main cluster.",
      "A deploy-safe bar chart with separate bars for categories and count on the y-axis.",
      "A deploy-safe histogram with touching bars that represent quantitative intervals or bins.",
      "A deploy-safe time plot with time on the horizontal axis and connected measurements showing change over time.",
      "A deploy-safe scatterplot with points rising from left to right to show positive association.",
      "A deploy-safe bell-shaped standard normal curve centered at 0 with shaded tail regions.",
      "A deploy-safe null distribution with a shaded p-value tail area.",
      "A deploy-safe number line drawing an estimate with lower and upper confidence interval endpoints.",
      "A deploy-safe sampling distribution visual showing that larger sample sizes have smaller standard error.",
      "A deploy-safe binomial bar chart showing probabilities for each possible count of successes.",
      "A deploy-safe diagram showing subjects randomly assigned to treatment and control groups.",
      "A deploy-safe segmented bar chart comparing conditional distributions across two groups.",
      "A deploy-safe scatterplot with a regression line and residual segments.",
      "A deploy-safe set of side-by-side boxplots comparing distributions across groups."
    ),
    file_path = c(
      "www/visuals/recreated/mean_vs_median_skew.png",
      "www/visuals/recreated/outlier_boxplot.png",
      "www/visuals/recreated/bar_chart_categorical.svg",
      "www/visuals/recreated/histogram_quantitative.svg",
      "www/visuals/recreated/time_plot.svg",
      "www/visuals/recreated/scatterplot_association.svg",
      "www/visuals/recreated/normal_curve_shading.svg",
      "www/visuals/recreated/p_value_tail_area.svg",
      "www/visuals/recreated/confidence_interval_number_line.svg",
      "www/visuals/recreated/sampling_distribution_clt.png",
      "www/visuals/recreated/binomial_distribution_bars.svg",
      "www/visuals/recreated/experiment_randomization_diagram.svg",
      "www/visuals/recreated/two_way_table_segmented_bar.svg",
      "www/visuals/recreated/regression_residual_plot.svg",
      "www/visuals/recreated/comparing_groups_boxplots.svg"
    ),
    thumbnail_path = NA_character_,
    display_permission_status = "created_by_us",
    safe_for_deployment = TRUE,
    tags = c(
      "resistant_measures|mean_vs_median|skewness_outliers|measure_of_center|outliers",
      "boxplot|outliers|extreme_values|skewness_outliers",
      "bar_chart|categorical|counts|graph_selection",
      "histogram|quantitative|bins|frequency|graph_selection",
      "time_plot|time_series|trend|over_time|graph_selection",
      "scatterplot|association|correlation|regression",
      "normal_distribution|standard_normal|z_score|normal_curve|density_curve",
      "hypothesis_test|p_value|statistical_significance|normal_distribution",
      "confidence_interval|margin_of_error|estimate",
      "sampling_distribution|central_limit_theorem|standard_error|sample_size",
      "binomial_distribution|successes|trials|BINS",
      "experiment|random_assignment|treatment|control|placebo",
      "two_way_table|conditional_distribution|categorical_relationships",
      "regression|residuals|least_squares|scatterplot",
      "boxplots|comparing_groups|two_sample|outliers"
    ),
    preferred_use = "both"
  )
}
