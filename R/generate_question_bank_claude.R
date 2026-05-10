library(dplyr)
library(tibble)
library(purrr)
library(stringr)
library(glue)
library(readr)
library(jsonlite)
library(fs)

source("R/wiki.R")

QUESTION_BANK_OUTPUT_CSV <- "data/processed/question_bank.csv"
QUESTION_BANK_OUTPUT_JSON <- "data/processed/question_bank_raw.json"
QUESTION_BANK_RAW_DIR <- "data/processed/question_bank_raw"
QUESTION_BANK_ERRORS_CSV <- "data/processed/question_bank_generation_errors.csv"
DEFAULT_QBANK_MODEL <- "claude-sonnet-4-6"
VALID_FORMATS <- c("multiple_choice", "fill_in_blank", "choose_best_answer", "drag_and_drop")
VALID_DIFFICULTIES <- c("easy", "medium", "hard")
REQUIRED_QUESTION_FIELDS <- c(
  "question_id",
  "module_id",
  "module_label",
  "topic_id",
  "topic_label",
  "concept_tag",
  "difficulty",
  "format",
  "question_text",
  "choices",
  "interaction_type",
  "correct_choice_id",
  "correct_answer",
  "accepted_answers",
  "hint",
  "explanation",
  "source_basis",
  "review_status",
  "generated_by"
)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

first_or_default <- function(x, default = "") {
  values <- coerce_text_vector(x)
  if (length(values) == 0) default else values[[1]]
}

current_utc_timestamp <- function(time = Sys.time()) {
  base::format(as.POSIXct(time, tz = "UTC"), "%Y-%m-%d %H:%M:%S", tz = "UTC")
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

OFFICIAL_TOPIC_STRUCTURE <- tibble::tribble(
  ~module_id, ~module_label, ~topic_id, ~topic_label, ~module_order, ~topic_order,
  "module_1", "Module 1: Data Types, Graphs, and Descriptive Statistics", "data_graphs", "Data Types and Graphs", 1L, 1L,
  "module_1", "Module 1: Data Types, Graphs, and Descriptive Statistics", "descriptive_stats", "Descriptive Statistics", 1L, 2L,
  "module_2", "Module 2: Relationships, Association, and Regression", "relationships_regression", "Relationships and Regression", 2L, 1L,
  "module_3", "Module 3: Producing Data: Sampling and Experiments", "producing_data", "Producing Data", 3L, 1L,
  "module_4", "Module 4: Probability Basics", "probability_basics", "Probability Basics", 4L, 1L,
  "module_5", "Module 5: Normal and Binomial Distributions", "normal_dist", "Normal Distribution", 5L, 1L,
  "module_5", "Module 5: Normal and Binomial Distributions", "binomial_dist", "Binomial Distribution", 5L, 2L,
  "module_6", "Module 6: Sampling Distributions", "sampling_dist", "Sampling Distributions", 6L, 1L,
  "module_7", "Module 7: Confidence Intervals", "ci_prop", "Confidence Intervals for Proportions", 7L, 1L,
  "module_7", "Module 7: Confidence Intervals", "ci_mean", "Confidence Intervals for Means", 7L, 2L,
  "module_8", "Module 8: Hypothesis Testing", "ht_foundations", "Hypothesis Testing Foundations", 8L, 1L,
  "module_8", "Module 8: Hypothesis Testing", "ht_prop", "Hypothesis Tests for Proportions", 8L, 2L,
  "module_8", "Module 8: Hypothesis Testing", "ht_mean", "Hypothesis Tests for Means", 8L, 3L,
  "module_9", "Module 9: Uses and Abuses of Tests", "uses_abuses_tests", "Uses and Abuses of Tests", 9L, 1L,
  "cumulative_review", "Cumulative Review", "final_review", "Cumulative Review", 10L, 1L
)

normalize_module_id_value <- function(module_id) {
  value <- first_or_default(module_id, "") %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "")

  if (!nzchar(value)) {
    return("")
  }

  if (value %in% c("cumulative_review", "cumulative", "review", "final_review")) {
    return("cumulative_review")
  }

  module_num <- str_match(value, "^(?:mod|module)_?0*([1-9])$")[, 2]
  if (!is.na(module_num) && nzchar(module_num)) {
    return(glue("module_{module_num}"))
  }

  value
}

empty_question_bank <- function() {
  tibble(
    question_id = character(),
    module_id = character(),
    module_label = character(),
    topic_id = character(),
    topic_label = character(),
    concept_tag = character(),
    difficulty = character(),
    format = character(),
    question_text = character(),
    choices = list(),
    interaction_type = character(),
    correct_choice_id = character(),
    correct_answer = list(),
    accepted_answers = list(),
    hint = character(),
    explanation = character(),
    source_basis = character(),
    review_status = character(),
    generated_by = character()
  )
}

serialize_text_vector <- function(x) {
  values <- unlist(x %||% character(), use.names = FALSE) %>%
    as.character() %>%
    str_squish()
  values <- values[nzchar(values)]
  if (length(values) == 0) {
    ""
  } else {
    paste(values, collapse = "\n")
  }
}

sanitize_text_vector <- function(x) {
  values <- unlist(x %||% character(), use.names = FALSE) %>%
    as.character() %>%
    str_squish()
  values[nzchar(values)]
}

normalize_question_text <- function(x) {
  text <- as.character(x %||% "")
  text[is.na(text)] <- ""

  text %>%
    str_to_lower() %>%
    str_replace_all("[\r\n\t]+", " ") %>%
    str_replace_all("[“”]", "\"") %>%
    str_replace_all("[‘’]", "'") %>%
    str_replace_all("[.,;:!?]+", " ") %>%
    str_replace_all("[()\\[\\]{}]+", " ") %>%
    str_replace_all("[-_/]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
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

coerce_text_vector <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(character())
  }

  if (is.character(x) && length(x) == 1) {
    x_trim <- str_trim(x)
    parsed <- parse_json_safely(x_trim)
    if (!is.null(parsed)) {
      return(coerce_text_vector(parsed))
    }
    if (str_detect(x_trim, "\\n")) {
      return(
        str_split(x_trim, "\\n")[[1]] %>%
          str_squish() %>%
          discard(~ !nzchar(.x))
      )
    }
  }

  as.character(unlist(x, use.names = FALSE)) %>%
    str_squish() %>%
    discard(~ !nzchar(.x))
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
    return(list(list(
      id = as.character(x$id %||% ""),
      text = as.character(x$text %||% "")
    )))
  }

  if (is.list(x) && length(x) > 0 && all(map_lgl(x, is_choice_object))) {
    return(
      purrr::map(x, function(item) {
        list(
          id = as.character(item$id %||% ""),
          text = as.character(item$text %||% "")
        )
      })
    )
  }

  values <- coerce_text_vector(x)
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

serialize_json_vector <- function(x) {
  jsonlite::toJSON(as.character(coerce_text_vector(x)), auto_unbox = FALSE, null = "null")
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

normalize_drag_interaction_type <- function(format, question_text = NULL, interaction_type = NULL) {
  if (!identical(format, "drag_and_drop")) {
    return(NA_character_)
  }
  
  explicit_type <- first_or_default(interaction_type, "")
  if (nzchar(explicit_type) && explicit_type %in% VALID_DRAG_INTERACTION_TYPES) {
    return(explicit_type)
  }
  
  question_text <- str_to_lower(first_or_default(question_text, ""))
  if (str_detect(question_text, "arrange|put\\s+.*in\\s+order|sequence|first\\s+to\\s+last|order\\s+from")) {
    return("ordering")
  }
  if (str_detect(question_text, "categor|sort\\s+.*group|sort\\s+.*categor|group\\s+the|classify|match\\s+.*category")) {
    return("categorize")
  }
  
  "select_all"
}

normalize_interaction_type_value <- function(format, question_text = NULL, interaction_type = NULL) {
  fmt <- first_or_default(format, "")

  if (fmt %in% c("multiple_choice", "choose_best_answer")) {
    return("single_select")
  }

  if (identical(fmt, "fill_in_blank")) {
    return("fill_in_blank")
  }

  if (identical(fmt, "drag_and_drop")) {
    return(normalize_drag_interaction_type(
      format = fmt,
      question_text = question_text,
      interaction_type = interaction_type
    ))
  }

  first_or_default(interaction_type, "")
}

resolve_choice_value_to_id <- function(value, choices) {
  value <- first_or_default(value, "") %>% str_squish()
  if (!nzchar(value) || length(choices) == 0) {
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

coerce_drag_category_mapping <- function(x, choices = list()) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(setNames(character(), character()))
  }
  
  if (is.list(x) && length(x) > 0 && all(map_lgl(x, ~ is.list(.x) && !is.null(names(.x)) && all(c("id", "category") %in% names(.x))))) {
    ids <- map_chr(x, ~ as.character(.x$id %||% ""))
    categories <- map_chr(x, ~ as.character(.x$category %||% ""))
    ids <- vapply(ids, resolve_choice_value_to_id, choices = choices, FUN.VALUE = character(1))
    keep <- nzchar(ids) & nzchar(categories)
    return(stats::setNames(categories[keep], ids[keep]))
  }
  
  values <- x
  if (!is.character(values) || is.null(names(values))) {
    values <- coerce_text_vector(x)
  }
  
  mapping <- setNames(character(), character())
  
  if (is.character(values) && !is.null(names(values)) && any(nzchar(names(values)))) {
    ids <- vapply(names(values), resolve_choice_value_to_id, choices = choices, FUN.VALUE = character(1))
    categories <- as.character(values %||% "")
    keep <- nzchar(ids) & nzchar(categories)
    if (any(keep)) {
      mapping <- stats::setNames(categories[keep], ids[keep])
    }
  } else {
    parsed <- lapply(coerce_text_vector(values), function(entry) {
      match <- str_match(entry, "^\\s*(.*?)\\s*(::|=>|->|=|\\|)\\s*(.*?)\\s*$")
      if (is.na(match[[1, 1]])) {
        return(NULL)
      }
      choice_id <- resolve_choice_value_to_id(match[[1, 2]], choices)
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
  
  mapping[!duplicated(names(mapping))]
}

get_drag_grading_values <- function(correct_answer, choices, interaction_type = NULL, question_text = NULL) {
  interaction_type <- normalize_drag_interaction_type("drag_and_drop", question_text = question_text, interaction_type = interaction_type)
  
  if (identical(interaction_type, "categorize")) {
    return(coerce_drag_category_mapping(correct_answer, choices = choices))
  }
  
  answers <- coerce_text_vector(correct_answer)
  resolved <- vapply(answers, resolve_choice_value_to_id, choices = choices, FUN.VALUE = character(1))
  resolved <- resolved[nzchar(resolved)]
  
  if (identical(interaction_type, "select_all")) {
    return(unique(resolved))
  }
  
  resolved
}

get_drag_correct_answer_display <- function(correct_answer, choices, interaction_type = NULL, question_text = NULL) {
  interaction_type <- normalize_drag_interaction_type("drag_and_drop", question_text = question_text, interaction_type = interaction_type)
  
  if (identical(interaction_type, "categorize")) {
    mapping <- coerce_drag_category_mapping(correct_answer, choices = choices)
    if (length(mapping) == 0) {
      return(character())
    }
    ids <- names(mapping)
    texts <- get_choice_texts(choices)
    choice_ids <- get_choice_ids(choices)
    labels <- vapply(ids, function(choice_id) {
      match_idx <- match(choice_id, choice_ids)
      if (is.na(match_idx)) choice_id else texts[[match_idx]]
    }, FUN.VALUE = character(1))
    return(paste0(labels, " -> ", unname(mapping)))
  }
  
  ids <- get_drag_grading_values(correct_answer, choices, interaction_type = interaction_type, question_text = question_text)
  texts <- get_choice_texts(choices)
  choice_ids <- get_choice_ids(choices)
  vapply(ids, function(choice_id) {
    match_idx <- match(choice_id, choice_ids)
    if (is.na(match_idx)) choice_id else texts[[match_idx]]
  }, FUN.VALUE = character(1))
}

validate_raw_drag_payload <- function(correct_answer, choices, interaction_type, question_text = NULL) {
  interaction_type <- normalize_drag_interaction_type("drag_and_drop", question_text = question_text, interaction_type = interaction_type)
  choice_ids <- get_choice_ids(choices)
  
  if (length(choice_ids) < 2) {
    return("drag_and_drop questions must include at least two choices.")
  }
  
  if (identical(interaction_type, "ordering")) {
    raw_answers <- coerce_text_vector(correct_answer)
    if (length(choice_ids) < 3) {
      return("Ordering drag_and_drop questions must include at least three choices.")
    }
    if (length(raw_answers) != length(choice_ids) || !all(raw_answers %in% choice_ids) || anyDuplicated(raw_answers) || !setequal(raw_answers, choice_ids)) {
      return("Ordering drag_and_drop questions must use correct_answer as an ordered vector of all choice ids.")
    }
    return(NULL)
  }
  
  if (identical(interaction_type, "select_all")) {
    raw_answers <- coerce_text_vector(correct_answer)
    if (length(raw_answers) == 0 || !all(raw_answers %in% choice_ids) || anyDuplicated(raw_answers)) {
      return("Select-all drag_and_drop questions must use correct_answer as a vector of valid choice ids.")
    }
    return(NULL)
  }
  
  mapping <- coerce_drag_category_mapping(correct_answer, choices = choices)
  if (length(mapping) != length(choice_ids) || !setequal(names(mapping), choice_ids) || any(!nzchar(unname(mapping)))) {
    return("Categorize drag_and_drop questions must map every choice id to a category.")
  }
  if (length(unique(unname(mapping))) < 2) {
    return("Categorize drag_and_drop questions must use at least two categories.")
  }
  
  NULL
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

  answers <- coerce_text_vector(correct_answer)
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
  answers <- coerce_text_vector(correct_answer)

  if (format %in% c("multiple_choice", "choose_best_answer")) {
    ids <- get_choice_ids(choices)
    texts <- get_choice_texts(choices)
    resolved_id <- derive_correct_choice_id(correct_choice_id, answers, choices, format)
    match_idx <- match(resolved_id, ids)
    if (!is.na(match_idx)) {
      return(texts[[match_idx]])
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

is_valid_choice_object_list <- function(choices) {
  if (!is.list(choices)) {
    return(FALSE)
  }
  if (length(choices) == 0) {
    return(TRUE)
  }
  ids <- get_choice_ids(choices)
  texts <- get_choice_texts(choices)
  all(nzchar(ids) & nzchar(texts)) && !anyDuplicated(ids)
}

get_topic_spec <- function(topic_id) {
  spec <- OFFICIAL_TOPIC_STRUCTURE %>%
    filter(topic_id == !!topic_id) %>%
    slice_head(n = 1)
  if (nrow(spec) == 0) {
    stop(glue("Unknown topic_id: {topic_id}"), call. = FALSE)
  }
  spec
}

FILL_IN_BLANK_QUESTION_BLOCKLIST <- regex(
  paste(
    c(
      "z\\s*=",
      "sqrt",
      "\\bp_?0\\b",
      "p\\s*-?hat",
      "p̂",
      "\\bx\\s*-?bar\\b",
      "x̄",
      "\\bmu\\b",
      "μ",
      "\\bsigma\\b",
      "σ",
      "≤",
      "≥",
      "\\bformula\\b",
      "\\bexpression\\b"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

FILL_IN_BLANK_COMPLEX_ANSWER_PATTERN <- regex(
  paste(
    c(
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
      "\\bp_?0\\b",
      "p\\s*-?hat",
      "p̂",
      "\\bx\\s*-?bar\\b",
      "x̄",
      "\\bmu\\b",
      "μ",
      "\\bsigma\\b",
      "σ",
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

FILL_IN_BLANK_ROUNDING_PATTERN <- regex(
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

normalize_fill_answer <- function(x) {
  first_or_default(x, "") %>%
    as.character() %>%
    str_squish()
}

fill_in_blank_question_is_formula_heavy <- function(question_text) {
  text <- as.character(question_text)
  text[is.na(text)] <- ""
  text <- str_squish(text)

  has_text <- nzchar(text)
  result <- rep(FALSE, length(text))
  if (!any(has_text)) {
    return(result)
  }

  result[has_text] <- str_detect(text[has_text], FILL_IN_BLANK_QUESTION_BLOCKLIST) |
    str_count(text[has_text], "_{3,}") > 1 |
    str_detect(
      text[has_text],
      regex("enter your answer as|type the expression|type the formula", ignore_case = TRUE)
    )

  result
}

fill_in_blank_answer_is_numeric <- function(answer_text) {
  answer <- str_squish(as.character(answer_text %||% ""))
  if (!nzchar(answer)) {
    return(FALSE)
  }
  
  str_detect(answer, "^[+-]?(?:\\d+(?:\\.\\d+)?|\\.\\d+)$")
}

fill_in_blank_answer_is_simple_phrase <- function(answer_text) {
  answer <- str_squish(as.character(answer_text %||% ""))
  if (!nzchar(answer)) {
    return(FALSE)
  }
  
  !fill_in_blank_answer_is_numeric(answer) &&
    !str_detect(answer, FILL_IN_BLANK_COMPLEX_ANSWER_PATTERN) &&
    !str_detect(answer, "[\r\n]") &&
    str_count(answer, "\\S+") <= 6 &&
    str_detect(answer, "^[[:alnum:]' -]+$")
}

fill_in_blank_has_rounding_instruction <- function(question_text) {
  text <- str_squish(as.character(question_text %||% ""))
  nzchar(text) && str_detect(text, FILL_IN_BLANK_ROUNDING_PATTERN)
}

fill_in_blank_variant_is_allowed <- function(answer_text) {
  fill_in_blank_answer_is_numeric(answer_text) || fill_in_blank_answer_is_simple_phrase(answer_text)
}

# Runtime-safe override for stricter notation-heavy fill-in-the-blank rejection.
FILL_IN_BLANK_QUESTION_BLOCKLIST <- regex(
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

FILL_IN_BLANK_COMPLEX_ANSWER_PATTERN <- regex(
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

fill_in_blank_question_is_formula_heavy <- function(question_text) {
  text <- as.character(question_text)
  text[is.na(text)] <- ""
  text <- str_squish(text)

  has_text <- nzchar(text)
  result <- rep(FALSE, length(text))
  if (!any(has_text)) {
    return(result)
  }

  result[has_text] <- str_detect(text[has_text], FILL_IN_BLANK_QUESTION_BLOCKLIST) |
    str_count(text[has_text], "_{3,}") > 1 |
    str_detect(
      text[has_text],
      regex("enter your answer as|type the expression|type the formula|type the notation|write the notation", ignore_case = TRUE)
    )

  result
}

fill_in_blank_answer_is_simple_phrase <- function(answer_text) {
  answer <- str_squish(as.character(answer_text %||% ""))
  if (!nzchar(answer)) {
    return(FALSE)
  }
  
  !fill_in_blank_answer_is_numeric(answer) &&
    !str_detect(answer, FILL_IN_BLANK_COMPLEX_ANSWER_PATTERN) &&
    !str_detect(answer, "[\r\n]") &&
    str_count(answer, "\\S+") <= 6 &&
    str_detect(answer, "^[[:alnum:]' -]+$")
}

fill_in_blank_variant_is_allowed <- function(answer_text) {
  fill_in_blank_answer_is_numeric(answer_text) || fill_in_blank_answer_is_simple_phrase(answer_text)
}

get_qbank_model <- function(model = NULL) {
  explicit_model <- model %||% Sys.getenv("ANTHROPIC_QBANK_MODEL")
  explicit_model <- explicit_model %>% as.character() %>% str_squish()
  if (nzchar(explicit_model)) {
    explicit_model
  } else {
    DEFAULT_QBANK_MODEL
  }
}

require_generation_environment <- function(api_key = Sys.getenv("ANTHROPIC_API_KEY")) {
  if (!nzchar(api_key)) {
    stop("ANTHROPIC_API_KEY is not set. Set it in the environment before generating questions.", call. = FALSE)
  }
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("The ellmer package is required to generate questions with Claude.", call. = FALSE)
  }
  invisible(api_key)
}

build_generation_context <- function(topic_id) {
  topic_spec <- get_topic_spec(topic_id)
  page <- get_concept_page(topic_id)

  if (is.null(page) || !nzchar(page$markdown_body[[1]] %||% "")) {
    stop(glue("No concept page content found for topic_id '{topic_id}'."), call. = FALSE)
  }

  context_body <- page$markdown_body[[1]] %>%
    str_replace_all("\\r", "\n") %>%
    str_squish() %>%
    str_sub(1, 6000)

  list(
    topic_spec = topic_spec,
    context_body = context_body
  )
}

strip_markdown_fences <- function(x) {
  x %>%
    str_replace("^\\s*```json\\s*", "") %>%
    str_replace("^\\s*```\\s*", "") %>%
    str_replace("\\s*```\\s*$", "") %>%
    str_trim()
}

extract_json_object <- function(x) {
  text <- x %||% ""
  start_pos <- str_locate(text, fixed("{"))[1]
  end_positions <- str_locate_all(text, fixed("}"))[[1]]

  if (is.na(start_pos) || nrow(end_positions) == 0) {
    return("")
  }

  end_pos <- end_positions[nrow(end_positions), 1]
  if (is.na(end_pos) || end_pos < start_pos) {
    return("")
  }

  str_sub(text, start_pos, end_pos)
}

chunk_stamp <- function() {
  format(Sys.time(), "%Y%m%dT%H%M%S")
}

save_raw_chunk <- function(raw_response, topic_id, format, status = "success", attempt = 1, raw_dir = QUESTION_BANK_RAW_DIR) {
  fs::dir_create(raw_dir)
  path <- fs::path(raw_dir, glue("{chunk_stamp()}_{topic_id}_{format}_{status}_attempt{attempt}.txt"))
  writeLines(enc2utf8(raw_response %||% ""), path, useBytes = TRUE)
  path
}

append_generation_error <- function(topic_id,
                                    format_name,
                                    error_message,
                                    raw_path = NA_character_,
                                    errors_path = QUESTION_BANK_ERRORS_CSV) {
  fs::dir_create(fs::path_dir(errors_path))

  new_row <- tibble(
    ts = current_utc_timestamp(),
    topic_id = topic_id,
    format = format_name,
    error_message = error_message,
    raw_path = raw_path %||% NA_character_
  )

  if (file.exists(errors_path)) {
    existing <- normalize_ts_column(readr::read_csv(errors_path, show_col_types = FALSE, progress = FALSE))
    errors <- bind_rows(existing, normalize_ts_column(new_row))
  } else {
    errors <- normalize_ts_column(new_row)
  }

  readr::write_csv(errors, errors_path)
  invisible(errors)
}

load_saved_question_bank <- function(csv_path = QUESTION_BANK_OUTPUT_CSV) {
  if (!file.exists(csv_path)) {
    return(empty_question_bank())
  }

  bank <- readr::read_csv(csv_path, show_col_types = FALSE, progress = FALSE)
  if (!"correct_choice_id" %in% names(bank)) {
    bank$correct_choice_id <- NA_character_
  }
  if (!"interaction_type" %in% names(bank)) {
    bank$interaction_type <- NA_character_
  }
  missing_cols <- setdiff(REQUIRED_QUESTION_FIELDS, names(bank))
  if (length(missing_cols) > 0) {
    stop(
      glue("Saved question bank is missing required columns: {paste(missing_cols, collapse = ', ')}"),
      call. = FALSE
    )
  }

  bank %>%
    mutate(
      module_id = vapply(module_id, normalize_module_id_value, character(1)),
      choices = map(choices, normalize_choice_objects),
      correct_choice_id = as.character(correct_choice_id %||% NA_character_),
      interaction_type = pmap_chr(list(format, question_text, interaction_type), normalize_interaction_type_value),
      correct_answer = map(correct_answer, coerce_text_vector),
      accepted_answers = map(accepted_answers, coerce_text_vector)
    ) %>%
    select(all_of(REQUIRED_QUESTION_FIELDS))
}

merge_question_bank <- function(existing_bank, new_chunk) {
  existing_bank <- if (is.null(existing_bank) || nrow(existing_bank) == 0) empty_question_bank() else as_tibble(existing_bank)
  new_chunk <- as_tibble(new_chunk)

  if (nrow(new_chunk) == 0) {
    return(existing_bank)
  }

  topic_id <- new_chunk$topic_id[[1]]
  format_name <- new_chunk$format[[1]]

  bind_rows(
    existing_bank %>% filter(!(topic_id == !!topic_id & format == !!format_name)),
    new_chunk
  ) %>%
    distinct(question_id, .keep_all = TRUE) %>%
    left_join(select(OFFICIAL_TOPIC_STRUCTURE, module_id, module_order, topic_id, topic_order), by = c("module_id", "topic_id")) %>%
    arrange(module_order, topic_order, format, difficulty, question_id) %>%
    select(all_of(REQUIRED_QUESTION_FIELDS))
}

save_question_bank <- function(bank,
                               csv_path = QUESTION_BANK_OUTPUT_CSV,
                               json_path = QUESTION_BANK_OUTPUT_JSON) {
  validation <- validate_question_bank(bank)
  if (!validation$valid) {
    stop(
      glue("Question bank failed validation:\n- {paste(validation$errors, collapse = '\n- ')}"),
      call. = FALSE
    )
  }

  fs::dir_create(fs::path_dir(csv_path))
  fs::dir_create(fs::path_dir(json_path))

  bank <- as_tibble(bank) %>% select(all_of(REQUIRED_QUESTION_FIELDS))

  csv_bank <- bank %>%
    mutate(
      choices = map_chr(choices, serialize_choice_objects),
      correct_choice_id = as.character(correct_choice_id %||% NA_character_),
      correct_answer = map_chr(correct_answer, serialize_json_vector),
      accepted_answers = map_chr(accepted_answers, serialize_json_vector)
    )

  readr::write_csv(csv_bank, csv_path)
  jsonlite::write_json(bank, json_path, pretty = TRUE, auto_unbox = TRUE, null = "null")

  invisible(
    list(
      csv_path = csv_path,
      json_path = json_path,
      n_questions = nrow(bank)
    )
  )
}

persist_generated_chunk <- function(questions,
                                    csv_path = QUESTION_BANK_OUTPUT_CSV,
                                    json_path = QUESTION_BANK_OUTPUT_JSON) {
  questions <- as_tibble(questions)
  if (nrow(questions) == 0) {
    return(invisible(NULL))
  }

  progress_bank <- merge_question_bank(load_saved_question_bank(csv_path), questions)
  save_question_bank(progress_bank, csv_path = csv_path, json_path = json_path)

  invisible(progress_bank)
}

remove_duplicate_questions <- function(bank, verbose = TRUE) {
  bank <- as_tibble(bank)
  if (nrow(bank) == 0 || !"question_text" %in% names(bank)) {
    return(bank)
  }

  bank <- bank %>%
    mutate(
      normalized_question_text = normalize_question_text(question_text),
      .row_id = row_number()
    )

  duplicates <- bank %>%
    filter(nzchar(normalized_question_text)) %>%
    group_by(normalized_question_text) %>%
    mutate(.duplicate_rank = row_number()) %>%
    ungroup() %>%
    filter(.duplicate_rank > 1)

  if (nrow(duplicates) > 0 && isTRUE(verbose)) {
    duplicate_summary <- duplicates %>%
      transmute(
        detail = glue("{question_id} [{topic_id}/{format}/{concept_tag}]")
      ) %>%
      pull(detail)

    message(
      glue(
        "Removed {nrow(duplicates)} duplicate question(s) after normalization.\n- {paste(duplicate_summary, collapse = '\n- ')}"
      )
    )
  }

  bank %>%
    group_by(normalized_question_text) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    arrange(.row_id) %>%
    select(-normalized_question_text, -.row_id)
}

check_expected_counts <- function(bank, n_per_format) {
  expected <- tidyr::expand_grid(
    topic_id = OFFICIAL_TOPIC_STRUCTURE$topic_id,
    format = VALID_FORMATS
  )

  actual <- as_tibble(bank) %>%
    count(topic_id, format, name = "actual_n")

  expected %>%
    left_join(actual, by = c("topic_id", "format")) %>%
    mutate(
      expected_n = as.integer(n_per_format),
      actual_n = coalesce(actual_n, 0L),
      shortfall = pmax(expected_n - actual_n, 0L)
    ) %>%
    filter(shortfall > 0) %>%
    left_join(
      select(OFFICIAL_TOPIC_STRUCTURE, topic_id, topic_label, module_id, module_label),
      by = "topic_id"
    ) %>%
    select(module_id, module_label, topic_id, topic_label, format, expected_n, actual_n, shortfall)
}

replenish_missing_questions <- function(bank,
                                        n_per_format = 3,
                                        max_attempts = 2,
                                        model = NULL,
                                        api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                                        csv_path = QUESTION_BANK_OUTPUT_CSV,
                                        json_path = QUESTION_BANK_OUTPUT_JSON,
                                        raw_dir = QUESTION_BANK_RAW_DIR,
                                        errors_path = QUESTION_BANK_ERRORS_CSV) {
  current_bank <- remove_duplicate_questions(bank, verbose = FALSE)
  missing <- check_expected_counts(current_bank, n_per_format = n_per_format)

  if (nrow(missing) == 0) {
    return(current_bank)
  }

  for (attempt in seq_len(max_attempts)) {
    if (nrow(missing) == 0) {
      break
    }

    for (i in seq_len(nrow(missing))) {
      topic_id <- missing$topic_id[[i]]
      format_name <- missing$format[[i]]
      shortfall <- missing$shortfall[[i]]

      if (shortfall <= 0) {
        next
      }

      existing_texts <- current_bank %>%
        filter(topic_id == !!topic_id) %>%
        pull(question_text)

      replacement_chunk <- tryCatch(
        generate_questions_for_topic_format(
          topic_id = topic_id,
          format = format_name,
          n = shortfall,
          model = model,
          api_key = api_key,
          csv_path = csv_path,
          json_path = json_path,
          raw_dir = raw_dir,
          errors_path = errors_path,
          avoid_question_texts = existing_texts
        ),
        error = function(e) {
          message(
            glue(
              "Replenishment attempt {attempt} failed for {topic_id}/{format_name}: {conditionMessage(e)}"
            )
          )
          empty_question_bank()
        }
      )

      if (nrow(replacement_chunk) > 0) {
        current_bank <- bind_rows(current_bank, replacement_chunk) %>%
          remove_duplicate_questions(verbose = FALSE)
      }
    }

    missing <- check_expected_counts(current_bank, n_per_format = n_per_format)
  }

  if (nrow(missing) > 0) {
    message(
      glue(
        "Still missing some topic/format counts after replenishment.\n- {paste(glue('{missing$topic_id}/{missing$format}: need {missing$shortfall} more'), collapse = '\n- ')}"
      )
    )
  }

  current_bank
}

build_generation_prompts <- function(topic_id, format, n, strict = FALSE, avoid_question_texts = character()) {
  context <- build_generation_context(topic_id)
  topic_spec <- context$topic_spec
  avoided_text <- sanitize_text_vector(avoid_question_texts)

  system_prompt <- paste(
    "You are generating original introductory statistics practice questions for offline local use.",
    "Ground every question in the provided concept-page content, but do not copy source wording.",
    "Keep notation aligned with the current course.",
    "Do not mention any professor names, source identities, or file paths.",
    "Generate only the requested format and keep the output compact.",
    "Keep hints to one sentence.",
    "Keep explanations concise, usually one or two sentences.",
    "Avoid long scenarios and keep drag_and_drop checklist items short.",
    "Do not reuse the same scenario or question wording from earlier generated questions in this topic.",
    "Vary contexts, numbers, and wording across questions.",
    "Avoid repeating common templates too closely.",
    "Use fill_in_blank only for simple vocabulary, simple short phrases, or a single numeric answer with explicit rounding instructions.",
    "Never use fill_in_blank for Greek letters or special symbols, notation such as μ0, p0, p-hat, x-bar, sigma, or alpha, formulas, inequalities, probability expressions such as P(X >= k), null-hypothesis notation such as H0:, multi-blank notation prompts, or any prompt that requires typing mathematical notation.",
    "Use multiple_choice or choose_best_answer when the skill is distinguishing notation, formulas, or symbolic wording.",
    "Use drag_and_drop for select-all, ordering, or categorize tasks only when the structure is clearer than multiple choice.",
    "Return valid compact JSON only.",
    "Do not use markdown fences.",
    "Do not mix arrays and objects in the same field.",
    "Return a single JSON object with the shape {\"questions\":[...]} and no other top-level keys.",
    "Each question must contain these fields exactly:",
    paste(REQUIRED_QUESTION_FIELDS, collapse = ", "),
    "Use only these difficulty values: easy, medium, hard.",
    glue("Use only this format value for this call: {format}."),
    if (format %in% c("multiple_choice", "choose_best_answer")) {
      paste(
        "choices must be an array of objects with id and text, using ids like A, B, C, D.",
        "correct_choice_id must match one of the choice ids.",
        "correct_answer must be the full correct answer text.",
        "accepted_answers must be an empty array."
      )
    } else {
      "Use an empty array for choices when the format does not need choices."
    },
    if (format == "fill_in_blank") {
      paste(
        "choices must be an empty array.",
        "correct_choice_id must be an empty string.",
        "correct_answer must be a concise canonical answer.",
        "accepted_answers must be a character array of acceptable plain-text variants.",
        "Use fill_in_blank mostly for vocabulary, a short conceptual phrase, or a single numeric answer.",
        "Prefer word answers like sample proportion, null value, reject, or fail to reject over notation-based answers.",
        "If the answer is numeric, question_text must explicitly say how to round.",
        "Do not require symbols, hats, subscripts, inequalities, Greek letters, probability notation, null-hypothesis notation, or typed expressions.",
        "Do not include notation-only accepted_answers such as p0, p-hat, p-hat variants, mu0, x-bar, sigma, or alpha.",
        "accepted_answers should stay short, usually 1 to 4 variants maximum."
      )
    } else {
      "Use an empty array for accepted_answers when the format does not use them."
    },
    if (format == "drag_and_drop") {
      paste(
        "interaction_type must be one of: select_all, ordering, categorize.",
        "choices must be an array of objects with id and text.",
        "If the prompt asks students to arrange, order, sequence, or go from first to last, interaction_type must be ordering.",
        "If the prompt asks students to select all that apply, interaction_type must be select_all.",
        "If the prompt asks students to sort into groups or categories, interaction_type must be categorize.",
        "For select_all, correct_answer must be an array of the correct choice ids only.",
        "For ordering, correct_answer must be an array of all choice ids in the exact correct order.",
        "For categorize, correct_answer must be an array of strings in the form choice_id::category_name, covering every choice.",
        "Keep drag_and_drop prompts short, with 4 or 5 items maximum.",
        "Do not use ordering prompts as checklist questions."
      )
    } else {
      NULL
    },
    "Set review_status to draft and generated_by to claude.",
    if (strict) {
      "Return compact valid JSON only. No markdown fences. No extra commentary."
    } else {
      NULL
    }
  )

  user_prompt <- paste(
    glue("Module: {topic_spec$module_label[[1]]}"),
    glue("Topic: {topic_spec$topic_label[[1]]}"),
    glue("topic_id: {topic_spec$topic_id[[1]]}"),
    glue("Generate exactly {n} question(s)."),
    glue("Requested format: {format}."),
    "Use clear concept_tag values that reflect the underlying skill.",
    if (length(avoided_text) > 0) {
      paste(
        "Avoid reusing or closely paraphrasing these existing question wordings:",
        paste(paste0("- ", avoided_text), collapse = "\n")
      )
    } else {
      NULL
    },
    "Concept-page grounding:",
    context$context_body,
    if (strict) {
      'Return compact valid JSON only in the form {"questions":[...]}.'
    } else {
      'Return JSON in the form {"questions":[...]}.'
    },
    sep = "\n\n"
  )

  list(
    topic_spec = topic_spec,
    system_prompt = system_prompt,
    user_prompt = user_prompt
  )
}

call_claude_for_topic_format <- function(topic_id,
                                         format,
                                         n = 2,
                                         model = NULL,
                                         strict = FALSE,
                                         avoid_question_texts = character(),
                                         api_key = Sys.getenv("ANTHROPIC_API_KEY")) {
  require_generation_environment(api_key)
  prompts <- build_generation_prompts(
    topic_id = topic_id,
    format = format,
    n = n,
    strict = strict,
    avoid_question_texts = avoid_question_texts
  )
  model_name <- get_qbank_model(model)

  chat <- ellmer::chat_anthropic(
    model = model_name,
    system_prompt = prompts$system_prompt
  )

  list(
    topic_spec = prompts$topic_spec,
    raw_response = chat$chat(prompts$user_prompt) %||% ""
  )
}

parse_json_payload <- function(raw_response, topic_id, format) {
  raw_response <- raw_response %||% ""
  if (!nzchar(str_squish(raw_response))) {
    stop(glue("Empty Claude response for topic_id '{topic_id}' and format '{format}'."), call. = FALSE)
  }

  stripped <- strip_markdown_fences(raw_response)
  extracted <- extract_json_object(stripped)
  candidates <- unique(c(stripped, extracted))
  candidates <- candidates[nzchar(str_squish(candidates))]

  parse_errors <- character()

  for (candidate in candidates) {
    parsed <- tryCatch(
      jsonlite::fromJSON(candidate, simplifyVector = FALSE),
      error = function(e) {
        parse_errors <<- c(parse_errors, conditionMessage(e))
        NULL
      }
    )

    if (!is.null(parsed)) {
      return(parsed)
    }
  }

  final_error <- tail(parse_errors, 1) %||% "Unknown JSON parsing error."
  stop(
    glue("Could not parse Claude JSON for topic_id '{topic_id}' and format '{format}': {final_error}"),
    call. = FALSE
  )
}

normalize_generated_question <- function(question, topic_spec, expected_format) {
  question_id <- question$question_id %||% ""
  question_id <- question_id %>% as.character() %>% str_squish()

  if (!nzchar(question_id)) {
    question_id <- glue(
      "claude_{topic_spec$topic_id[[1]]}_{expected_format}_{chunk_stamp()}_{sample(1000:9999, 1)}"
    )
  }

  choice_objects <- normalize_choice_objects(question$choices)
  interaction_type <- normalize_interaction_type_value(
    format = expected_format,
    question_text = question$question_text %||% "",
    interaction_type = question$interaction_type %||% NA_character_
  )
  if (identical(expected_format, "drag_and_drop")) {
    drag_error <- validate_raw_drag_payload(
      correct_answer = question$correct_answer,
      choices = choice_objects,
      interaction_type = interaction_type,
      question_text = question$question_text %||% ""
    )
    if (!is.null(drag_error)) {
      stop(drag_error, call. = FALSE)
    }
  }
  correct_choice_id <- derive_correct_choice_id(
    question$correct_choice_id %||% NA_character_,
    question$correct_answer,
    choice_objects,
    expected_format
  )
  correct_answer <- get_correct_answer_display(
    question$correct_answer,
    correct_choice_id,
    choice_objects,
    expected_format,
    interaction_type = interaction_type,
    question_text = question$question_text %||% ""
  )
  accepted_answers <- coerce_text_vector(question$accepted_answers)
  if (identical(expected_format, "fill_in_blank") && length(accepted_answers) == 0) {
    accepted_answers <- coerce_text_vector(correct_answer)
  }

  tibble(
    question_id = question_id,
    module_id = normalize_module_id_value(topic_spec$module_id[[1]]),
    module_label = topic_spec$module_label[[1]],
    topic_id = topic_spec$topic_id[[1]],
    topic_label = topic_spec$topic_label[[1]],
    concept_tag = question$concept_tag %||% topic_spec$topic_id[[1]],
    difficulty = question$difficulty %||% "medium",
    format = expected_format,
    question_text = question$question_text %||% "",
    choices = list(choice_objects),
    interaction_type = interaction_type,
    correct_choice_id = as.character(correct_choice_id %||% NA_character_),
    correct_answer = list(as.character(correct_answer %||% character())),
    accepted_answers = list(as.character(accepted_answers %||% character())),
    hint = question$hint %||% "",
    explanation = question$explanation %||% "",
    source_basis = question$source_basis %||% glue("concept_page::{topic_spec$topic_id[[1]]}"),
    review_status = question$review_status %||% "draft",
    generated_by = question$generated_by %||% "claude"
  ) %>%
    mutate(
      question_id = as.character(question_id),
      module_id = vapply(module_id, normalize_module_id_value, character(1)),
      module_label = as.character(module_label),
      topic_id = as.character(topic_id),
      topic_label = as.character(topic_label),
      concept_tag = as.character(concept_tag),
      difficulty = as.character(difficulty),
      format = as.character(format),
      question_text = as.character(question_text),
      interaction_type = as.character(interaction_type %||% NA_character_),
      correct_choice_id = as.character(correct_choice_id),
      hint = as.character(hint),
      explanation = as.character(explanation),
      source_basis = as.character(source_basis),
      review_status = as.character(review_status),
      generated_by = as.character(generated_by)
    )
}

normalize_question_payload <- function(parsed, topic_spec, format, n_expected) {
  questions <- parsed$questions %||% parsed

  if (is.null(questions)) {
    stop(
      glue("Claude returned no question payload for topic_id '{topic_spec$topic_id[[1]]}' and format '{format}'."),
      call. = FALSE
    )
  }

  if (!is.null(questions$question_text)) {
    questions <- list(questions)
  }

  if (!is.list(questions) || length(questions) == 0) {
    stop(
      glue("Claude returned an empty question list for topic_id '{topic_spec$topic_id[[1]]}' and format '{format}'."),
      call. = FALSE
    )
  }

  bank <- purrr::map_dfr(
    questions,
    normalize_generated_question,
    topic_spec = topic_spec,
    expected_format = format
  )

  if (nrow(bank) != n_expected) {
    stop(
      glue("Expected {n_expected} question(s) for topic_id '{topic_spec$topic_id[[1]]}' and format '{format}', but received {nrow(bank)}."),
      call. = FALSE
    )
  }

  bank
}

validate_question_bank <- function(bank, allow_empty = FALSE) {
  bank <- as_tibble(bank)
  if ("module_id" %in% names(bank)) {
    bank <- bank %>%
      mutate(module_id = vapply(module_id, normalize_module_id_value, character(1)))
  }
  if (all(c("format", "question_text", "interaction_type") %in% names(bank))) {
    bank <- bank %>%
      mutate(interaction_type = pmap_chr(list(format, question_text, interaction_type), normalize_interaction_type_value))
  }
  errors <- character()

  if (nrow(bank) == 0 && !allow_empty) {
    errors <- c(errors, "Question bank has zero rows.")
  }

  missing_cols <- setdiff(REQUIRED_QUESTION_FIELDS, names(bank))
  if (length(missing_cols) > 0) {
    errors <- c(errors, glue("Missing required columns: {paste(missing_cols, collapse = ', ')}."))
  }

  if (!"question_id" %in% names(bank) || any(is.na(bank$question_id) | !nzchar(bank$question_id))) {
    errors <- c(errors, "Every question must have a non-empty question_id.")
  }
  if (!"topic_id" %in% names(bank) || any(!bank$topic_id %in% OFFICIAL_TOPIC_STRUCTURE$topic_id)) {
    errors <- c(errors, "Every question must have a valid topic_id from the official module/topic structure.")
  }
  if (!"module_id" %in% names(bank) || any(!bank$module_id %in% OFFICIAL_TOPIC_STRUCTURE$module_id)) {
    errors <- c(errors, "Every question must have a valid module_id from the official module/topic structure.")
  }
  if (!"format" %in% names(bank) || any(!bank$format %in% VALID_FORMATS)) {
    errors <- c(errors, "Every question must have a valid format.")
  }
  if (!"difficulty" %in% names(bank) || any(!bank$difficulty %in% VALID_DIFFICULTIES)) {
    errors <- c(errors, "Every question must have a valid difficulty.")
  }
  if ("question_text" %in% names(bank) && any(is.na(bank$question_text) | !nzchar(bank$question_text))) {
    errors <- c(errors, "Every question must have non-empty question_text.")
  }
  if ("question_text" %in% names(bank)) {
    duplicate_rows <- bank %>%
      mutate(normalized_question_text = normalize_question_text(question_text)) %>%
      filter(nzchar(normalized_question_text)) %>%
      group_by(normalized_question_text) %>%
      filter(n() > 1) %>%
      ungroup()

    if (nrow(duplicate_rows) > 0) {
      duplicate_group_count <- duplicate_rows %>%
        distinct(normalized_question_text) %>%
        nrow()

      duplicate_examples <- duplicate_rows %>%
        distinct(topic_id, format, concept_tag, question_text) %>%
        mutate(detail = glue("{topic_id}/{format}/{concept_tag}: {str_trunc(question_text, 100)}")) %>%
        pull(detail)

      errors <- c(
        errors,
        glue(
          "Question text must not contain duplicate normalized wording. Found {duplicate_group_count} duplicate group(s).\nExamples:\n- {paste(head(duplicate_examples, 6), collapse = '\n- ')}"
        )
      )
    }
  }
  if ("explanation" %in% names(bank) && any(is.na(bank$explanation) | !nzchar(bank$explanation))) {
    errors <- c(errors, "Every question must have a non-empty explanation.")
  }

  if ("choices" %in% names(bank)) {
    malformed_choices <- bank %>%
      filter(!map_lgl(choices, is_valid_choice_object_list))
    if (nrow(malformed_choices) > 0) {
      errors <- c(errors, "choices must be a valid array of objects with id and text.")
    }

    invalid_choices <- bank %>%
      filter(format %in% c("multiple_choice", "choose_best_answer")) %>%
      filter(map_int(choices, length) == 0)
    if (nrow(invalid_choices) > 0) {
      errors <- c(errors, "Multiple-choice style questions must include choices.")
    }

    fill_choices <- bank %>%
      filter(format == "fill_in_blank") %>%
      filter(map_int(choices, length) > 0)
    if (nrow(fill_choices) > 0) {
      errors <- c(errors, "Fill-in-the-blank questions must keep choices empty.")
    }
  }

  if ("interaction_type" %in% names(bank)) {
    invalid_single_select_type <- bank %>%
      filter(format %in% c("multiple_choice", "choose_best_answer")) %>%
      filter(is.na(interaction_type) | interaction_type != "single_select")
    if (nrow(invalid_single_select_type) > 0) {
      errors <- c(errors, "Multiple-choice style questions must use interaction_type = 'single_select'.")
    }

    invalid_fill_type <- bank %>%
      filter(format == "fill_in_blank") %>%
      filter(is.na(interaction_type) | interaction_type != "fill_in_blank")
    if (nrow(invalid_fill_type) > 0) {
      errors <- c(errors, "Fill-in-the-blank questions must use interaction_type = 'fill_in_blank'.")
    }

    invalid_drag_type <- bank %>%
      filter(format == "drag_and_drop") %>%
      filter(!interaction_type %in% VALID_DRAG_INTERACTION_TYPES)
    if (nrow(invalid_drag_type) > 0) {
      errors <- c(errors, "drag_and_drop questions must include a valid interaction_type.")
    }
  }

  if ("correct_choice_id" %in% names(bank)) {
    invalid_choice_id <- bank %>%
      filter(format %in% c("multiple_choice", "choose_best_answer")) %>%
      filter(is.na(correct_choice_id) | !nzchar(correct_choice_id))
    if (nrow(invalid_choice_id) > 0) {
      errors <- c(errors, "Multiple-choice style questions must include correct_choice_id.")
    }

    unmatched_choice_id <- bank %>%
      filter(format %in% c("multiple_choice", "choose_best_answer")) %>%
      filter(!map2_lgl(correct_choice_id, choices, ~ .x %in% get_choice_ids(.y)))
    if (nrow(unmatched_choice_id) > 0) {
      errors <- c(errors, "correct_choice_id must match one of the choice ids.")
    }

    fill_choice_id <- bank %>%
      filter(format == "fill_in_blank") %>%
      filter(!is.na(correct_choice_id) & nzchar(correct_choice_id))
    if (nrow(fill_choice_id) > 0) {
      errors <- c(errors, "Fill-in-the-blank questions must not use correct_choice_id.")
    }
  }

  if ("accepted_answers" %in% names(bank)) {
    invalid_accepts <- bank %>%
      filter(format == "fill_in_blank") %>%
      filter(map_int(accepted_answers, length) == 0)
    if (nrow(invalid_accepts) > 0) {
      errors <- c(errors, "Fill-in-the-blank questions must include accepted_answers.")
    }
  }

  if ("correct_answer" %in% names(bank)) {
    invalid_correct <- bank %>% filter(map_int(correct_answer, length) == 0)
    if (nrow(invalid_correct) > 0) {
      errors <- c(errors, "Every question must include a correct_answer.")
    }
  }

  if (all(c("format", "question_text", "choices", "correct_answer", "interaction_type") %in% names(bank))) {
    drag_bank <- bank %>%
      filter(format == "drag_and_drop") %>%
      mutate(
        grading_values = pmap(list(correct_answer, choices, interaction_type, question_text), get_drag_grading_values)
      )
    
    if (nrow(drag_bank) > 0) {
      invalid_drag_choices <- drag_bank %>%
        filter(map_int(choices, length) < 2)
      if (nrow(invalid_drag_choices) > 0) {
        errors <- c(errors, "drag_and_drop questions must include at least two choices.")
      }
      
      invalid_ordering <- drag_bank %>%
        filter(interaction_type == "ordering") %>%
        filter(
          map_int(choices, length) < 3 |
            !map2_lgl(grading_values, choices, ~ {
              choice_ids <- get_choice_ids(.y)
              length(.x) == length(choice_ids) &&
                length(.x) >= 3 &&
                !anyDuplicated(.x) &&
                setequal(.x, choice_ids)
            })
        )
      if (nrow(invalid_ordering) > 0) {
        errors <- c(errors, "Ordering drag_and_drop questions must use correct_answer as an ordered vector of all choice ids.")
      }
      
      invalid_select_all <- drag_bank %>%
        filter(interaction_type == "select_all") %>%
        filter(
          !map2_lgl(grading_values, choices, ~ {
            choice_ids <- get_choice_ids(.y)
            length(.x) > 0 && all(.x %in% choice_ids) && !anyDuplicated(.x)
          })
        )
      if (nrow(invalid_select_all) > 0) {
        errors <- c(errors, "Select-all drag_and_drop questions must use correct_answer as a vector of valid choice ids.")
      }
      
      invalid_categorize <- drag_bank %>%
        filter(interaction_type == "categorize") %>%
        filter(
          !map2_lgl(grading_values, choices, ~ {
            choice_ids <- get_choice_ids(.y)
            is.character(.x) &&
              length(.x) == length(choice_ids) &&
              setequal(names(.x), choice_ids) &&
              all(nzchar(unname(.x))) &&
              length(unique(unname(.x))) >= 2
          })
        )
      if (nrow(invalid_categorize) > 0) {
        errors <- c(errors, "Categorize drag_and_drop questions must map every choice id to a non-empty category.")
      }
    }
  }

  if (all(c("format", "question_text", "correct_answer", "accepted_answers") %in% names(bank))) {
    fill_bank <- bank %>% filter(format == "fill_in_blank")
    
    if (nrow(fill_bank) > 0) {
      formula_heavy_fill <- fill_bank %>%
        filter(fill_in_blank_question_is_formula_heavy(question_text))
      if (nrow(formula_heavy_fill) > 0) {
        errors <- c(errors, "Fill-in-the-blank questions cannot use formula-heavy or notation-heavy question text.")
      }
      
      multi_part_fill <- fill_bank %>%
        filter(map_int(correct_answer, length) != 1)
      if (nrow(multi_part_fill) > 0) {
        errors <- c(errors, "Fill-in-the-blank questions must have exactly one canonical correct_answer.")
      }
      
      complex_correct_fill <- fill_bank %>%
        filter(
          !map_lgl(correct_answer, ~ {
            answer <- normalize_fill_answer(.x)
            fill_in_blank_answer_is_numeric(answer) || fill_in_blank_answer_is_simple_phrase(answer)
          })
        )
      if (nrow(complex_correct_fill) > 0) {
        errors <- c(errors, "Fill-in-the-blank correct_answer must be a simple word/phrase or a single numeric value.")
      }
      
      too_many_variants_fill <- fill_bank %>%
        filter(map_int(accepted_answers, length) > 4)
      if (nrow(too_many_variants_fill) > 0) {
        errors <- c(errors, "Fill-in-the-blank accepted_answers must stay short; too many variants suggest the item should be multiple choice instead.")
      }
      
      complex_variants_fill <- fill_bank %>%
        filter(!map_lgl(accepted_answers, ~ all(map_lgl(.x, fill_in_blank_variant_is_allowed))))
      if (nrow(complex_variants_fill) > 0) {
        errors <- c(errors, "Fill-in-the-blank accepted_answers must avoid notation-heavy, symbolic, or multi-part variants.")
      }
      
      numeric_without_rounding_fill <- fill_bank %>%
        filter(map_lgl(correct_answer, ~ fill_in_blank_answer_is_numeric(normalize_fill_answer(.x)))) %>%
        filter(!map_lgl(question_text, fill_in_blank_has_rounding_instruction))
      if (nrow(numeric_without_rounding_fill) > 0) {
        errors <- c(errors, "Numeric fill-in-the-blank questions must include clear rounding or decimal instructions in question_text.")
      }
    }
  }

  list(
    valid = length(errors) == 0,
    errors = unique(errors)
  )
}

generate_questions_for_topic_format <- function(topic_id,
                                                format,
                                                n = 2,
                                                model = NULL,
                                                api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                                                csv_path = QUESTION_BANK_OUTPUT_CSV,
                                                json_path = QUESTION_BANK_OUTPUT_JSON,
                                                raw_dir = QUESTION_BANK_RAW_DIR,
                                                errors_path = QUESTION_BANK_ERRORS_CSV,
                                                avoid_question_texts = character()) {
  if (!format %in% VALID_FORMATS) {
    stop(glue("Invalid format '{format}'. Must be one of: {paste(VALID_FORMATS, collapse = ', ')}."), call. = FALSE)
  }

  require_generation_environment(api_key)

  last_error <- NULL
  last_failed_raw_path <- NA_character_

  for (attempt in seq_len(2)) {
    strict <- attempt == 2
    raw_response <- NULL

    attempt_result <- tryCatch(
      {
        response <- call_claude_for_topic_format(
          topic_id = topic_id,
          format = format,
          n = n,
          model = model,
          strict = strict,
          avoid_question_texts = avoid_question_texts,
          api_key = api_key
        )

        raw_response <- response$raw_response
        parsed <- parse_json_payload(raw_response, topic_id = topic_id, format = format)
        questions <- normalize_question_payload(
          parsed = parsed,
          topic_spec = response$topic_spec,
          format = format,
          n_expected = n
        )

        validation <- validate_question_bank(questions)
        if (!validation$valid) {
          stop(
            glue(
              "Chunk validation failed for topic_id '{topic_id}' and format '{format}':\n- {paste(validation$errors, collapse = '\n- ')}"
            ),
            call. = FALSE
          )
        }

        save_raw_chunk(
          raw_response = raw_response,
          topic_id = topic_id,
          format = format,
          status = "success",
          attempt = attempt,
          raw_dir = raw_dir
        )

        questions
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        if (!is.null(raw_response) && nzchar(str_squish(raw_response))) {
          last_failed_raw_path <<- save_raw_chunk(
            raw_response = raw_response,
            topic_id = topic_id,
            format = format,
            status = "failed_raw",
            attempt = attempt,
            raw_dir = raw_dir
          )
        }
        NULL
      }
    )

    if (!is.null(attempt_result)) {
      persist_error <- tryCatch(
        {
          persist_generated_chunk(
            attempt_result,
            csv_path = csv_path,
            json_path = json_path
          )
          NULL
        },
        error = function(e) conditionMessage(e)
      )

      if (!is.null(persist_error)) {
        warning(
          glue(
            "Generated questions for topic_id '{topic_id}' and format '{format}' were returned, but saving to the question bank failed: {persist_error}"
          ),
          call. = FALSE
        )
      }

      return(attempt_result)
    }
  }

  append_generation_error(
    topic_id = topic_id,
    format_name = format,
    error_message = last_error %||% glue("Unknown generation error for topic_id '{topic_id}' and format '{format}'."),
    raw_path = last_failed_raw_path,
    errors_path = errors_path
  )

  stop(
    glue("Question generation failed for topic_id '{topic_id}' and format '{format}': {last_error}"),
    call. = FALSE
  )
}

generate_questions_for_topic <- function(topic_id,
                                         n_per_format = 2,
                                         model = NULL,
                                         api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                                         csv_path = QUESTION_BANK_OUTPUT_CSV,
                                         json_path = QUESTION_BANK_OUTPUT_JSON,
                                         raw_dir = QUESTION_BANK_RAW_DIR,
                                         errors_path = QUESTION_BANK_ERRORS_CSV) {
  require_generation_environment(api_key)
  expected_rows <- length(VALID_FORMATS) * n_per_format
  chunks <- list()
  failures <- list()

  for (format_name in VALID_FORMATS) {
    chunk <- tryCatch(
      generate_questions_for_topic_format(
        topic_id = topic_id,
        format = format_name,
        n = n_per_format,
        model = model,
        api_key = api_key,
        csv_path = csv_path,
        json_path = json_path,
        raw_dir = raw_dir,
        errors_path = errors_path
      ),
      error = function(e) {
        failures[[format_name]] <<- conditionMessage(e)
        NULL
      }
    )

    if (!is.null(chunk) && nrow(chunk) > 0) {
      chunks[[format_name]] <- chunk
    }
  }

  if (length(chunks) == 0) {
    failure_text <- if (length(failures) == 0) {
      "Unknown generation failure."
    } else {
      paste(glue("{names(failures)}: {unlist(failures, use.names = FALSE)}"), collapse = "\n- ")
    }
    stop(
      glue("All formats failed for topic_id '{topic_id}'.\n- {failure_text}"),
      call. = FALSE
    )
  }

  bank <- bind_rows(chunks)
  if (length(failures) > 0) {
    warning(
      glue(
        "Some formats failed for topic_id '{topic_id}'. Returned {nrow(bank)} of {expected_rows} expected rows.\n- {paste(glue('{names(failures)}: {unlist(failures, use.names = FALSE)}'), collapse = '\n- ')}"
      ),
      call. = FALSE
    )
  }

  bank
}

test_generate_topic <- function(topic_id = "ht_prop",
                                n_per_format = 2,
                                model = NULL,
                                api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                                csv_path = QUESTION_BANK_OUTPUT_CSV,
                                json_path = QUESTION_BANK_OUTPUT_JSON,
                                raw_dir = QUESTION_BANK_RAW_DIR,
                                errors_path = QUESTION_BANK_ERRORS_CSV) {
  expected_rows <- length(VALID_FORMATS) * n_per_format

  bank <- generate_questions_for_topic(
    topic_id = topic_id,
    n_per_format = n_per_format,
    model = model,
    api_key = api_key,
    csv_path = csv_path,
    json_path = json_path,
    raw_dir = raw_dir,
    errors_path = errors_path
  )

  print(bank %>% count(format, name = "n"))
  validation <- validate_question_bank(bank)
  print(validation)

  if (nrow(bank) < expected_rows) {
    stop(
      glue("Expected at least {expected_rows} rows for topic_id '{topic_id}', but received {nrow(bank)}."),
      call. = FALSE
    )
  }

  if (!isTRUE(validation$valid)) {
    stop(
      glue("Generated bank failed validation for topic_id '{topic_id}':\n- {paste(validation$errors, collapse = '\n- ')}"),
      call. = FALSE
    )
  }

  invisible(bank)
}

generate_full_question_bank <- function(n_per_format = 3,
                                        model = NULL,
                                        api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                                        csv_path = QUESTION_BANK_OUTPUT_CSV,
                                        json_path = QUESTION_BANK_OUTPUT_JSON,
                                        raw_dir = QUESTION_BANK_RAW_DIR,
                                        errors_path = QUESTION_BANK_ERRORS_CSV) {
  require_generation_environment(api_key)

  successful_chunks <- list()
  chunk_index <- 1L

  for (topic_id in OFFICIAL_TOPIC_STRUCTURE$topic_id) {
    for (format_name in VALID_FORMATS) {
      chunk <- tryCatch(
        generate_questions_for_topic_format(
          topic_id = topic_id,
          format = format_name,
          n = n_per_format,
          model = model,
          api_key = api_key,
          csv_path = csv_path,
          json_path = json_path,
          raw_dir = raw_dir,
          errors_path = errors_path
        ),
        error = function(e) {
          message(conditionMessage(e))
          empty_question_bank()
        }
      )

      if (nrow(chunk) > 0) {
        successful_chunks[[chunk_index]] <- chunk
        chunk_index <- chunk_index + 1L
      }
    }
  }

  if (length(successful_chunks) == 0) {
    stop("No question chunks were generated successfully.", call. = FALSE)
  }

  combined_bank <- bind_rows(successful_chunks) %>%
    mutate(
      module_id = vapply(module_id, normalize_module_id_value, character(1)),
      interaction_type = pmap_chr(list(format, question_text, interaction_type), normalize_interaction_type_value)
    ) %>%
    left_join(select(OFFICIAL_TOPIC_STRUCTURE, module_id, module_order, topic_id, topic_order), by = c("module_id", "topic_id")) %>%
    arrange(module_order, topic_order, format, difficulty, question_id) %>%
    select(all_of(REQUIRED_QUESTION_FIELDS))

  deduped_bank <- remove_duplicate_questions(combined_bank)
  replenished_bank <- replenish_missing_questions(
    deduped_bank,
    n_per_format = n_per_format,
    max_attempts = 2,
    model = model,
    api_key = api_key,
    csv_path = csv_path,
    json_path = json_path,
    raw_dir = raw_dir,
    errors_path = errors_path
  ) %>%
    mutate(
      module_id = vapply(module_id, normalize_module_id_value, character(1)),
      interaction_type = pmap_chr(list(format, question_text, interaction_type), normalize_interaction_type_value)
    )

  missing_counts <- check_expected_counts(replenished_bank, n_per_format = n_per_format)
  validation <- validate_question_bank(replenished_bank)

  if (nrow(replenished_bank) == 0) {
    stop("Full bank generation produced zero rows after deduplication and replenishment.", call. = FALSE)
  }

  if (nrow(missing_counts) > 0 || !isTRUE(validation$valid)) {
    missing_text <- if (nrow(missing_counts) > 0) {
      paste(glue("{missing_counts$topic_id}/{missing_counts$format}: need {missing_counts$shortfall} more"), collapse = "\n- ")
    } else {
      "None"
    }
    validation_text <- if (length(validation$errors) > 0) {
      paste(validation$errors, collapse = "\n- ")
    } else {
      "None"
    }

    stop(
      glue(
        "Final bank failed post-processing.\nMissing topic/format counts:\n- {missing_text}\nValidation errors:\n- {validation_text}"
      ),
      call. = FALSE
    )
  }

  replenished_bank
}
