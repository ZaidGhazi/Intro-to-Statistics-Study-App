library(dplyr)
library(purrr)
library(stringr)
library(tibble)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

build_alias_table <- function() {
  tibble::tribble(
    ~canonical, ~aliases, ~concept_hint,
    "p_hat", list(c("p^", "p̂", "p\u0302", "p-hat", "p hat", "phat", "sample proportion", "sample prop")), "sample_proportion",
    "p_0", list(c("p0", "p_0", "p naught", "p-naught", "p null", "null proportion", "hypothesized proportion", "hypothesized prop")), "null_proportion",
    "x_bar", list(c("xbar", "x-bar", "x̄", "x\u0304", "x bar", "sample mean")), "sample_mean",
    "mu_0", list(c("mu0", "μ0", "mu_0", "mu 0", "mu naught", "mu null", "hypothesized mean")), "null_mean",
    "standard error", list(c("se", "std error", "std. error", "standard err", "standrd error")), "standard_error",
    "confidence interval", list(c("ci", "conf interval", "conf. interval", "confidence int", "confidance interval")), "confidence_interval",
    "hypothesis test", list(c("hyp test", "hyo test", "hyptest", "hypothesis testing", "hypthesis test", "hypotesis test", "hypothisis test", "significance test")), "hypothesis_testing",
    "z_star", list(c("zstar", "z*", "z star", "z critical", "z crit")), "critical_value",
    "t_star", list(c("tstar", "t*", "t star", "t critical", "t crit")), "critical_value",
    "degrees of freedom", list(c("df", "d.f.", "deg freedom", "degree freedom")), "degrees_freedom",
    "p_value", list(c("pvalue", "p-value", "p value", "p val", "p-val")), "p_value",
    "null hypothesis", list(c("h0", "h_0", "null hyp", "null")), "null_hypothesis",
    "alternative hypothesis", list(c("ha", "h_a", "alt hypothesis", "alternative", "alt hyp")), "alternative_hypothesis"
  ) %>%
    tidyr::unnest_longer(aliases, values_to = "alias") %>%
    mutate(
      alias = as.character(alias),
      normalized_alias = normalize_alias_key(alias)
    ) %>%
    arrange(desc(nchar(alias)), canonical)
}

normalize_alias_key <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("[\u2018\u2019]", "'") %>%
    str_replace_all("[\u2010-\u2015]", "-") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
}

fix_common_stat_misspellings <- function(x) {
  x %>%
    str_replace_all(regex("\\bhyo\\s+test\\b", ignore_case = TRUE), "hypothesis test") %>%
    str_replace_all(regex("\\bhypthesis\\b|\\bhypotesis\\b|\\bhypothisis\\b", ignore_case = TRUE), "hypothesis") %>%
    str_replace_all(regex("\\bconfidance\\b|\\bconfedence\\b", ignore_case = TRUE), "confidence") %>%
    str_replace_all(regex("\\bsignficance\\b|\\bsignifigance\\b", ignore_case = TRUE), "significance") %>%
    str_replace_all(regex("\\bstandrd\\b|\\bstandar\\b", ignore_case = TRUE), "standard")
}

alias_regex <- function(alias) {
  escaped <- stringr::str_replace_all(alias, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1")
  if (str_detect(alias, "^[[:alnum:]_ ]+$")) {
    paste0("(?<![[:alnum:]_])", escaped, "(?![[:alnum:]_])")
  } else {
    escaped
  }
}

is_plain_word_alias <- function(alias) {
  str_detect(alias %||% "", "^[[:alnum:]_ ]+$")
}

replace_literal_alias <- function(text, alias, replacement) {
  alias <- as.character(alias %||% "")
  if (!nzchar(alias) || !grepl(alias, text, fixed = TRUE)) {
    return(list(text = text, matched = FALSE))
  }
  list(
    text = gsub(alias, paste0(" ", replacement, " "), text, fixed = TRUE),
    matched = TRUE
  )
}

apply_core_notation_aliases <- function(text) {
  literal_map <- list(
    p_hat = c("p^", "p\u0302", "p-hat", "p hat", "phat"),
    p_0 = c("p0", "p_0", "p naught", "p-naught"),
    x_bar = c("xbar", "x-bar", "x\u0304", "x bar"),
    mu_0 = c("mu0", "mu_0", "mu 0", "mu naught", "\u03bc0"),
    z_star = c("zstar", "z*", "z star"),
    t_star = c("tstar", "t*", "t star")
  )
  aliases_used <- character()
  for (canonical in names(literal_map)) {
    for (alias in literal_map[[canonical]]) {
      replaced <- replace_literal_alias(text, normalize_alias_key(alias), canonical)
      if (isTRUE(replaced$matched)) {
        aliases_used <- c(aliases_used, canonical)
        text <- str_squish(replaced$text)
      }
    }
  }
  list(text = text, aliases_added = aliases_used)
}

apply_alias_replacements <- function(text, return_aliases = FALSE) {
  cleaned <- normalize_alias_key(fix_common_stat_misspellings(text %||% ""))
  table <- build_alias_table()
  aliases_used <- character()
  core_aliases <- apply_core_notation_aliases(cleaned)
  cleaned <- core_aliases$text
  aliases_used <- c(aliases_used, core_aliases$aliases_added)

  for (i in seq_len(nrow(table))) {
    alias <- table$normalized_alias[[i]]
    canonical <- table$canonical[[i]]

    if (is_plain_word_alias(alias)) {
      detector <- regex(alias_regex(alias), ignore_case = TRUE)
      if (str_detect(cleaned, detector)) {
        aliases_used <- c(aliases_used, canonical)
        cleaned <- str_replace_all(cleaned, detector, paste0(" ", canonical, " "))
        cleaned <- str_squish(cleaned)
      }
    } else {
      replaced <- replace_literal_alias(cleaned, alias, canonical)
      if (isTRUE(replaced$matched)) {
        aliases_used <- c(aliases_used, canonical)
        cleaned <- str_squish(replaced$text)
      }
    }
  }

  # A tiny fuzzy layer for common short typos where edit distance is safe.
  tokens <- str_split(cleaned, "\\s+")[[1]]
  tokens <- vapply(tokens, function(token) {
    if (nchar(token) < 5 || str_detect(token, "_")) {
      return(token)
    }
    candidates <- c("hypothesis", "confidence", "proportion", "standard", "interval", "significance")
    distances <- utils::adist(token, candidates)
    best <- which.min(distances)
    if (length(best) == 1 && distances[[best]] <= 2) candidates[[best]] else token
  }, FUN.VALUE = character(1))
  cleaned <- str_squish(paste(tokens, collapse = " "))

  if (isTRUE(return_aliases)) {
    list(text = cleaned, aliases_added = paste(unique(aliases_used), collapse = "|"))
  } else {
    cleaned
  }
}

normalize_student_query <- function(query) {
  vapply(query %||% character(), apply_alias_replacements, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

normalize_chunk_text <- function(text) {
  normalized <- vapply(text %||% character(), apply_alias_replacements, FUN.VALUE = character(1), USE.NAMES = FALSE)
  normalized %>%
    str_replace_all("[^[:alnum:]_ ]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
}

expand_query <- function(query, max_queries = 8L) {
  original <- str_squish(as.character(query %||% ""))
  normalized <- normalize_student_query(original)
  table <- build_alias_table()
  canonicals <- table$canonical[table$canonical %in% str_split(normalized, "\\s+")[[1]]]

  alias_expansions <- character()
  if (length(canonicals) > 0) {
    alias_expansions <- table %>%
      filter(canonical %in% canonicals) %>%
      group_by(canonical) %>%
      summarise(alias_text = paste(head(alias, 4), collapse = " "), .groups = "drop") %>%
      transmute(query = str_squish(paste(normalized, alias_text))) %>%
      pull(query)
  }

  intent_expansions <- c()
  if (str_detect(normalized, "p_hat|p_0|proportion")) {
    intent_expansions <- c(intent_expansions, paste(normalized, "population proportion sample proportion one proportion"))
  }
  if (str_detect(normalized, "x_bar|mu_0|mean")) {
    intent_expansions <- c(intent_expansions, paste(normalized, "population mean sample mean t distribution"))
  }
  if (str_detect(normalized, "hypothesis test|p_value|null hypothesis")) {
    intent_expansions <- c(intent_expansions, paste(normalized, "null alternative p_value alpha reject fail to reject"))
  }
  if (str_detect(normalized, "confidence interval|z_star|t_star")) {
    intent_expansions <- c(intent_expansions, paste(normalized, "margin of error critical value interval interpretation"))
  }

  queries <- unique(str_squish(c(original, normalized, alias_expansions, intent_expansions)))
  queries <- queries[nzchar(queries)]
  head(queries, max_queries)
}
