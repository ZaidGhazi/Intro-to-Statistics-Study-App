if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

is_visual_request <- function(text) {
  stringr::str_detect(
    stringr::str_to_lower(text %||% ""),
    "visual|visually|diagram|plot|graph|histogram|picture|draw|show me|curve|shade|number line|scatterplot|boxplot"
  )
}

normalize_visual_type <- function(visual_type = NULL) {
  visual_type <- stringr::str_to_lower(as.character(visual_type %||% ""))
  visual_type <- stringr::str_replace_all(visual_type, "[^a-z0-9]+", "_")
  visual_type <- stringr::str_replace_all(visual_type, "^_|_$", "")
  dplyr::case_when(
    visual_type %in% c(
      "mean_vs_median_skew", "right_skew_mean_median", "right_skewed_mean_median",
      "skew_mean_median", "resistant_measures", "mean_median_outliers", "skewness_outliers"
    ) ~ "mean_vs_median_skew",
    visual_type %in% c("outlier_boxplot", "boxplot_outliers", "outliers_boxplot") ~ "outlier_boxplot",
    visual_type %in% c("bar_vs_histogram", "bar_chart_vs_histogram", "graph_type_comparison") ~ "bar_vs_histogram",
    visual_type %in% c("recreated_p_value_tail_area", "p_value_tail_area", "p_value_tail", "tail_area", "hypothesis_test_tail") ~ "p_value_tail_area",
    visual_type %in% c("recreated_confidence_interval_number_line", "confidence_interval_number_line", "ci_number_line", "margin_of_error_number_line") ~ "confidence_interval_number_line",
    visual_type %in% c("recreated_standard_normal_curve", "standard_normal_curve", "normal_curve_shading", "z_score_curve") ~ "standard_normal_curve",
    visual_type %in% c("recreated_scatterplot_association", "scatterplot_association", "correlation_scatterplot") ~ "scatterplot_association",
    visual_type %in% c("recreated_bar_chart_categorical", "bar_chart_categorical") ~ "bar_chart_categorical",
    visual_type %in% c("recreated_histogram_quantitative", "histogram_quantitative") ~ "histogram_quantitative",
    visual_type %in% c("sampling_distribution_clt", "clt_sampling_distribution", "recreated_sampling_distribution_clt") ~ "sampling_distribution_clt",
    nzchar(visual_type) ~ visual_type,
    TRUE ~ NA_character_
  )
}

question_field_text <- function(current_question = NULL, field = NULL) {
  if (is.null(current_question) || is.null(field)) {
    return("")
  }
  value <- current_question[[field]] %||% ""
  if (is.list(value) && !is.data.frame(value)) {
    value <- unlist(value, use.names = FALSE)
  }
  paste(as.character(value %||% ""), collapse = " ")
}

question_combined_text <- function(current_question = NULL, user_text = NULL, concept_tag = NULL, module_id = NULL) {
  stringr::str_to_lower(paste(
    user_text %||% "",
    concept_tag %||% "",
    module_id %||% "",
    question_field_text(current_question, "question_text"),
    question_field_text(current_question, "concept_tag"),
    question_field_text(current_question, "topic_id"),
    question_field_text(current_question, "module_id"),
    question_field_text(current_question, "correct_answer"),
    question_field_text(current_question, "accepted_answers"),
    collapse = " "
  ))
}

choose_visual_type <- function(user_text = NULL,
                               current_question = NULL,
                               concept_tag = NULL,
                               module_id = NULL) {
  combined <- question_combined_text(current_question, user_text, concept_tag, module_id)

  if (stringr::str_detect(combined, "resistant|nonresistant|non resistant|right-skew|right skew|skewed|outlier|extreme value|mean|median|measure of center")) {
    return("mean_vs_median_skew")
  }
  if (stringr::str_detect(combined, "p[- ]?value|p value|tail area|rejection region|significance|hypothesis test")) {
    return("p_value_tail_area")
  }
  if (stringr::str_detect(combined, "confidence interval|margin of error|plausible range|lower endpoint|upper endpoint")) {
    return("confidence_interval_number_line")
  }
  if (stringr::str_detect(combined, "z[- ]?score|normal curve|standard normal|bell curve|normal probability|empirical rule")) {
    return("standard_normal_curve")
  }
  if (stringr::str_detect(combined, "scatterplot|correlation|association|regression|slope|explanatory|response")) {
    return("scatterplot_association")
  }
  if (stringr::str_detect(combined, "bar chart|histogram|categorical|quantitative|variable type|graph selection")) {
    return("bar_vs_histogram")
  }
  if (stringr::str_detect(combined, "boxplot|iqr|interquartile|five number|five-number")) {
    return("outlier_boxplot")
  }
  if (stringr::str_detect(combined, "sampling distribution|central limit|clt|sample mean|sample proportion") &&
      stringr::str_detect(combined, "spread|standard error|sample size|larger n|smaller n|n increases")) {
    return("sampling_distribution_clt")
  }

  NULL
}

has_question_visual_link <- function(current_question = NULL) {
  if (is.null(current_question)) {
    return(FALSE)
  }

  fields <- c("visual_id", "visual_ids", "visual_template_id", "tutor_visual_ids")

  any(vapply(fields, function(field) {
    value <- current_question[[field]] %||% ""
    if (is.list(value) && !is.data.frame(value)) {
      value <- unlist(value, use.names = FALSE)
    }
    value <- as.character(value %||% "")
    any(nzchar(value) & !is.na(value))
  }, logical(1)))
}

strict_question_visual_type <- function(current_question = NULL) {
  if (is.null(current_question)) {
    return(NULL)
  }

  explicit_template <- normalize_visual_type(current_question$visual_template_id %||% "")
  if (!is.na(explicit_template) && nzchar(explicit_template) && isTRUE(current_question$visual_required %||% FALSE)) {
    return(explicit_template)
  }

  combined <- question_combined_text(current_question)

  # Keep automatic visuals conservative. A normal hint/concept response should
  # only get a visual when the current question itself clearly matches a visual template.
  if (
    stringr::str_detect(combined, "resistant_measures|mean_vs_median|skewness_outliers") ||
      (
        stringr::str_detect(combined, "right-skew|right skew|skewed|outlier|extreme value|resistant") &&
          stringr::str_detect(combined, "mean|median|measure of center|center")
      )
  ) {
    return("mean_vs_median_skew")
  }

  if (
    stringr::str_detect(combined, "outlier_boxplot|boxplot_outliers") ||
      (
        stringr::str_detect(combined, "boxplot|outlier|iqr|interquartile") &&
          stringr::str_detect(combined, "spread|quartile|iqr|five-number|five number")
      )
  ) {
    return("outlier_boxplot")
  }

  if (
    stringr::str_detect(combined, "graph_choice|variable_classification|bar_vs_histogram") &&
      stringr::str_detect(combined, "bar chart|histogram|categorical|quantitative|variable type|display|graph")
  ) {
    return("bar_vs_histogram")
  }

  if (
    stringr::str_detect(combined, "p_value_interpretation|p[- ]?value|tail area|rejection region") &&
      stringr::str_detect(combined, "hypothesis|test|null|alpha|significance|shade|tail")
  ) {
    return("p_value_tail_area")
  }

  if (
    stringr::str_detect(combined, "confidence interval|margin of error|plausible range") &&
      stringr::str_detect(combined, "number line|endpoint|interval|estimate|margin")
  ) {
    return("confidence_interval_number_line")
  }

  if (
    stringr::str_detect(combined, "z[- ]?score|standard normal|normal curve|normal probability|shade|empirical rule")
  ) {
    return("standard_normal_curve")
  }

  if (
    stringr::str_detect(combined, "scatterplot|correlation|association|regression|slope")
  ) {
    return("scatterplot_association")
  }

  if (
    stringr::str_detect(combined, "sampling distribution|central limit|clt") &&
      stringr::str_detect(combined, "spread|standard error|sample size|larger n|smaller n|n increases")
  ) {
    return("sampling_distribution_clt")
  }

  NULL
}

should_attach_visual_for_help <- function(help_mode = NULL,
                                          current_question = NULL,
                                          user_text = NULL) {
  if (is_visual_request(user_text)) {
    return(TRUE)
  }

  # Quick action buttons should only include visuals when the current question
  # has a high-confidence visual match. Do not attach visuals merely because a
  # broad retrieved concept page found a generic visual.
  if (!help_mode %in% c("hint", "concept")) {
    return(FALSE)
  }

  !is.null(strict_question_visual_type(current_question))
}

plot_mean_vs_median_skew <- function() {
  set.seed(123)
  incomes <- c(
    stats::rlnorm(250, meanlog = log(60000), sdlog = 0.35),
    250000, 350000, 500000
  )

  df <- tibble::tibble(income = incomes)
  mean_income <- mean(df$income)
  median_income <- stats::median(df$income)
  dollar_label <- function(x) paste0("$", format(round(x), big.mark = ",", scientific = FALSE))

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = income)) +
        ggplot2::geom_histogram(bins = 35, color = "white", fill = "#4f7cac") +
        ggplot2::geom_vline(xintercept = median_income, linetype = "dashed", linewidth = 1.05, color = "#2a9d8f") +
        ggplot2::geom_vline(xintercept = mean_income, linewidth = 1.05, color = "#d1495b") +
        ggplot2::annotate("text", x = median_income, y = Inf, label = "Median", vjust = 2, hjust = -0.1, color = "#2a9d8f") +
        ggplot2::annotate("text", x = mean_income, y = Inf, label = "Mean", vjust = 4, hjust = -0.1, color = "#d1495b") +
        ggplot2::labs(
          x = "Household income",
          y = "Number of households",
          title = "Right-skewed income distribution",
          subtitle = "A few very high incomes pull the mean to the right"
        ) +
        ggplot2::scale_x_continuous(labels = dollar_label) +
        ggplot2::theme_minimal(base_size = 13)
    )
  }

  graphics::hist(
    df$income,
    breaks = 35,
    col = "#4f7cac",
    border = "white",
    main = "Right-skewed income distribution",
    xlab = "Household income",
    ylab = "Number of households",
    axes = FALSE
  )
  graphics::axis(1, at = graphics::axTicks(1), labels = dollar_label(graphics::axTicks(1)))
  graphics::axis(2)
  graphics::box()
  graphics::abline(v = median_income, lty = 2, lwd = 2, col = "#2a9d8f")
  graphics::abline(v = mean_income, lty = 1, lwd = 2, col = "#d1495b")
  graphics::legend("topright", legend = c("Median", "Mean"), lty = c(2, 1), lwd = 2, col = c("#2a9d8f", "#d1495b"), bty = "n")
  invisible(NULL)
}

plot_outlier_boxplot <- function() {
  set.seed(2331)
  values <- c(stats::rnorm(80, mean = 50, sd = 8), 92, 105, 118)
  df <- tibble::tibble(group = "Class data", value = values)

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = group, y = value)) +
        ggplot2::geom_boxplot(width = 0.35, fill = "#f2cc8f", color = "#3d405b", outlier.color = "#d1495b", outlier.size = 3) +
        ggplot2::geom_jitter(width = 0.08, alpha = 0.35, color = "#4f7cac") +
        ggplot2::labs(x = NULL, y = "Value", title = "Boxplot with high outliers", subtitle = "Extreme values stand apart from the main cluster") +
        ggplot2::theme_minimal(base_size = 13)
    )
  }

  graphics::boxplot(values, horizontal = TRUE, col = "#f2cc8f", main = "Boxplot with high outliers", xlab = "Value")
  graphics::stripchart(values, method = "jitter", add = TRUE, pch = 16, col = "#4f7cac")
  invisible(NULL)
}

plot_bar_vs_histogram <- function() {
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  graphics::barplot(height = c(18, 31, 24, 12), names.arg = c("A", "B", "C", "D"), col = "#81b29a", border = "white", main = "Bar chart", ylab = "Count")
  graphics::mtext("Separate categories", side = 3, line = 0.2, cex = 0.8)
  set.seed(42)
  values <- stats::rnorm(180, mean = 70, sd = 10)
  graphics::hist(values, breaks = 12, col = "#4f7cac", border = "white", main = "Histogram", xlab = "Quantitative value", ylab = "Count")
  graphics::mtext("Touching bins", side = 3, line = 0.2, cex = 0.8)
  invisible(NULL)
}

plot_p_value_tail_area <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    x <- seq(-3.5, 3.5, length.out = 500)
    df <- tibble::tibble(x = x, density = stats::dnorm(x), shade = x >= 1.65)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = density)) +
        ggplot2::geom_area(data = dplyr::filter(df, shade), fill = "#d1495b", alpha = 0.45) +
        ggplot2::geom_line(linewidth = 1, color = "#3d405b") +
        ggplot2::geom_vline(xintercept = 1.65, linetype = "dashed", color = "#d1495b") +
        ggplot2::annotate("text", x = 2.25, y = 0.12, label = "p-value\n(shaded tail)", color = "#d1495b") +
        ggplot2::labs(title = "P-value as a tail area", subtitle = "Assuming the null model is true, how unusual is the observed statistic?", x = "Test statistic under H0", y = "Density") +
        ggplot2::theme_minimal(base_size = 13)
    )
  }
  x <- seq(-3.5, 3.5, length.out = 500)
  y <- stats::dnorm(x)
  graphics::plot(x, y, type = "l", lwd = 2, main = "P-value as a tail area", xlab = "Test statistic under H0", ylab = "Density")
  idx <- x >= 1.65
  graphics::polygon(c(1.65, x[idx], max(x[idx])), c(0, y[idx], 0), col = "#d1495b55", border = NA)
  invisible(NULL)
}

plot_confidence_interval_number_line <- function() {
  est <- 52
  moe <- 6
  lower <- est - moe
  upper <- est + moe
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    df <- tibble::tibble(x = c(lower, est, upper), label = c("Lower", "Estimate", "Upper"))
    return(
      ggplot2::ggplot() +
        ggplot2::geom_segment(ggplot2::aes(x = lower, xend = upper, y = 0, yend = 0), linewidth = 2, color = "#4f7cac") +
        ggplot2::geom_point(data = df, ggplot2::aes(x = x, y = 0), size = 4, color = "#3d405b") +
        ggplot2::geom_text(data = df, ggplot2::aes(x = x, y = 0.08, label = label), vjust = 0, size = 4) +
        ggplot2::annotate("text", x = est, y = -0.12, label = "estimate ± margin of error", color = "#3d405b") +
        ggplot2::scale_y_continuous(limits = c(-0.25, 0.25), breaks = NULL) +
        ggplot2::labs(title = "Confidence interval on a number line", subtitle = "The interval gives plausible values for the population parameter", x = "Parameter value", y = NULL) +
        ggplot2::theme_minimal(base_size = 13)
    )
  }
  graphics::plot(c(lower - 5, upper + 5), c(0, 0), type = "n", yaxt = "n", ylab = "", xlab = "Parameter value", main = "Confidence interval")
  graphics::segments(lower, 0, upper, 0, lwd = 4, col = "#4f7cac")
  graphics::points(c(lower, est, upper), c(0, 0, 0), pch = 19, cex = 1.4)
  invisible(NULL)
}

plot_standard_normal_curve <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    x <- seq(-3.5, 3.5, length.out = 500)
    df <- tibble::tibble(x = x, density = stats::dnorm(x), shade = x >= 1)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = density)) +
        ggplot2::geom_area(data = dplyr::filter(df, shade), fill = "#4f7cac", alpha = 0.35) +
        ggplot2::geom_line(linewidth = 1, color = "#3d405b") +
        ggplot2::geom_vline(xintercept = 0, color = "#3d405b") +
        ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "#4f7cac") +
        ggplot2::annotate("text", x = 1.35, y = 0.18, label = "z = 1") +
        ggplot2::labs(title = "Standard normal curve", subtitle = "A z-score marks location in standard-deviation units", x = "z", y = "Density") +
        ggplot2::theme_minimal(base_size = 13)
    )
  }
  x <- seq(-3.5, 3.5, length.out = 500)
  graphics::plot(x, stats::dnorm(x), type = "l", lwd = 2, main = "Standard normal curve", xlab = "z", ylab = "Density")
  graphics::abline(v = c(0, 1), lty = c(1, 2))
  invisible(NULL)
}

plot_scatterplot_association <- function() {
  set.seed(2026)
  x <- seq(1, 50)
  y <- 3 + 0.8 * x + stats::rnorm(length(x), sd = 6)
  df <- tibble::tibble(x = x, y = y)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(size = 2.4, color = "#4f7cac") +
        ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#d1495b", linewidth = 1) +
        ggplot2::labs(title = "Positive association in a scatterplot", subtitle = "As x increases, y tends to increase", x = "Explanatory variable", y = "Response variable") +
        ggplot2::theme_minimal(base_size = 13)
    )
  }
  graphics::plot(x, y, pch = 19, col = "#4f7cac", main = "Positive association", xlab = "Explanatory variable", ylab = "Response variable")
  graphics::abline(stats::lm(y ~ x), col = "#d1495b", lwd = 2)
  invisible(NULL)
}

plot_bar_chart_categorical <- function() {
  counts <- c(Instagram = 28, TikTok = 34, YouTube = 22, Other = 11)
  graphics::barplot(counts, col = "#81b29a", border = "white", main = "Bar chart for categorical data", ylab = "Count")
  invisible(NULL)
}

plot_histogram_quantitative <- function() {
  set.seed(88)
  values <- stats::rnorm(160, mean = 72, sd = 9)
  graphics::hist(values, breaks = 12, col = "#4f7cac", border = "white", main = "Histogram for quantitative data", xlab = "Exam score", ylab = "Count")
  invisible(NULL)
}

plot_sampling_distribution_clt <- function() {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    x <- seq(40, 80, length.out = 400)
    df <- dplyr::bind_rows(
      tibble::tibble(x = x, density = stats::dnorm(x, mean = 60, sd = 7), group = "Small n"),
      tibble::tibble(x = x, density = stats::dnorm(x, mean = 60, sd = 3), group = "Large n")
    )
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = density, linetype = group)) +
        ggplot2::geom_line(linewidth = 1.1, color = "#3d405b") +
        ggplot2::labs(title = "Sampling distributions get narrower as n grows", subtitle = "Larger samples reduce the standard error", x = "Sample statistic", y = "Density", linetype = NULL) +
        ggplot2::theme_minimal(base_size = 13)
    )
  }
  x <- seq(40, 80, length.out = 400)
  graphics::plot(x, stats::dnorm(x, 60, 7), type = "l", lwd = 2, lty = 2, main = "Sampling distributions", xlab = "Sample statistic", ylab = "Density")
  graphics::lines(x, stats::dnorm(x, 60, 3), lwd = 2)
  graphics::legend("topright", c("Small n", "Large n"), lty = c(2, 1), lwd = 2, bty = "n")
  invisible(NULL)
}

visual_caption_for_type <- function(visual_type) {
  visual_type <- normalize_visual_type(visual_type)
  switch(
    visual_type,
    mean_vs_median_skew = "In a right-skewed distribution, a few extreme high values pull the mean to the right. The median stays closer to the typical case, which is why it is more resistant.",
    outlier_boxplot = "A boxplot makes outliers visible as points beyond the main spread. Those extreme values can strongly affect nonresistant summaries like the mean.",
    bar_vs_histogram = "A bar chart separates categories, while a histogram uses touching bins for a quantitative variable.",
    p_value_tail_area = "The shaded tail represents results as extreme as, or more extreme than, the observed statistic under the null model.",
    confidence_interval_number_line = "A confidence interval is estimate ± margin of error, shown as a plausible range for the population parameter.",
    standard_normal_curve = "A z-score marks how many standard deviations a value is from the mean on the standard normal curve.",
    scatterplot_association = "A scatterplot shows the direction, form, and strength of association between two quantitative variables.",
    bar_chart_categorical = "A bar chart compares counts across categories.",
    histogram_quantitative = "A histogram shows the distribution of a quantitative variable across adjacent intervals.",
    sampling_distribution_clt = "A larger sample size reduces standard error, so the sampling distribution is narrower.",
    "This visual is an illustrative aid for the current practice question."
  )
}

visual_response_for_type <- function(visual_type) {
  visual_type <- normalize_visual_type(visual_type)
  response <- switch(
    visual_type,
    mean_vs_median_skew = paste("### Visual aid", "", "I added a visual below. Notice how the high values create a long right tail.", "", "- The **mean** moves toward that tail.", "- The **median** stays closer to the main cluster.", "- That is why the median is usually more resistant for skewed data with outliers.", "", "**Think about this:** Based on the visual, which measure better represents a typical value: the mean or the median?", sep = "\n"),
    outlier_boxplot = paste("### Visual aid", "", "I added a visual below. Notice how the extreme points sit away from the main cluster.", "", "- Outliers can strongly affect nonresistant summaries.", "- Resistant summaries stay closer to the typical values.", "", "**Think about this:** Which summary would you trust more if you want a typical value?", sep = "\n"),
    bar_vs_histogram = paste("### Visual aid", "", "I added a visual below comparing a bar chart with a histogram.", "", "- A **bar chart** uses separated bars for categories.", "- A **histogram** uses touching bins for quantitative values.", "", "**Quick check:** Is the variable a category label or a measured number?", sep = "\n"),
    p_value_tail_area = paste("### Visual aid", "", "I added a visual below. The shaded tail shows the p-value idea.", "", "- Start by assuming the null hypothesis is true.", "- Then ask how far into the tail the observed statistic falls.", "- Smaller shaded tail areas give stronger evidence against the null.", "", "**Quick check:** Is the shaded area large enough to be common under the null, or small enough to be surprising?", sep = "\n"),
    confidence_interval_number_line = paste("### Visual aid", "", "I added a number-line view of a confidence interval.", "", "- The center is the sample estimate.", "- The endpoints are estimate ± margin of error.", "- The whole interval is a plausible range for the population parameter.", "", "**Quick check:** Are you describing the sample statistic or the unknown population parameter?", sep = "\n"),
    standard_normal_curve = paste("### Visual aid", "", "I added a normal-curve view.", "", "- The center is the mean.", "- A z-score marks distance from the mean in standard-deviation units.", "- Shaded regions represent probabilities.", "", "**Quick check:** Is the value above or below the mean?", sep = "\n"),
    scatterplot_association = paste("### Visual aid", "", "I added a scatterplot view of association.", "", "- Each point represents one case.", "- The overall pattern shows direction and strength.", "- A trend does not prove causation by itself.", "", "**Quick check:** Does the problem ask about association, prediction, or causation?", sep = "\n"),
    sampling_distribution_clt = paste("### Visual aid", "", "I added a sampling-distribution view.", "", "- Larger samples make the sampling distribution narrower.", "- The center stays near the population parameter when the statistic is unbiased.", "- The spread is the standard error.", "", "**Quick check:** What happens to standard error when n increases?", sep = "\n"),
    paste("### Visual aid", "", "I added a visual below. Use the picture to connect the shape, area, or pattern back to the current question.", "", "**Quick check:** What part of the visual matches the wording of the question?", sep = "\n")
  )
  stringr::str_trim(response)
}

deterministic_visual_message <- function(visual_type,
                                         message_id = NULL,
                                         file_path = NULL,
                                         src = NULL,
                                         module_id = NULL,
                                         concept_tag = NULL) {
  visual_type <- normalize_visual_type(visual_type)
  list(
    message_id = message_id %||% NA_character_,
    visual_id = visual_type %||% "deterministic_visual",
    visual_type = "image",
    render_function = paste0("plot_", visual_type),
    file_path = file_path %||% NA_character_,
    src = src %||% NA_character_,
    caption = visual_caption_for_type(visual_type),
    source_type = "recreated_visual",
    display_permission_status = "created_by_us",
    safe_for_deployment = TRUE,
    module_id = module_id %||% NA_character_,
    concept_tag = concept_tag %||% NA_character_
  )
}

save_stat2331_visual_png <- function(visual_type,
                                     message_id = NULL,
                                     module_id = NULL,
                                     concept_tag = NULL,
                                     output_dir = file.path("www", "session_visuals")) {
  visual_type <- normalize_visual_type(visual_type)
  if (is.null(visual_type) || !nzchar(visual_type %||% "")) {
    return(NULL)
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  safe_message_id <- stringr::str_replace_all(message_id %||% format(Sys.time(), "%Y%m%d%H%M%OS3"), "[^A-Za-z0-9_\\-]", "_")
  safe_visual_type <- stringr::str_replace_all(visual_type, "[^A-Za-z0-9_\\-]", "_")
  file_name <- paste0(safe_message_id, "_", safe_visual_type, ".png")
  file_path <- file.path(output_dir, file_name)

  opened <- FALSE
  tryCatch(
    {
      grDevices::png(filename = file_path, width = 900, height = 560, res = 120)
      opened <- TRUE
      render_stat2331_visual(visual_type)
      grDevices::dev.off()
      opened <- FALSE
      deterministic_visual_message(
        visual_type = visual_type,
        message_id = message_id,
        file_path = normalizePath(file_path, winslash = "/", mustWork = FALSE),
        src = paste0("session_visuals/", file_name),
        module_id = module_id,
        concept_tag = concept_tag
      )
    },
    error = function(e) {
      if (isTRUE(opened)) {
        try(grDevices::dev.off(), silent = TRUE)
      }
      warning("Could not render deterministic intro stats visual: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

render_stat2331_visual <- function(visual_type) {
  visual_type <- normalize_visual_type(visual_type)
  plot_obj <- switch(
    visual_type,
    mean_vs_median_skew = plot_mean_vs_median_skew(),
    outlier_boxplot = plot_outlier_boxplot(),
    bar_vs_histogram = plot_bar_vs_histogram(),
    p_value_tail_area = plot_p_value_tail_area(),
    confidence_interval_number_line = plot_confidence_interval_number_line(),
    standard_normal_curve = plot_standard_normal_curve(),
    scatterplot_association = plot_scatterplot_association(),
    bar_chart_categorical = plot_bar_chart_categorical(),
    histogram_quantitative = plot_histogram_quantitative(),
    sampling_distribution_clt = plot_sampling_distribution_clt(),
    NULL
  )
  if (inherits(plot_obj, "ggplot")) {
    print(plot_obj)
  }
  invisible(plot_obj)
}

# -----------------------------------------------------------------------------
# 2026-05 Intro-statistics visual routing extension
# These later definitions intentionally override earlier helper functions so that
# visuals are attached only when the current question clearly supports them or the
# student explicitly asks for a visual explanation.
# -----------------------------------------------------------------------------

normalize_visual_type <- function(visual_type = NULL) {
  visual_type <- stringr::str_to_lower(as.character(visual_type %||% ""))
  visual_type <- stringr::str_replace_all(visual_type, "[^a-z0-9]+", "_")
  visual_type <- stringr::str_replace_all(visual_type, "^_|_$", "")
  dplyr::case_when(
    visual_type %in% c("mean_vs_median_skew", "right_skew_mean_median", "right_skewed_mean_median", "skew_mean_median", "resistant_measures", "mean_median_outliers", "skewness_outliers") ~ "mean_vs_median_skew",
    visual_type %in% c("outlier_boxplot", "boxplot_outliers", "outliers_boxplot") ~ "outlier_boxplot",
    visual_type %in% c("bar_vs_histogram", "bar_chart_vs_histogram", "graph_type_comparison") ~ "bar_vs_histogram",
    visual_type %in% c("recreated_p_value_tail_area", "p_value_tail_area", "p_value_tail", "tail_area", "hypothesis_test_tail") ~ "p_value_tail_area",
    visual_type %in% c("recreated_confidence_interval_number_line", "confidence_interval_number_line", "ci_number_line", "margin_of_error_number_line") ~ "confidence_interval_number_line",
    visual_type %in% c("recreated_standard_normal_curve", "standard_normal_curve", "normal_curve_shading", "z_score_curve") ~ "standard_normal_curve",
    visual_type %in% c("recreated_scatterplot_association", "scatterplot_association", "correlation_scatterplot") ~ "scatterplot_association",
    visual_type %in% c("regression_residual_plot", "residual_plot", "regression_residuals") ~ "regression_residual_plot",
    visual_type %in% c("recreated_bar_chart_categorical", "bar_chart_categorical") ~ "bar_chart_categorical",
    visual_type %in% c("recreated_histogram_quantitative", "histogram_quantitative") ~ "histogram_quantitative",
    visual_type %in% c("sampling_distribution_clt", "clt_sampling_distribution", "recreated_sampling_distribution_clt") ~ "sampling_distribution_clt",
    visual_type %in% c("binomial_distribution_bars", "binomial_bars", "binomial_distribution") ~ "binomial_distribution_bars",
    visual_type %in% c("experiment_randomization_diagram", "random_assignment_diagram", "randomization_diagram") ~ "experiment_randomization_diagram",
    visual_type %in% c("two_way_table_segmented_bar", "conditional_distribution_bars", "segmented_bar_two_way") ~ "two_way_table_segmented_bar",
    visual_type %in% c("comparing_groups_boxplots", "group_boxplots", "boxplots_compare_groups") ~ "comparing_groups_boxplots",
    nzchar(visual_type) ~ visual_type,
    TRUE ~ NA_character_
  )
}

choose_visual_type <- function(user_text = NULL,
                               current_question = NULL,
                               concept_tag = NULL,
                               module_id = NULL) {
  combined <- question_combined_text(current_question, user_text, concept_tag, module_id)

  if (stringr::str_detect(combined, "residual|observed minus predicted")) return("regression_residual_plot")
  if (stringr::str_detect(combined, "scatterplot|correlation|association|regression|slope|explanatory|response")) return("scatterplot_association")
  if (stringr::str_detect(combined, "binomial|successes|trials|bins")) return("binomial_distribution_bars")
  if (stringr::str_detect(combined, "random assignment|treatment|placebo|experiment|matched pairs|blocking|factor")) return("experiment_randomization_diagram")
  if (stringr::str_detect(combined, "two[- ]?way|conditional distribution|segmented bar|simpson")) return("two_way_table_segmented_bar")
  if (stringr::str_detect(combined, "p[- ]?value|p value|tail area|rejection region|significance|hypothesis test|alpha")) return("p_value_tail_area")
  if (stringr::str_detect(combined, "confidence interval|margin of error|plausible range|lower endpoint|upper endpoint|confidence level")) return("confidence_interval_number_line")
  if (stringr::str_detect(combined, "z[- ]?score|normal curve|standard normal|bell curve|normal probability|empirical rule")) return("standard_normal_curve")
  if (stringr::str_detect(combined, "sampling distribution|central limit|clt|standard error|sample size") && stringr::str_detect(combined, "spread|standard error|sample size|larger n|smaller n|n increases|n grows|sample mean|sample proportion")) return("sampling_distribution_clt")
  if (stringr::str_detect(combined, "boxplot|iqr|interquartile|five number|five-number|quartile")) return("outlier_boxplot")
  if (stringr::str_detect(combined, "resistant|nonresistant|non resistant|right-skew|right skew|skewed|outlier|extreme value|mean|median|measure of center")) return("mean_vs_median_skew")
  if (stringr::str_detect(combined, "bar chart|histogram|categorical|quantitative|variable type|graph selection")) return("bar_vs_histogram")
  NULL
}

question_has_visual_language <- function(current_question = NULL) {
  combined <- question_combined_text(current_question)
  stringr::str_detect(combined, "visual|graph|plot|chart|histogram|bar chart|scatterplot|curve|number line|diagram|shaded|boxplot|use the visual|figure")
}

strict_question_visual_type <- function(current_question = NULL) {
  if (is.null(current_question)) return(NULL)

  explicit_template <- normalize_visual_type(current_question$visual_template_id %||% "")
  explicit_allowed <- !is.na(explicit_template) && nzchar(explicit_template) &&
    (isTRUE(current_question$visual_required %||% FALSE) || question_has_visual_language(current_question))
  if (explicit_allowed) return(explicit_template)

  combined <- question_combined_text(current_question)
  if (stringr::str_detect(combined, "residual|observed minus predicted")) return("regression_residual_plot")
  if (stringr::str_detect(combined, "random assignment|treatment group|control group|placebo") && question_has_visual_language(current_question)) return("experiment_randomization_diagram")
  if (stringr::str_detect(combined, "binomial") && question_has_visual_language(current_question)) return("binomial_distribution_bars")
  if (stringr::str_detect(combined, "two[- ]?way|conditional distribution|segmented bar") && question_has_visual_language(current_question)) return("two_way_table_segmented_bar")
  if (stringr::str_detect(combined, "p[- ]?value|tail area|rejection region") && stringr::str_detect(combined, "hypothesis|test|null|alpha|significance|shade|tail")) return("p_value_tail_area")
  if (stringr::str_detect(combined, "confidence interval|margin of error|number line|endpoint")) return("confidence_interval_number_line")
  if (stringr::str_detect(combined, "z[- ]?score|standard normal|normal curve|normal probability|shade|empirical rule")) return("standard_normal_curve")
  if (stringr::str_detect(combined, "scatterplot|correlation|association|regression|slope") && question_has_visual_language(current_question)) return("scatterplot_association")
  if (stringr::str_detect(combined, "sampling distribution|central limit|clt") && stringr::str_detect(combined, "spread|standard error|sample size|larger n|smaller n|n increases|n grows")) return("sampling_distribution_clt")
  if (stringr::str_detect(combined, "boxplot|outlier|iqr|interquartile") && stringr::str_detect(combined, "spread|quartile|iqr|five-number|five number|boxplot")) return("outlier_boxplot")
  if (stringr::str_detect(combined, "right-skew|right skew|skewed|outlier|extreme value|resistant") && stringr::str_detect(combined, "mean|median|measure of center|center")) return("mean_vs_median_skew")
  if (stringr::str_detect(combined, "bar chart|histogram|categorical|quantitative|variable type") && question_has_visual_language(current_question)) return("bar_vs_histogram")
  NULL
}

should_attach_visual_for_help <- function(help_mode = NULL,
                                          current_question = NULL,
                                          user_text = NULL) {
  if (is_visual_request(user_text)) return(TRUE)
  if (!help_mode %in% c("hint", "concept")) return(FALSE)
  # For quick buttons, avoid forcing visuals. Show them only if the current
  # question itself is visual/graph-based or explicitly requires a visual.
  isTRUE(current_question$visual_required %||% FALSE) ||
    (question_has_visual_language(current_question) && !is.null(strict_question_visual_type(current_question)))
}

plot_binomial_distribution_bars <- function() {
  n <- 10; p <- 0.35; x <- 0:n
  probs <- stats::dbinom(x, size = n, prob = p)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    df <- tibble::tibble(successes = x, probability = probs)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = successes, y = probability)) +
        ggplot2::geom_col(fill = "#4f7cac", color = "white") +
        ggplot2::labs(title = "Binomial distribution", subtitle = "Probabilities for counts of successes", x = "Number of successes in 10 trials", y = "Probability") +
        ggplot2::theme_minimal(base_size = 13)
    )
  }
  graphics::barplot(probs, names.arg = x, col = "#4f7cac", border = "white", main = "Binomial distribution", xlab = "Successes", ylab = "Probability")
  invisible(NULL)
}

plot_experiment_randomization_diagram <- function() {
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  draw_box <- function(x, y, label) {
    graphics::rect(x, y, x + .22, y + .13, border = "#3d405b", lwd = 2)
    graphics::text(x + .11, y + .065, label, cex = .9)
  }
  draw_box(.07, .60, "Subjects")
  draw_box(.38, .60, "Random\nassignment")
  draw_box(.69, .75, "Treatment\ngroup")
  draw_box(.69, .45, "Control\ngroup")
  draw_box(.69, .15, "Compare\nresponses")
  graphics::arrows(.29, .665, .38, .665, length = .08, lwd = 2)
  graphics::arrows(.60, .665, .69, .81, length = .08, lwd = 2)
  graphics::arrows(.60, .665, .69, .51, length = .08, lwd = 2)
  graphics::arrows(.80, .75, .80, .28, length = .08, lwd = 2)
  graphics::arrows(.80, .45, .80, .28, length = .08, lwd = 2)
  graphics::title("Randomized comparative experiment")
  invisible(NULL)
}

plot_two_way_table_segmented_bar <- function() {
  df <- tibble::tibble(group = rep(c("Group A", "Group B"), each = 2), outcome = rep(c("Yes", "No"), 2), prop = c(.62, .38, .44, .56))
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = group, y = prop, fill = outcome)) +
        ggplot2::geom_col(color = "white") +
        ggplot2::labs(title = "Comparing conditional distributions", x = NULL, y = "Proportion", fill = "Outcome") +
        ggplot2::theme_minimal(base_size = 13)
    )
  }
  graphics::barplot(matrix(df$prop, nrow = 2), beside = FALSE, names.arg = c("Group A", "Group B"), col = c("#4f7cac", "#f2cc8f"), main = "Conditional distributions", ylab = "Proportion")
  graphics::legend("topright", c("Yes", "No"), fill = c("#4f7cac", "#f2cc8f"), bty = "n")
  invisible(NULL)
}

plot_regression_residual_plot <- function() {
  set.seed(11)
  x <- seq(1, 10, length.out = 25)
  y <- 2.5 + 1.1 * x + stats::rnorm(length(x), 0, 1.2)
  fit <- stats::lm(y ~ x)
  df <- tibble::tibble(x = x, y = y, fitted = stats::fitted(fit))
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(color = "#4f7cac", size = 2) +
        ggplot2::geom_line(ggplot2::aes(y = fitted), color = "#d1495b", linewidth = 1.1) +
        ggplot2::geom_segment(data = df[seq(1, nrow(df), by = 4), ], ggplot2::aes(xend = x, yend = fitted), linetype = "dashed") +
        ggplot2::labs(title = "Regression line with residuals", subtitle = "Vertical gaps are observed minus predicted values", x = "Explanatory variable", y = "Response variable") +
        ggplot2::theme_minimal(base_size = 13)
    )
  }
  graphics::plot(x, y, pch = 19, col = "#4f7cac", main = "Regression line with residuals", xlab = "Explanatory variable", ylab = "Response variable")
  graphics::abline(fit, col = "#d1495b", lwd = 2)
  invisible(NULL)
}

plot_comparing_groups_boxplots <- function() {
  set.seed(22)
  values <- data.frame(
    value = c(stats::rnorm(70, 68, 7), stats::rnorm(70, 74, 8), stats::rnorm(65, 70, 7), 104, 110),
    group = c(rep("Group 1", 70), rep("Group 2", 70), rep("Group 3", 67))
  )
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    return(
      ggplot2::ggplot(values, ggplot2::aes(x = group, y = value)) +
        ggplot2::geom_boxplot(fill = "#f2cc8f", color = "#3d405b", outlier.color = "#d1495b") +
        ggplot2::labs(title = "Comparing distributions with boxplots", subtitle = "Compare center, spread, and possible outliers", x = NULL, y = "Value") +
        ggplot2::theme_minimal(base_size = 13)
    )
  }
  graphics::boxplot(value ~ group, data = values, col = "#f2cc8f", main = "Comparing distributions", ylab = "Value")
  invisible(NULL)
}

visual_caption_for_type <- function(visual_type) {
  visual_type <- normalize_visual_type(visual_type)
  switch(
    visual_type,
    mean_vs_median_skew = "In a right-skewed distribution, a few extreme high values pull the mean to the right. The median stays closer to the typical case, which is why it is more resistant.",
    outlier_boxplot = "A boxplot makes outliers visible as points beyond the main spread. Those extreme values can strongly affect nonresistant summaries like the mean.",
    bar_vs_histogram = "A bar chart separates categories, while a histogram uses touching bins for a quantitative variable.",
    p_value_tail_area = "The shaded tail represents results as extreme as, or more extreme than, the observed statistic under the null model.",
    confidence_interval_number_line = "A confidence interval is estimate ± margin of error, shown as a plausible range for the population parameter.",
    standard_normal_curve = "A z-score marks how many standard deviations a value is from the mean on the standard normal curve.",
    scatterplot_association = "A scatterplot shows the direction, form, and strength of association between two quantitative variables.",
    regression_residual_plot = "Residuals are the vertical gaps between observed points and the fitted regression line.",
    bar_chart_categorical = "A bar chart compares counts across categories.",
    histogram_quantitative = "A histogram shows the distribution of a quantitative variable across adjacent intervals.",
    sampling_distribution_clt = "A larger sample size reduces standard error, so the sampling distribution is narrower.",
    binomial_distribution_bars = "A binomial distribution gives probabilities for counts of successes in a fixed number of independent trials.",
    experiment_randomization_diagram = "Random assignment helps create comparable treatment groups in an experiment.",
    two_way_table_segmented_bar = "Segmented bars compare conditional distributions across groups in a two-way table.",
    comparing_groups_boxplots = "Side-by-side boxplots compare center, spread, and possible outliers across groups.",
    "This visual is an illustrative aid for the current practice question."
  )
}

visual_response_for_type <- function(visual_type) {
  visual_type <- normalize_visual_type(visual_type)
  response <- switch(
    visual_type,
    mean_vs_median_skew = paste("### Visual aid", "", "I added a visual below. Notice how the high values create a long right tail.", "", "- The **mean** moves toward that tail.", "- The **median** stays closer to the main cluster.", "- That is why the median is usually more resistant for skewed data with outliers.", "", "**Think about this:** Based on the visual, which measure better represents a typical value: the mean or the median?", sep = "\n"),
    outlier_boxplot = paste("### Visual aid", "", "I added a visual below. Notice how the extreme points sit away from the main cluster.", "", "- Outliers can strongly affect nonresistant summaries.", "- Resistant summaries stay closer to the typical values.", "", "**Think about this:** Which summary would you trust more if you want a typical value?", sep = "\n"),
    bar_vs_histogram = paste("### Visual aid", "", "I added a visual below comparing a bar chart with a histogram.", "", "- A **bar chart** uses separated bars for categories.", "- A **histogram** uses touching bins for quantitative values.", "", "**Quick check:** Is the variable a category label or a measured number?", sep = "\n"),
    p_value_tail_area = paste("### Visual aid", "", "I added a visual below. The shaded tail shows the p-value idea.", "", "- Start by assuming the null hypothesis is true.", "- Then ask how far into the tail the observed statistic falls.", "- Smaller shaded tail areas give stronger evidence against the null.", "", "**Quick check:** Is the shaded area large enough to be common under the null, or small enough to be surprising?", sep = "\n"),
    confidence_interval_number_line = paste("### Visual aid", "", "I added a number-line view of a confidence interval.", "", "- The center is the sample estimate.", "- The endpoints are estimate ± margin of error.", "- The whole interval is a plausible range for the population parameter.", "", "**Quick check:** Are you describing the sample statistic or the unknown population parameter?", sep = "\n"),
    standard_normal_curve = paste("### Visual aid", "", "I added a normal-curve view.", "", "- The center is the mean.", "- A z-score marks distance from the mean in standard-deviation units.", "- Shaded regions represent probabilities.", "", "**Quick check:** Is the value above or below the mean?", sep = "\n"),
    scatterplot_association = paste("### Visual aid", "", "I added a scatterplot view of association.", "", "- Each point represents one case.", "- The overall pattern shows direction and strength.", "- A trend does not prove causation by itself.", "", "**Quick check:** Does the problem ask about association, prediction, or causation?", sep = "\n"),
    regression_residual_plot = paste("### Visual aid", "", "I added a regression visual. The dashed vertical gaps show residuals.", "", "- A **residual** is observed minus predicted.", "- Points far from the line have large residuals.", "- Regression describes association and prediction, not automatic causation.", sep = "\n"),
    binomial_distribution_bars = paste("### Visual aid", "", "I added a binomial distribution. Each bar is the probability of a possible count of successes.", "", "**Quick check:** What are the fixed number of trials and the definition of success?", sep = "\n"),
    experiment_randomization_diagram = paste("### Visual aid", "", "I added a simple experiment diagram. Random assignment sends subjects into treatment groups by chance.", "", "**Quick check:** Is the study selecting a sample, assigning treatments, or both?", sep = "\n"),
    two_way_table_segmented_bar = paste("### Visual aid", "", "I added segmented bars to compare conditional distributions.", "", "**Quick check:** Are you comparing counts, percents within groups, or overall totals?", sep = "\n"),
    comparing_groups_boxplots = paste("### Visual aid", "", "I added side-by-side boxplots. Compare medians, spreads, and possible outliers across groups.", sep = "\n"),
    sampling_distribution_clt = paste("### Visual aid", "", "I added a sampling-distribution view.", "", "- Larger samples make the sampling distribution narrower.", "- The center stays near the population parameter when the statistic is unbiased.", "- The spread is the standard error.", "", "**Quick check:** What happens to standard error when n increases?", sep = "\n"),
    paste("### Visual aid", "", "I added a visual below. Use the picture to connect the shape, area, or pattern back to the current question.", "", "**Quick check:** What part of the visual matches the wording of the question?", sep = "\n")
  )
  stringr::str_trim(response)
}

render_stat2331_visual <- function(visual_type) {
  visual_type <- normalize_visual_type(visual_type)
  plot_obj <- switch(
    visual_type,
    mean_vs_median_skew = plot_mean_vs_median_skew(),
    outlier_boxplot = plot_outlier_boxplot(),
    bar_vs_histogram = plot_bar_vs_histogram(),
    p_value_tail_area = plot_p_value_tail_area(),
    confidence_interval_number_line = plot_confidence_interval_number_line(),
    standard_normal_curve = plot_standard_normal_curve(),
    scatterplot_association = plot_scatterplot_association(),
    regression_residual_plot = plot_regression_residual_plot(),
    bar_chart_categorical = plot_bar_chart_categorical(),
    histogram_quantitative = plot_histogram_quantitative(),
    sampling_distribution_clt = plot_sampling_distribution_clt(),
    binomial_distribution_bars = plot_binomial_distribution_bars(),
    experiment_randomization_diagram = plot_experiment_randomization_diagram(),
    two_way_table_segmented_bar = plot_two_way_table_segmented_bar(),
    comparing_groups_boxplots = plot_comparing_groups_boxplots(),
    NULL
  )
  if (inherits(plot_obj, "ggplot")) print(plot_obj)
  invisible(plot_obj)
}
