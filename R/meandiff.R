#' The cumulative distribution function for the CI width for a mean
#'
#' Calculates the probability P(W_n <= width), where W_n is the width
#' of the confidence interval for a mean difference based on the
#' t-distribution assuming equal variances in the two groups, i.e.
#' what t.test(var.equal = TRUE) provides.
#'
#' This assumes the observations in each group are i.i.d. normally distributed
#' with a common standard deviation sigma which is treated as unknown to us
#' and must be estimated.
#'
#' @param sigma Common standard deviation.
#' @param width Width of confidence interval.
#' @param n1 Number of observations in group 1.
#' @param n2 Number of observations in group 2.
#' @param conf.level Confidence level of the interval.
#'
#' @returns A numeric value.
#'
#' @noRd
#' @keywords internal
ci_width_md_cdf_equal <- function(sigma, width, n1, n2, conf.level = 0.95) {
  alpha <- 1 - conf.level
  df <- n1 + n2 - 2
  val <- (df*width^2) / (4*sigma^2*stats::qt(1 - alpha/2, df)^2*(1/n1 + 1/n2))
  return(stats::pchisq(val, df))
}

#' Calculates the probability, width or sample size given the two others
#' for confidence interval of a mean difference assuming a common standard
#' deviation.
#'
#' This assumes the observations in each group are i.i.d. normally distributed
#' with a common standard deviation sigma which is treated as unknown to us
#' and must be estimated.
#'
#' @param sigma Common standard deviation.
#' @param width Width of confidence interval.
#' @param n1 Number of observations in group 1.
#' @param prob Probability of obtaining given width.
#' @param nratio Ratio of group sizes (n2/n1).
#' @param conf.level Confidence level of the interval.
#'
#' @returns A data.frame.
#'
#' @noRd
#' @keywords internal
ci_width_md_equal_simple <- function(sigma, width = NULL, n1 = NULL, prob = NULL, nratio = 1, conf.level = 0.95) {
  if (is.null(prob) & !is.null(n1) & !is.null(width)) {
    prob <- ci_width_md_cdf_equal(sigma = sigma, width = width, n1 = n1, n2 = nratio*n1, conf.level = conf.level)
  }
  else if (is.null(width) & !is.null(prob) & !is.null(n1)) {
    width <- stats::uniroot(\(w) ci_width_md_cdf_equal(sigma = sigma, width = w, n1 = n1, n2 = nratio*n1, conf.level = conf.level) - prob, c(0, 1e+07))$root
  }
  else if (is.null(n1) & !is.null(width) & !is.null(prob)) {
    n1 <- stats::uniroot(\(n1) ci_width_md_cdf_equal(sigma = sigma, width = width, n1 = n1, n2 = nratio*n1, conf.level = conf.level) - prob, c(2, 1e+07))$root
  }

  dplyr::tibble(
    sigma = sigma,
    width = width,
    prob = prob,
    n1 = n1,
    n2 = nratio*n1,
    nratio = nratio,
    conf.level = conf.level
  )
}



#' Calculates the probability, width or sample size given the two others
#' for confidence interval of a mean difference.
#'
#' One can provide several values of arguments, and the function returns
#' the results as a data.frame or a `ggplot2` plot.
#'
#' This assumes the observations in each group are i.i.d. normally distributed
#' with a common standard deviation sigma which is treated as unknown to us
#' and must be estimated.
#'
#' @param sigma Common standard deviation.
#' @param width Width of confidence interval.
#' @param n1 Number of observations in group 1.
#' @param prob Probability of obtaining given width.
#' @param nratio Ratio of group sizes (n2/n1).
#' @param conf.level Confidence level of the interval.
#' @param plot `TRUE` returns a `ggplot2` plot. `FALSE` returns a data.frame.
#' @param plot_order Names of the variables to display in the plot.
#' + A vector with names of up to four arguments (sigma, width, n, prob, conf.level)
#' + The order of the arguments determines the appearance of the plot:
#'    - 1: y-axis, 2: x-axis, 3: color, 4: facet.
#' @param connect Connects the observations in the plot.
#'
#' @returns A `ggplot2` object or data.frame (if `plot = FALSE`).
#'
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' # Calculate the sample sizes needed to obtain a 95% confidence interval for
#' # a mean difference of width 1, 3 and 5 with a probability of either 80%
#' # or 90% and a standard deviation of 10 or 15 in both groups:
#' ci_width_md_equal(sigma = c(10, 15), width = c(1, 3, 5), prob = c(0.80, 0.90))
#' # Make a plot of the sample size as a function of width, probability and
#' # standard deviation:
#' ci_width_md_equal(sigma = c(10, 15), width = c(1, 3, 5), prob = c(0.80, 0.90), plot = TRUE)
ci_width_md_equal <- function(sigma, width = NULL, n1 = NULL, prob = NULL, nratio = 1, conf.level = 0.95, plot = FALSE, plot_order = c("n1", "width", "prob", "sigma", "nratio"), connect = TRUE) {
  # Check for valid arguments.
  if (!valid_range(sigma, lower = 0)) {
    stop("'sigma' must contain values > 0.")
  }
  if (!valid_range(nratio, lower = 0)) {
    stop("'nratio' must contain values > 0.")
  }
  if (!is.null(width) & !valid_range(width, lower = 0)) {
    stop("'width' must contain values > 0.")
  }
  if (!is.null(n1) & !valid_range(n1, lower = 1)) {
    stop("'n1' must contain values > 1.")
  }
  if (!is.null(prob) & !valid_range(prob, lower = 0, upper = 1)) {
    stop("'prob' must contain values within (0,1).")
  }
  n_null <- is.null(width) + is.null(n1) + is.null(prob)
  if (n_null != 1) {
    stop("Exactly two of the arguments 'n', 'width, and 'prob' must be specified.")
  }

  # Make a data frame for all combinations of the provided arguments.
  pars <- list(sigma = sigma, width = width, n1 = n1, prob = prob, nratio = nratio, conf.level = conf.level)
  pars <- pars[!sapply(pars, is.null)]
  df <- do.call(expand.grid, pars)

  # Calculate the missing argument and collect the results as a data frame.
  df <- purrr::pmap(df, ci_width_md_equal_simple)
  df <- do.call(rbind, df)
  df <- dplyr::mutate(df, n = n1 + n2)

  # Return data frame.
  if (!plot) {
    return(df)
  }

  # Else make a plot.

  # We extract all the arguments that have more than one value.
  plot_vars <- df |>
    dplyr::select(dplyr::where(~ dplyr::n_distinct(.x) > 1)) |>
    names()

  plot_order <- plot_order[plot_order %in% plot_vars]

  if (length(plot_order) > 4) {
    stop("'plot_order' must contain at most four arguments.")
  }

  if (length(plot_order) <= 2) {
    fig <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(y = !!rlang::sym(plot_order[1]), x = !!rlang::sym(plot_order[2])))
  } else {
    fig <- df |>
      dplyr::mutate(!!rlang::sym(plot_order[3]) := factor(!!rlang::sym(plot_order[3]))) |>
      ggplot2::ggplot(mapping = ggplot2::aes(y = !!rlang::sym(plot_order[1]), x = !!rlang::sym(plot_order[2]), color = !!rlang::sym(plot_order[3])))
  }

  fig <- fig + ggplot2::geom_point()
  if (length(plot_order) == 4) {
    fig <- fig + ggplot2::facet_wrap(paste("~", plot_order[4]))
  }

  if (connect) {
    fig <- fig + ggplot2::geom_line()
  }

  fig <- fig + ggplot2::theme_minimal()
  return(fig)
}
