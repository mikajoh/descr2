#' Estimator for the average causal mediation effect (AMCE)
#'
#' Estimates the average causal mediation effect (AMCE) for all
#' provided treatments. Can estimate clustered and/or robust standard
#' errors.
#'
#' @importFrom stats as.formula lm na.omit
#' @importFrom broom tidy
#' @importFrom sandwich estfun sandwich
#' @importFrom lmtest coeftest
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos is_empty enexpr quo_name
#' 
#'
#' @param data Data
#' @param post Post treatment variable.
#' @param ... The treatment components (without quotes).
#' @param cluster Variable indicating clusters if SEs should be
#'   clustered.
#' @param subgroup Variable(s) indicating which subgroups, if any, to
#'   independently calculate AMCEs for. If multiple subgroups are
#'   provided, then it will iterate over each combination.
#' @param diff If \code{diff} is provided, it will calculate the
#'   difference in AMCE between the unique values of \code{diff}.
#' @param subset Subset to estimate AMCEs for.
#' @keywords AMCE
#' @examples
#'\dontrun{
#'  amce <- amce(data, post, treatment_1, treatment_2)
#' }
#' @export
amce <- function(data, post, ...,
                 cluster = NULL,
                 subgroup = NULL,
                 diff = NULL,
                 subset = NULL) {

  if (!is.null(subset))
    data <- data[subset, ]

  have_diff <- !is.null(diff)
  have_subgroup <- !is.null(subgroup)
  have_cluster <- !is.null(cluster)
  
  post <- quo_name(enexpr(post))
  
  quos <- quos(...)
  if (is_empty(quos)) {
    treat <- names(data)
  } else {
    treat <- unname(tidyselect::vars_select(names(data), !!! quos))
  }
  treat <- setdiff(treat, c(post, subgroup, diff, cluster))
  
  ## Turn all vars into factors.
  for (var in c(treat, subgroup, diff)) {
    if (!is.factor(data[[var]])) {
      data[[var]] <- factor(data[[var]])
    }
  }

  ## Make compute list to lapply over.
  if (have_subgroup) {
    subgroup_values <- lapply(subgroup, function(x) levels(factor(data[[x]])))
    subgroup_values <- c(subgroup_values, list(treat))
    names(subgroup_values) <- c(subgroup, "treat")

    if (have_diff) {
      subgroup_values <- c(subgroup_values, list(diff))
      names(subgroup_values) <- c(subgroup, "treat", "diff")
    }

    grid <- expand.grid(subgroup_values, stringsAsFactors = FALSE)

  } else if (have_diff) {

    grid <- expand.grid(
      treat = treat,
      diff = diff,
      stringsAsFactors = FALSE
    )

  } else {

    grid <- data.frame(
      treat = treat,
      stringsAsFactors = FALSE
    )

  }

  res <- lapply(1:nrow(grid), function(i) {

    .treat <- grid$treat[i]
    .diff <- grid$diff[i]
    .data <- data

    ## Filter subgroup
    if (have_subgroup) {
      for (.sub in subgroup) {
        .data <- .data[.data[[.sub]] == grid[[.sub]][i], ]
        if (nrow(.data) == 0) {
          warn_msg <- c(
            "No rows left in subgroup when:\n",
            lapply(subgroup, function(x)
              paste0(x, " == ", gsub("\n", " ", grid[[x]][i]), "\n"))
          )
          warning(warn_msg)
          return(NULL)
        }
      }
    }


    if (have_diff) {
      formula <- paste0(post, " ~ ",
                        .treat, " + ",
                        .diff, " + ",
                        .treat, ":", .diff)
    } else {
      formula <- paste0(post, " ~ ", .treat)
    }

    ## Estimator that can return cluster-robust se.
    fit <- estimator_regression(
      formula = formula,
      data = .data,
      cluster = cluster
    )

    ## Baseline for reference category
    baseline <- data.frame(
      term = levels(.data[[.treat]])[1],
      estimate = 0,
      std.error = 0,
      statistic = NA,
      p.value = NA
    )

    if (have_diff) {
      baseline <- cbind(
        baseline,
        data.frame(
          diff_variable = .diff,
          diff_value = levels(.data[[.diff]])[-1])
      )
    }

    ## Tidy estimate to return
    est <- fit

    if (have_diff) {
      est <- est[grepl("\\:", est$term), ]
      est$diff_variable <- .diff
      est$diff_value <- gsub("^.*\\:(.*)$", "\\1", est$term)
      est$diff_value <- gsub(paste0("^", .diff), "", est$diff_value)
    } else {
      est <- est[est$term != "(Intercept)", ]
    }

    est$term <- gsub("^(.*)\\:.*$", "\\1", est$term)
    est$term <- gsub(paste0("^", .treat), "", est$term)

    est <- rbind(baseline, est)
    est$treatment <- .treat
    names(est)[1] <- "value"

    if (have_subgroup) {
      for (.sub in subgroup) {
        est[[.sub]] <- grid[[.sub]][i]
      }
    }

    est
  })
  res <- do.call("rbind", res)

  res$treatment <- factor(res$treatment, levels = unique(res$treatment))
  res$value <- factor(res$value, levels = unique(res$value))
  res$value_order <- 1:nrow(res)
  names(res)[names(res) == "std.error"] <- "std_error"
  names(res)[names(res) == "p.value"] <- "p_value"

  return(res)
}


estimator_regression <- function(formula, data,
                                 cluster = NULL,
                                 weights = NULL) {

  if (is.character(formula)) formula <- as.formula(formula)

  all_vars <- all.vars(formula)
  if (!is.null(cluster)) all_vars <- c(all_vars, cluster)
  if (!is.null(weights)) all_vars <- c(all_vars, weights)
  data <- data[, all_vars]
  data <- na.omit(data)

  if (!is.null(weights)) {
    we <- data[[weights]]
  } else {
    we <- NULL
  }

  fit <- lm(
    formula = formula,
    data = data,
    weights = we
  )

  if (!is.null(cluster)) {

    cl <- data[[cluster]]
    M <- length(unique(cl))
    N <- length(cl)

    dfc <- (M / (M - 1)) * ((N - 1) / (N - fit$rank))
    u <- apply(estfun(fit), 2, function(x) tapply(x, cl, sum))
    vcov_cl <- dfc * sandwich(fit, meat. = crossprod(u) / N)

    out <- coeftest(fit, vcov_cl)
    out <- tidy(out)

  } else {

    out <- tidy(fit)

  }

  out
}
