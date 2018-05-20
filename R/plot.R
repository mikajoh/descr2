#' Plot both pdfs and pngs in one go.
#' @importFrom ggplot2 ggsave
#' @importFrom here here
#'
#' @param plot A ggplot2 plot object.
#' @param filename filename (without extenstion!)
#' @param ... Other arguments passed to \code{ggsave()}.
#' 
#' @export
ggsave2 <- function(plot, filename, ...) {

  if (!dir.exists(here("output", "figs"))) {
    dir.create(here("output", "figs"))
  }

  if (!dir.exists(here("output", "figs", "png"))) {
    dir.create(here("output", "figs", "png"))
  }


  ggsave(
    plot = plot,
    filename = paste0(
      here("output", "figs"),
      "/", filename, ".pdf"
    ),
    ...
  )
  
  ggsave(
    plot = plot,
    filename = paste0(
      here("output", "figs", "png"),
      "/", filename, ".png"
    ),
    ...
  )
  
  invisible(plot)
}

#' Project theme for ggplot2.
#' 
#' @importFrom ggplot2 theme
#'
#' @param ... Other arguments passed to \code{theme()}.
#' 
#' @export
theme_m <- function(...) {
  theme(
    text = element_text(size = 12,
                        colour = "black"),
    axis.text = element_text(size = 10,
                             colour = "black"),
    axis.title = element_text(size = 11,
                              colour = "black"),
    axis.line = element_line(),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(1, "mm"),
    plot.margin = margin(1, 5, 1, 1, "mm"),
    panel.spacing.x = unit(7.5, "mm"),
    panel.spacing.y = unit(2.5, "mm"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10,
                              face = "bold"),
    ## strip.text.y = element_text(angle = 0, face = "bold.italic"),
    strip.text.y = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.key.height = unit(4, "mm"),
    legend.title.align = .125,
    legend.margin = margin(),
    ...
  )
}

#' Plot AMCEs for closest and prefered candidates separately for
#' difference subgroups.
#' 
#' @importFrom ggplot2 aes aes_string ggplot facet_grid geom_point geom_vline
#'   scale_x_continuous scale_y_discrete labs theme element_blank
#'   element_line element_text margin scale_shape_manual unit
#' @importFrom ggstance geom_errorbarh position_dodgev
#'
#' @param res results from \code{amce()} passed through
#'   \code{add_labels()}.
#' @param by Variable to facet by.
#' 
#' @export
plot_sub_by <- function(res, by) {
  res$lwr <- res$estimate - (1.96 * res$std_error)
  res$upr <- res$estimate + (1.96 * res$std_error)  
  ggplot(
    data = res,
    aes_string(
      x = "estimate", y = "value",
      shape = "exp_treat",
      xmin = "lwr",
      xmax = "upr")) +
    facet_grid(
      paste0("treatment ~ ", by),
      scales = "free_y",
      space = "free_y") +
    ggstance::geom_errorbarh(
      width = 0,
      position = position_dodgev(.75)) +
    geom_point(position = position_dodgev(.75)) +
    geom_vline(aes(xintercept = 0), linetype = "dotted")  +
    scale_shape_manual(
      values = c(19, 1)) +
    scale_x_continuous(
      limits = c(-.4, .4),
      breaks = round(seq(-.4, .4, .2), 2),
      expand = c(0, 0),
      labels = function(x) x * 100) +
    scale_y_discrete(
      labels = function(x) parse(text = as.character(x))) +
    labs(
      x = "Marginal effect, choosing candidate (%)",
      y = "Candidate attributes",
      shape = "Decision type:"
    ) +
    theme_m()  
}

#' Plot difference between closest and prefered candidates for
#' difference subgroups.
#' 
#' @importFrom ggplot2 aes aes_string ggplot facet_grid geom_point geom_vline
#'   scale_x_continuous scale_y_discrete labs theme element_blank
#'   element_line element_text margin scale_shape_manual unit
#' @importFrom ggstance geom_errorbarh position_dodgev
#'
#' @param res results from \code{amce()} passed through
#'   \code{add_labels()}.
#' @param by Variable to facet by.
#' 
#' @export
plot_diff_by <- function(res, by) {
  res$lwr <- res$estimate - (1.96 * res$std_error)
  res$upr <- res$estimate + (1.96 * res$std_error)
  ggplot(
    data = res,
    aes_string(
      x = "estimate", y = "value",
      xmin = "lwr",
      xmax = "upr")) +
    facet_grid(
      paste0("treatment ~ ", by),
      scales = "free_y",
      space = "free_y") +
    ggstance::geom_errorbarh(
      width = 0,
      position = position_dodgev(.75)) +
    geom_point(position = position_dodgev(.75)) +
    geom_vline(aes(xintercept = 0), linetype = "dotted") +
    scale_x_continuous(
      limits = c(-.2, .2),
      breaks = round(seq(-.2, .2, .1), 2),
      expand = c(0, 0),
      labels = function(x) x * 100) +
    scale_y_discrete(
      labels = function(x) parse(text = as.character(x))) +
    labs(
      x = paste0(
        "Difference in Marginal Effect, choosing candidate (%)\n",
        "(Closest - Prefered)"),
      y = "Candidate attributes"
    ) +
    theme_m()
}
