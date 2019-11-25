
#' Function for visualizing prior and posterior probabilities of the hypotheses
#' as a probability wheel.
#' @title Plot Probability Wheel
#' @param x object of class \code{"ab"}.
#' @param type character indicating whether to plot a probability wheel
#'   visualizing the prior probabilities of the hypotheses (i.e., \code{type =
#'   "prior"}) or the posterior probabilities of the hypotheses (i.e.,
#'   \code{type = "posterior"}). The default is \code{"posterior"}.
#' @param useGGplot2 Logical, indicating whether \code{"ggplot2"} should be used to create the plot.
#'
#' @author Quentin F. Gronau
#' @example examples/example.prob_wheel.R
#' @importFrom plotrix floating.pie
#' @importFrom graphics legend par
#' @export
prob_wheel <- function(x, type = "posterior", useGGplot2 = FALSE) {

  ### argument checking ###

  # check x
  if ( ! inherits(x, "ab")) {
    stop('x needs to be of class "ab"',
         call. = FALSE)
  }

  # check type
  if ( ! (type %in% c("posterior", "prior")) || length(type) > 1) {
    stop('type needs to be of either "posterior" or "prior"',
         call. = FALSE)
  }

  if (useGGplot2 && !requireNamespace("ggplot2", quietly = TRUE))
    stop('if useGGplot2 = TRUE then ggplot2 must be installed.',
         call. = FALSE)

  if (type == "posterior") {
    p <- x$post_prob
  } else if (type == "prior") {
    p <- x$input$prior_prob
  }


  # make sure that p has correct order
  p <- p[c("H-", "H0", "H+", "H1")]

  radius <- 0.2
  index <- p != 0
  p[p == 0] <- 1e-100 # to avoid not plotting the correct colors for extreme cases

  #col <- c("firebrick", "grey70", "dodgerblue", "darkgoldenrod1")
  col <- c(RColorBrewer::brewer.pal(8, name = "Dark2")[2],
           RColorBrewer::brewer.pal(8, name = "Dark2")[8],
           RColorBrewer::brewer.pal(8, name = "Dark2")[1],
           RColorBrewer::brewer.pal(8, name = "Dark2")[6])
  names(col) <- c("H-", "H0", "H+", "H1")

  legend_order <- c("H1", "H+", "H-", "H0")
  hypnice <- names(p)
  hypnice[hypnice == "H-"] <- "H\u2212" # to ensure equal spacing
  if (type == "prior") {

    legend_lab <- paste0("P(", hypnice, ") = ",
                         sprintf("%.3f", round(p, 3)))

  } else if (type == "posterior") {

    legend_lab <- paste0("P(", hypnice, " | data) = ",
                         sprintf("%.3f", round(p, 3)))

  }
  names(legend_lab) <- names(p)
  legend_col <- col[legend_order]
  legend_index <- index[legend_order]

  if (useGGplot2) {

    breaks <- names(legend_index[legend_index])
    breaks <- breaks[match(names(p), breaks, nomatch = 0L)]
    df <- data.frame(
      value = p[breaks],
      group = factor(seq_along(breaks), labels = names(p[breaks]))
    )

    # TODO: spacing legend elements, legend color order, legend label order!

    stroke <- 0.75
    col <- col[breaks]
    legend_lab <- legend_lab[legend_order][legend_index]#legend_lab[breaks]
    gl <- ggplot2::guide_legend(override.aes = list(size = 6, shape = 21, stroke = stroke,
                                                    fill = col[sort(names(col))]))

    # to please R CMD check
    value <- group <- NULL
    # g <-
      ggplot2::ggplot(df, ggplot2::aes(x = "", y = value, fill = group, color = group)) +
      ggplot2::geom_bar(width = 1, size = stroke, stat = "identity", color = "black", key_glyph = "point") +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::scale_fill_manual(name = NULL, breaks = breaks, values = col, labels = legend_lab, guide = gl) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.margin   = ggplot2::margin(10, 100, 10, 50),
        legend.margin = ggplot2::margin(1, 20, 1, 1),
        legend.text   = ggplot2::element_text(size = 16)
      )

    return(g)


  } else {
    op <- par(xpd = TRUE, mar = rep(0.3, 4))
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
         xlim = c(0.5, 1.5), ylim = c(0.5, 1.5), asp = 1 / 10)

    floating.pie(0.8, 1, p,
                 radius = radius, col = col,
                 lwd = 2, startpos = pi/2)

    legend(1.04, 1, legend = legend_lab[legend_order][legend_index],
           pt.bg = legend_col[legend_index],
           pt.cex = rep(1.7, sum(legend_index)),
           pt.lwd = rep(2, sum(legend_index)),
           col = rep("black", sum(legend_index)),
           pch = rep(21, sum(legend_index)),
           bty = "n", cex = 1.5, xjust = 0,
           yjust = 0.5)
    par(op)
  }
}
