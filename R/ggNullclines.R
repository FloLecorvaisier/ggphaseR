#### ggNullclines ####

ggNullclines <- function (deriv, xlim, ylim, parameters = NULL, system = "two.dim",
                          points = 101, col = c("blue", "cyan"), add = TRUE, add.legend = TRUE,
                          state.names = if (system == "two.dim") c("x", "y") else "y",
                          xlab = if (system == "two.dim") state.names[1] else "t",
                          ylab = if (system == "two.dim") state.names[2] else state.names[1])
{
  if (any(!is.vector(xlim), length(xlim) != 2)) {
    stop("xlim is not a vector of length 2, as is required")
  }
  if (xlim[2] <= xlim[1]) {
    stop("xlim[2] is less than or equal to xlim[1]")
  }
  if (any(!is.vector(ylim), length(ylim) != 2)) {
    stop("ylim is not a vector of length 2, as is required")
  }
  if (ylim[2] <= ylim[1]) {
    stop("ylim[2] is less than or equal to ylim[1]")
  }
  if (points <= 0) {
    stop("points is less than or equal to zero")
  }
  if (!(system %in% c("one.dim", "two.dim"))) {
    stop("system must be set to either \"one.dim\" or \"two.dim\"")
  }
  if (!is.vector(col)) {
    stop("col is not a vector as required")
  }
  if (length(col) != 2) {
    if (length(col) == 1) {
      col <- rep(col, 2)
    }
    else if (length(col) > 2) {
      col <- col[1:2]
    }
    message("Note: col has been reset as required")
  }
  if (!is.logical(add)) {
    stop("add must be logical")
  }
  if (!is.logical(add.legend)) {
    stop("add.legend must be logical")
  }
  x <- seq(xlim[1], xlim[2], length.out = points)
  y <- seq(ylim[1], ylim[2], length.out = points)
  dx <- dy <- matrix(0, ncol = points, nrow = points)
  if (!add) {
    p = ggplot()
  }
  else p = last_plot()
  if (system == "one.dim") {
    for (i in 1:points) {
      dy[1, i] <- deriv(0, stats::setNames(c(y[i]), state.names),
                        parameters)[[1]]
    }
    for (i in 2:points) {
      dy[i, ] <- dy[1, ]
    }
    x_ = rep(x, times = length(x))
    y_ = rep(y, each = length(y))
    p <- last_plot() +
      geom_contour(aes(x = x_, y = y_, z = dy), breaks = 0, col = col[1]) +
      labs(x = xlab, y = ylab)
    if (add.legend) {
      p <- p +
        geom_line(aes(x = c(mean(xlim)),
                      y = c(mean(ylim)),
                      color = paste0("d", state.names, "/dt = 0 for all t"))) +
        scale_color_manual(values = col[1]) +
        labs(x = xlab, y = ylab, color = element_blank())
    }
    suppressMessages(print(p))
    return(list(add = add, add.legend = add.legend, col = col,
                deriv = deriv, dy = dy, parameters = parameters,
                points = points, system = system, x = x, xlim = xlim,
                y = y, ylim = ylim))
  }
  else {
    for (i in 1:points) {
      for (j in 1:points) {
        df <- deriv(0, stats::setNames(c(x[i], y[j]),
                                       state.names), parameters)
        dx[i, j] <- df[[1]][1]
        dy[i, j] <- df[[1]][2]
      }
    }
    x_ = rep(x, times = length(x))
    y_ = rep(y, each = length(y))
    p <- last_plot() +
      geom_contour(aes(x = x_, y = y_, z = dx), breaks = 0, col = col[1]) +
      geom_contour(aes(x = x_, y = y_, z = dy), breaks = 0, col = col[2]) +
      labs(x = xlab, y = ylab)
    if (add.legend) {
      p <- p +
        geom_line(aes(x = c(mean(xlim), mean(xlim)),
                      y = c(mean(ylim), mean(ylim)),
                      color = c("x nullclines", "y nullclines"))) +
        scale_color_manual(values = col) +
        labs(x = xlab, y = ylab, color = element_blank())
    }
    print(p)
    return(list(add = add, add.legend = add.legend, col = col,
                deriv = deriv, dx = dx, dy = dy, parameters = parameters,
                points = points, system = system, x = x, xlab = xlab, xlim = xlim,
                y = y, ylab = ylab, ylim = ylim))
  }
}
