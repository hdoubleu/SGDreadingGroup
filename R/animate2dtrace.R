library(ggplot2)
library(animation)

#' Creates a 2 dimension trace plot with ggplot2::geom_path and animate it
#'
#' @param data \code{data.frame} with 2 columns corresponding to x and y values
#' @param gif_loc Character vector indicating the location to save the
#'        animation to
#'
#' @return none
#'
#' @export
animate_trace_plot <- function(data = NULL, gif_loc = NULL) {

  plot_xlim <- range(data[, 1]) + c(-0.5, 0.5)
  plot_ylim <- range(data[, 2]) + c(-0.5, 0.5)

  animation_pause_interval <- 0.3
  data_colnames <- colnames(data)

  animation::saveGIF({
    ani.options(interval = animation_pause_interval)
    for (i in 1:nrow(data)) {
      plot <- ggplot(data = data[c(1:i),],
                     aes_string(x = data_colnames[1], y = data_colnames[2])) +
        xlab(data_colnames[1]) + ylab(data_colnames[2]) +
        xlim(plot_xlim) + ylim(plot_ylim) +
        geom_path() +
        theme_bw()
      print(plot)
      ani.pause()
    }
  })
}
