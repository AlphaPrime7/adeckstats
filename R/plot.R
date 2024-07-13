## adeckstats - R package for Random Numbers Distribution Simulation
## Copyright (C) 2024 Tingwei Adeck

#' Adeckstats Plots
#' @family plots
#' @param x plot requirement
#' @param which_sim sim number
#' @param whichplots int
#' @param histogram boolean
#' @param density boolean
#' @param xlab xlab
#' @param ylab ylab
#' @param ... dots
#' @return plot object
#' @name plots
NULL

#' @rdname plots
#' @return plot object
#' @export
plot.adeck_normsim <- function(x,
                               which_sim = 1,
                               whichplots = 1,
                               histogram = 1 %in% whichplots,
                               density = 2 %in% whichplots, ...){

  def.par <- par(ask = (as.numeric(histogram) + as.numeric(density) > 1), "mar") #dev.off()

  adeck_normsim.obj <- x

  if (!inherits(adeck_normsim.obj, "adeck_normsim"))
    stop("Use only with \"adeck_normsim\" objects!")

  if (which_sim == "all"){
    data = combine_tibbles(adeck_normsim.obj)
  }
  else {
    data = adeck_normsim.obj[[which_sim]]
  }
  data

  if (histogram) {
    if (which_sim != "all"){
      plot.adeck_norm_tbl(data)
    }
    else if (which_sim == "all"){
      suppressWarnings({
        h <- ggplot2::ggplot(data, ggplot2::aes(x=y, fill= sim_number)) +
          ggplot2::geom_histogram(color='#e9ecef', alpha=0.6, position='identity') +
          ggplot2::stat_bin(bins = 30)
        h <- plotly::ggplotly(h)
        h
      })
    }
  }
  else {
    print('Waiting on you to learn')
  }
}

#' @rdname plots
#' @return plot object
#' @export
plot.adeck_norm_tbl <- function(x,
                                whichplots = 1,
                                histogram = 1 %in% whichplots,
                                density = 2 %in% whichplots,
                                xlab = NULL,
                                ylab = NULL, ...){

  ntbl <- x
  if (!inherits(ntbl, "adeck_norm_tbl"))
    stop("Use only with \"adeck_norm_tbl\" objects!")

  if (histogram) {
    xlab = "Spatial Step Size"
    ylab = "Frequency or Density"
    main = "Histogram with Density Plot of Simulation(s)"
    h = hist(ntbl$y,
             main = main,
             xlab = xlab,
             ylab = ylab,
             xlim = c(min(ntbl$y) - 1, max(ntbl$y) + 1),
             ylim = c(0,1),
             col = "darkmagenta",
             freq = F, ...)
    text(h$mids, h$density, labels = h$density, adj = c(0.5, -0.5))
    lines(stats::density(ntbl$y),
          lwd = 2,
          col = "green")
  }
}
