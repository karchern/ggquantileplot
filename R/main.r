GeomQuantile <- ggproto("GeomCrossbar", Geom,
  setup_params = function(data, params) {
    GeomErrorbar$setup_params(data, params)
  },
  extra_params = c("na.rm", "orientation"),
  setup_data = function(data, params) {
    GeomErrorbar$setup_data(data, params)
  },
  default_aes = aes(
    colour = "black", fill = NA, linewidth = 0.5, linetype = 1,
    alpha = NA
  ),
  required_aes = c("x", "y", "ymin|xmin", "ymax|xmax"),
  draw_key = draw_key_crossbar,
  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", fatten = 2.5, width = NULL,
                        flipped_aes = FALSE) {
    data <- check_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)


    middle <- transform(data, x = xmin, xend = xmax, yend = y, linewidth = linewidth * fatten, alpha = NA)


    # No notch
    box <- data_frame0(
      x = c(data$xmin, data$xmin, data$xmax, data$xmax, data$xmin),
      y = c(data$ymax, data$ymin, data$ymin, data$ymax, data$ymax),
      alpha = rep(data$alpha, 5),
      colour = rep(data$colour, 5),
      linewidth = rep(data$linewidth, 5),
      linetype = rep(data$linetype, 5),
      fill = rep(data$fill, 5),
      group = rep(seq_len(nrow(data)), 5) # each bar forms it's own group
    )

    box <- flip_data(box, flipped_aes)
    middle <- flip_data(middle, flipped_aes)

    # browser()
    ggname("geom_crossbar", gTree(children = gList(
      GeomPolygon$draw_panel(box, panel_params, coord, lineend = lineend, linejoin = linejoin),
      GeomSegment$draw_panel(middle, panel_params, coord, lineend = lineend, linejoin = linejoin)
    )))
    # browser()
  },
  rename_size = TRUE
)

draw_key_quantileplot <- function(data, params, size) {
  gp <- gpar(col = data$colour %||% "grey20", fill = alpha(data$fill %||%
    "white", data$alpha), lwd = (data$linewidth %||% 0.5) *
    .pt, lty = data$linetype %||% 1, lineend = params$lineend %||%
    "butt", linejoin = params$linejoin %||% "mitre")
  if (isTRUE(params$flipped_aes)) {
    grobTree(rectGrob(height = 0.75, width = 0.5), linesGrob(
      0.5,
      c(0.125, 0.875)
    ), gp = gp)
  } else {
    grobTree(rectGrob(height = 0.5, width = 0.75), gp = gp)
  }
}

#' Plot a quantile plot
#'
#'  Quantile plots are an alternative to box plots / histograms / violinplots as in that they visualize the distribution of data points.
#' I often found myself wanting a more 'robust' alternative to box plots when working with zero-inflated data (often found in -omics data).
#' For very sparse data, boxplots are useless as the median (and often times even upper quartile) is equal to 0/detection limit, so all
#' the user can see is are the outliers, which is not helpful in judging the distribution. Quantile plots are a useful alternative since they show the cutoffs of a range of quantiles.
#'
#' @export
geom_quantileplot <- function(mapping = NULL, data = NULL,
                              stat = StatQuantileplot, position = "dodge2",
                              ...,
                              outlier.colour = NULL,
                              outlier.color = NULL,
                              outlier.fill = NULL,
                              outlier.shape = 19,
                              outlier.size = 1.5,
                              outlier.stroke = 0.5,
                              outlier.alpha = NULL,
                              quantilesP = NULL,
                              notch = FALSE,
                              notchwidth = 0.5,
                              varwidth = FALSE,
                              na.rm = FALSE,
                              orientation = NA,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  # varwidth = TRUE is not compatible with preserve = "total"
  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      cli::cli_warn("Can't preserve total widths when {.code varwidth = TRUE}.")
      position$preserve <- "single"
    }
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomQuantileplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      orientation = orientation,
      quantilesP = quantilesP,
      ...
    )
  )
}

#' @export
StatQuantileplot <- ggproto("StatBoxplot", Stat,
  required_aes = c("y|x", "fill"),
  non_missing_aes = "weight",
  # either the x or y aesthetic will get dropped during
  # statistical transformation, depending on the orientation
  dropped_aes = c("x", "y", "weight"),
  setup_data = function(self, data, params) {
    data <- flip_data(data, params$flipped_aes)
    data$x <- data$x %||% 0
    data <- remove_missing(
      data,
      na.rm = params$na.rm,
      vars = "x",
      name = "stat_boxplot"
    )
    data <- convert_to_quantile_plot_factors(data, "fill", quantilesP = params$quantilesP)
    flip_data(data, params$flipped_aes)
  },
  setup_params = function(self, data, params) {
    # data <- convert_to_quantile_plot_factors(data, "supp", numExtensions = length(params$quantilesP) - 1)
    # Let default fill take over.

    params$flipped_aes <- has_flipped_aes(data, params,
      main_is_orthogonal = TRUE,
      group_has_equal = TRUE,
      main_is_optional = TRUE
    )
    data <- flip_data(data, params$flipped_aes)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }

    params$width <- params$width %||% (resolution(data$x %||% 0) * 0.75)

    if (!is_mapped_discrete(data$x) && is.double(data$x) && !has_groups(data) && any(data$x != data$x[1L])) {
      cli::cli_warn(c(
        "Continuous {.field {flipped_names(params$flipped_aes)$x}} aesthetic",
        "i" = "did you forget {.code aes(group = ...)}?"
      ))
    }

    if (is.null(params$quantilesP)) {
      cli::cli_abort(c(
        "quantilesP not supplied. Please supply a vector of quantiles. First entry needs to be 0.5."
      ))
    }

    if (!quantilesP[1] == 0.5) {
      cli::cli_abort("The first entry in the supplied quantiles needs to be 0.5")
    }

    params
  },
  extra_params = c("na.rm", "orientation", "quantilesP"),
  compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    # qs <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
    qs <- quantilesP[2:length(quantilesP)]

    r <- map(qs, function(x) {
      qqLow <- as.numeric(stats::quantile(data$y, 1 - x))
      qqUp <- as.numeric(stats::quantile(data$y, x))
      median <- stats::median(data$y)
      return(c(qqLow, qqUp, median))
    })

    r <- do.call(rbind.data.frame, r)
    colnames(r) <- c("lower", "upper", "middle")
    r$quantileGroup <- str_c(1 - qs, qs, sep = ":")

    if (vec_unique_count(data$x) > 1) {
      width <- diff(range(data$x)) * 0.9
    }

    # df <- data_frame0(!!!as.list(stats))
    df <- r
    # df$outliers <- list(data$y[outliers])

    if (is.null(data$weight)) {
      n <- sum(!is.na(data$y))
    } else {
      # Sum up weights for non-NA positions of y and weight
      n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
    }

    # df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
    # df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)

    df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
    df$width <- width
    df$relvarwidth <- sqrt(n)
    df$flipped_aes <- flipped_aes
    flip_data(df, flipped_aes)
  }
)

#' @export
GeomQuantileplot <- ggproto("GeomQuantileplot", Geom,

  # need to declare `width` here in case this geom is used with a stat that
  # doesn't have a `width` parameter (e.g., `stat_identity`).
  extra_params = c("na.rm", "width", "orientation"),
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },
  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)


    if (!is.null(data$outliers)) {
      suppressWarnings({
        out_min <- vapply(data$outliers, min, numeric(1))
        out_max <- vapply(data$outliers, max, numeric(1))
      })

      data$ymin_final <- pmin(out_min, data$ymin)
      data$ymax_final <- pmax(out_max, data$ymax)
    }
    data$fill <- factor(str_c(data$fill, " ", data$quantileGroup, sep = ""), levels = levels(data$fill)[str_detect(levels(data$fill), ":")])
    # if `varwidth` not requested or not available, don't use it
    data$xmin <- data$x - data$width / 2
    data$xmax <- data$x + data$width / 2
    data$width <- NULL
    if (!is.null(data$relvarwidth)) data$relvarwidth <- NULL
    flip_data(data, params$flipped_aes)
  },
  draw_group = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", fatten = 2, outlier.colour = NULL,
                        outlier.fill = NULL, outlier.shape = 19,
                        outlier.size = 1.5, outlier.stroke = 0.5,
                        outlier.alpha = NULL, notch = FALSE, notchwidth = 0.5,
                        varwidth = FALSE, flipped_aes = FALSE) {
    # For reasons unknown to me, the data being fed into here is not the same as the data coming out of compute_group (which I think it should be)
    # I have to put some hacky hacks here to make things work...
    data$xmin <- min(data$xmin)
    data$xmax <- max(data$xmax)
    data <- check_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)
    data <- data[dim(data)[1]:1, ]

    if (nrow(data) <= 1) {
      cli::cli_abort(c(
        "Make sure to supply more than one quantile."
      ))
    }


    common <- list(
      colour = data$colour,
      linewidth = data$linewidth,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group
    )

    box <- data_frame0(
      xmin = data$xmin,
      xmax = data$xmax,
      ymin = data$lower,
      y = data$middle,
      ymax = data$upper,
      ynotchlower = ifelse(notch, data$notchlower, NA),
      ynotchupper = ifelse(notch, data$notchupper, NA),
      notchwidth = notchwidth,
      alpha = data$alpha,
      !!!common
    )
    box <- flip_data(box, flipped_aes)

    # browser()
    ggname("geom_quantileplot", grobTree(
      # GeomCrossbar$draw_panel expects box to be a 1-row dataframe, but in our case it's not. The function throws a warning because of that.
      # This is very hacky...
      suppressWarnings(GeomQuantile$draw_panel(
        box,
        fatten = 2,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin,
        flipped_aes = flipped_aes
      ))
    ))
  },
  draw_key = draw_key_quantileplot,
  default_aes = aes(
    weight = 1, colour = "grey20", fill = "white", size = NULL,
    alpha = NA, shape = 19, linetype = "solid", linewidth = 0.5
  ),

  # required_aes = c("x|y", "lower|xlower", "upper|xupper", "middle|xmiddle", "ymin|xmin", "ymax|xmax"),
  required_aes = c("x|y", "lower|xlower", "upper|xupper", "middle|xmiddle", "fill"),
  rename_size = TRUE
)
