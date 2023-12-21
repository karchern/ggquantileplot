library(tidyverse)
library(grid)
library(ggplot2)
# These functions need to be loaded for some internal functions in GeomQuantilePlot
library(rlang)
library(ggforce)
library(vctrs)
library(colorspace)

rev_scale <- function(x) {
    return((max(x) - x) / (max(x) - min(x)))
}

show_stack <- function() {
  cat("#----- Stack containing call to show_stack -----#\n\n")
  x <- sys.calls()
  lapply(head(x, -1), function(x) {print(x); cat("\n")})
  cat("#-----------------------------------------------#\n\n")
}

# Some more coded that needed to be copied manually. I don't know why these functions are not exposed and I'm not even sure where they come from
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

NO_GROUP <- -1L

# Ensure that the data frame contains a grouping variable.
#
# If the `group` variable is not present, then a new group
# variable is generated from the interaction of all discrete (factor or
# character) vectors, excluding `label`. The special value `NO_GROUP`
# is used for all observations if no discrete variables exist.
add_group <- function(data) {
  if (empty(data)) return(data)

  if (is.null(data[["group"]])) {
    disc <- vapply(data, is.discrete, logical(1))
    disc[names(disc) %in% c("label", "PANEL")] <- FALSE

    if (any(disc)) {
      data$group <- id(data[disc], drop = TRUE)
    } else {
      data$group <- NO_GROUP
      attr(data$group, "n") <- 1L
    }
  } else {
    data$group <- id(data["group"], drop = TRUE)
  }

  data
}

# Is a grouping available?
# (Will return TRUE if an explicit group or a discrete variable with only one
# level existed when add_group() was called.)
has_groups <- function(data) {
  # If no group aesthetic is specified, all values of the group column equal to
  # NO_GROUP. On the other hand, if a group aesthetic is specified, all values
  # are different from NO_GROUP (since they are a result of plyr::id()). NA is
  # returned for 0-row data frames.
  data$group[1L] != NO_GROUP
}

# From ggplot2/R/utilities.r
snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}

snake_class <- function(x) {
    snakeize(class(x)[1])
}

check_linewidth <- function(data, name) {
  if (is.null(data$linewidth) && !is.null(data$size)) {
    deprecate_soft0("3.4.0", I(paste0("Using the `size` aesthetic with ", name)), I("the `linewidth` aesthetic"))
    data$linewidth <- data$size
  }
  data
}

is_mapped_discrete <- function (x) inherits(x, "mapped_discrete")

data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")

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
      ...
    )
  )
}

StatQuantileplot <- ggproto("StatBoxplot", Stat,
  required_aes = c("y|x"),
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
    flip_data(data, params$flipped_aes)
  },

  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = TRUE,
                                          group_has_equal = TRUE,
                                          main_is_optional = TRUE)
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

    params
  },

  extra_params = c("na.rm", "orientation"),

  compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    qs <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)

    r <- map(qs, \(x) {
        qqLow <- as.numeric(stats::quantile(data$y, 1 - x))
        qqUp <- as.numeric(stats::quantile(data$y, x))
        median <- stats::median(data$y)
        return(c(qqLow, qqUp, median))
    })

    r <- do.call(rbind.data.frame, r)
    colnames(r) <- c("lower", "upper", 'middle')
    r$quantileGroup <- str_c(qs, 1 - qs, sep = ":")
    
    


    # if (!is.null(data$weight)) {
    #     mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
    #     stats <- as.numeric(stats::coef(mod))
    # } else {
    #stats <- as.numeric(stats::quantile(data$y, qs))
    #names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    #iqr <- diff(stats[c(2, 4)])

    # outliers <- data$y < (stats[2] - coef * iqr) | data$y > (stats[4] + coef * iqr)
    # if (any(outliers)) {
    #   stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
    # }

    if (vec_unique_count(data$x) > 1)
      width <- diff(range(data$x)) * 0.9

    #df <- data_frame0(!!!as.list(stats))
    df <- r
    #df$outliers <- list(data$y[outliers])

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

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
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

      data$ymin_final  <- pmin(out_min, data$ymin)
      data$ymax_final  <- pmax(out_max, data$ymax)
    }
    
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
    browser()
    if ("fill" %in% colnames(data)) {
        data$fill <- map_chr(rev_scale(1:length(data$fill)), \(n) {
            return(colorspace::lighten(data$fill[1], amount = n, method = "relative"))
        })
    }    

    
    
    # this may occur when using geom_quantileplot(stat = "identity")
    # if (nrow(data) != 1) {
    #     cli::cli_abort(c(
    #         "Can only draw one boxplot per group",
    #         "i" = "Did you forget {.code aes(group = ...)}?"
    #     ))
    # }

    if (nrow(data) <= 1) {
        cli::cli_abort(c(
            "Make sure to supply more than one quantile"
        ))
    }    
    

    common <- list(
      colour = data$colour,
      linewidth = data$linewidth,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group
    )
    
    # whiskers <- data_frame0(
    #   x = c(data$x, data$x),
    #   xend = c(data$x, data$x),
    #   y = c(data$upper, data$lower),
    #   yend = c(data$ymax, data$ymin),
    #   alpha = c(NA_real_, NA_real_),
    #   !!!common,
    #   .size = 2
    # )
    # whiskers <- flip_data(whiskers, flipped_aes)

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

    # if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
    #   outliers <- data_frame0(
    #     y = data$outliers[[1]],
    #     x = data$x[1],
    #     colour = outlier.colour %||% data$colour[1],
    #     fill = outlier.fill %||% data$fill[1],
    #     shape = outlier.shape %||% data$shape[1],
    #     size = outlier.size %||% data$size[1],
    #     stroke = outlier.stroke %||% data$stroke[1],
    #     #fill = NA,
    #     alpha = outlier.alpha %||% data$alpha[1],
    #     .size = length(data$outliers[[1]])
    #   )
    #   outliers <- flip_data(outliers, flipped_aes)

    #   outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    # } else {
    #   outliers_grob <- NULL
    # }
    #browser()
    ggname("geom_quantileplot", grobTree(
        GeomCrossbar$draw_panel(
            box,
            fatten = 2,
            panel_params,
            coord,
            lineend = lineend,
            linejoin = linejoin,
            flipped_aes = flipped_aes
        )
    ))
  },

  draw_key = draw_key_boxplot,

  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = NULL,
    alpha = NA, shape = 19, linetype = "solid", linewidth = 0.5),

  #required_aes = c("x|y", "lower|xlower", "upper|xupper", "middle|xmiddle", "ymin|xmin", "ymax|xmax"),
  required_aes = c("x|y", "lower|xlower", "upper|xupper", "middle|xmiddle", 'fill'),

  rename_size = TRUE
)

ggplot(data = ToothGrowth, aes(x = as.factor(dose), y = len)) +
    geom_quantileplot(aes(fill = as.factor(supp))) +
    scale_fill_manual(values = c('OJ' = "#163300", "VC" = "#4d1601"))