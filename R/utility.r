

# This file loads dependencies and non-exposed functions (mainly from ggplot2) that I found were required to get this to work
# Some resources state that loading entire packages is discourared, but many others do it. I'm trying to keep the environemnt as clean as I can.
#' @import ggplot2 purrr
#' @importFrom rlang list2
#' @importFrom vctrs vec_unique_count

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

#' @export
convert_to_quantile_plot_factors <- function(data, factorName, numExtensions = NULL) {
    if (!is.factor(data[[factorName]])) {
      cli::cli_warn(str_c("Transforming vector", factorName, " into factor"))
    }
    newLevels <- c()
    cli::cli_warn(str_c("Attention: Adding dummy levels to column ", factorName))
    for (origLevel in levels(data[[factorName]])) {
      newLevels <- c(newLevels, c(origLevel, str_c(origLevel, 1:numExtensions, sep = "___")))
    }
    data[[factorName]] <- factor(data[[factorName]], levels = newLevels)
    return(data)
}

quantile_palette  <- function(baseColors, quantilesP) {
    # discrete_scale needs a function that takes a single integer as an argument and returns the color values we need
    # This function fulfills this criterion, albeit in an idiotic fashion: The integer is never actually used.
    # But I think for our purposes this is fine. I wish I could solve this in a better fashion!
    function(n) {
        return(unlist(map(baseColors, \(x) {
            rev(map(rev_scale(quantilesP[2:length(quantilesP)]), \(y) {
                colorspace::lighten(x, amount = y, method = "relative")
            }))
        })))
    }
}

#' @export
scale_fill_quantile <- function(
    baseColors = NULL,
    quantilesPLevels = NULL,
    quantilesP = NULL,
    ...) {
    quantilesPLevelsBase <- quantilesPLevels
    quantilesPLevelsBase <- quantilesPLevelsBase[!str_detect(quantilesPLevelsBase, "___")]
    labels <- quantilesPLevels[1:(length(quantilesPLevels)-length(baseColors))]
    quantilesStr <- str_c(str_c((1 - quantilesP[2:length(quantilesP)]) * 100, "", sep = ""), str_c(quantilesP[2:length(quantilesP)] * 100, "%", sep = ""), sep = "-")
    quantilesStr <- unlist(map(quantilesPLevelsBase, \(x) str_c(x, quantilesStr, sep = ": ")))
    #browser()
    discrete_scale(
        aesthetics = "fill",
        scale_name = "scale_fill_quantile",
        palette = quantile_palette(baseColors, quantilesP),
        breaks = labels,
        labels = quantilesStr,
        drop = FALSE
    )
}

#' @export
add_quantileplot_legend <- function(basePlot, data, baseColors, quantilesP) {
    # remove original legend from basePlor
    origPlot <- basePlot
    basePlot <- basePlot + theme(legend.position = "none")
    # Modify legend
    fillVar <- rlang::as_name(pQuantileplot$layers[[1]]$mapping$fill)
    origPlot <- origPlot +
        scale_fill_quantile(baseColors = unname(baseColors), quantilesPLevels = levels(data[[fillVar]]), quantilesP = quantilesP) +
        theme(
            legend.key.size = unit(1, "lines")
        )
    # Extract and paste together with patchwork
    legendPlot <- cowplot::get_legend(origPlot)
    return((basePlot) + legendPlot + plot_layout(widths = c(2, 1)))
}

