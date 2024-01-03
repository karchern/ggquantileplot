

# This file loads dependencies and non-exposed functions (mainly from ggplot2) that I found were required to get this to work
# Some resources state that loading entire packages is discourared, but many others do it. I'm trying to keep the environemnt as clean as I can.
#' @import ggplot2 purrr
#' @import grid
#' @importFrom rlang list2
#' @importFrom vctrs vec_unique_count
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom dplyr tibble

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

data_frame0 <- function(...) tibble(..., .name_repair = "minimal")

#' @export
convert_to_quantile_plot_factors <- function(data, factorName = "fill", quantilesP) {
  if (!is.factor(data[[factorName]])) {
    cli::cli_abort(str_c("Fill vector needs to be a factor."))
  }
  newLevels <- c()
  for (origLevel in levels(data[[factorName]])) {
    newLevels <- c(newLevels, c(origLevel, str_c(origLevel, " ", str_c(1 - quantilesP[2:length(quantilesP)], quantilesP[2:length(quantilesP)], sep = ":"))))
  }
  data[[factorName]] <- factor(data[[factorName]], levels = newLevels)
  return(data)
}

#' @export
quantile_palette <- function(baseColors, quantilesP) {
  # discrete_scale needs a function that takes a single integer as an argument and returns the color values we need
  # This function fulfills this criterion, albeit in an idiotic fashion: The integer is never actually used.
  # But I think for our purposes this is fine. I wish I could solve this in a better fashion!
  function(n) {
    # The unname here is crucial
    return(unname(unlist(map(baseColors, \(x) {
      rev(map(rev_scale(quantilesP[2:length(quantilesP)]), \(y) {
        colorspace::lighten(x, amount = y, method = "relative")
      }))
    }))))
  }
}

#' @export
# scale_fill_quantile <- function(baseColors, quantilesP) {
#   return(scale_fill_manual(values = quantile_palette(baseColors, quantilesP)()))
# }
scale_fill_quantile <- function(baseColors, quantilesP) {
    return(scale_fill_manual(values = quantile_palette(baseColors, quantilesP)()[1:10]))
}
