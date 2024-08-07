#' Calculate Breaks for Delta Legends
#'
#' This function calculates the break points for delta legends based on the data.
#' It also handles special cases, such as when all data is within a small range.
#'
#' @param vars <`named list`> A list object with a pre-determined class. The
#' output of \code{\link{vars_build}}.
#' @param scale <`character`> The scale at which the user is on. The output of
#' @param character <`logical`>, if `TRUE`, the breaks are returned as characters.
#' @param data <`data.frame`> Optional, a data frame to use instead of recovering it
#' from the file.
#'
#' @return A vector of break points. Numeric if `character = FALSE`, otherwise
#' character.
#' @export
breaks_delta <- function(vars, scale = NULL, character = FALSE, data = NULL) {
  if (is.null(data)) {
    data <- data_get(vars = vars, scale = scale)
  }

  return(breaks_delta_helper(data, character))
}

#' Breaks Delta Helper Function
#'
#' This function and its methods create breaks for positive, negative, or both
#' types of values in a given vector. It handles outliers and provides options
#' for output format.
#'
#' @param data <`numeric`> Data containg the class fo inform if the values are
#' only negative, positive or mixed.
#' @param character <`logical`> If TRUE, returns breaks as character strings.
#' @param ... Additional arguments
#'
#' @return <`numeric/character`> Breaks from negative to positive values.
#' @export
breaks_delta_helper <- function(data, character, ...) {
  UseMethod("breaks_delta_helper", data)
}

#' @describeIn breaks_delta_helper default method
#' @return Return breaks from negative to positive values
#' @export
breaks_delta_helper.default <- function(data, character, ...) {
  # Data as absolute
  data_vl <- abs(data$var_left)

  # Remove outliers
  which_out <- find_outliers(x = data_vl)
  if (length(which_out) > 0) data_vl <- data_vl[-which_out]

  # Use the 50th and 100th percentile for the last bracket
  breaks <- stats::quantile(data_vl, na.rm = TRUE)[c(3, 5)]
  breaks <- c(0.02, breaks)

  # If the last break is lower than 2% (which is the forced middle bracket),
  # override the last bracket to 10%
  if (breaks[2] <= 0.03) {
    breaks[2] <- 0.1
    breaks[3] <- 0.15
  }

  # Convert to character
  if (character) breaks <- convert_unit.pct(x = breaks, decimal = 0)

  # Construct the output
  out <- c(
    sprintf("-%s", breaks[3]),
    sprintf("-%s", breaks[2]),
    sprintf("-%s", breaks[1]),
    breaks[1],
    breaks[2],
    breaks[3]
  )
  out <- unname(out)

  if (!character) {
    return(as.numeric(out))
  }
  out
}

#' Adjust Probabilities for Quantile Breaks
#'
#' This function incrementally adjusts a vector of probabilities used for
#' quantile breaks. It ensures that each break has a non-zero value and
#' maintains at least a specified minimum gap between each consecutive
#' probability value.
#'
#' @param probs <`numeric vector`> A numeric vector of probabilities.
#' @param breaks <`numeric vector`> A numeric vector of quantile breaks
#' corresponding to `probs`.
#' @param min_gap <`numeric`> Minimum gap to maintain between consecutive
#' probabilities.
#'
#' @return <`numeric vector`> An adjusted vector of probabilities where each
#' element is non-zero and consecutive elements have at least `min_gap`
#' difference.
adjust_probs <- function(probs, breaks, min_gap) {
  for (i in seq_along(probs)) {
    if (breaks[i] == 0) {
      probs[i] <- probs[i] + min_gap
    }
    # Ensure probs do not exceed 1 and maintain minimum gap
    if (i > 1) {
      probs[i] <- max(probs[i], probs[i-1] + min_gap)
    }
    probs[i] <- min(probs[i], 1)
  }
  return(probs)
}

#' @describeIn breaks_delta_helper positive method
#' @return Return breaks as only positive values
#' @export
breaks_delta_helper.positive <- function(data, character, ...) {

  vec <- data$var_left

  # Remove outliers
  which_out <- find_outliers(x = vec)
  if (length(which_out) > 0) vec <- vec[-which_out]

  # Split in 5
  probs <- c(0.2,0.4,0.6,0.8,1)
  breaks <- stats::quantile(vec, probs = probs, na.rm = TRUE)

  # Loop to adjust breaks until no zeros and maintain minimum gap
  min_gap <- 0.025
  while (any(breaks == 0) || any(diff(probs) < min_gap)) {
    probs <- adjust_probs(probs, breaks, min_gap)
    breaks <- stats::quantile(vec, probs = probs, na.rm = TRUE)
  }

  breaks <- c(0, breaks)
  # Try and make the second break a 2% increase (only if it's suitable: if
  # the following break is higher than a 3% increase)
  if (breaks[3] > 0.03) breaks[2] <- 0.02

  # Convert to character
  if (character) breaks <- convert_unit.pct(x = breaks, decimal = 0)

  out <- unname(breaks)

  if (!character) {
    return(as.numeric(out))
  }

  out
}

#' @describeIn breaks_delta_helper negative method
#' @return Return breaks as only negative values
#' @export
breaks_delta_helper.negative <- function(data, character, ...) {

  vec <- data$var_left

  # Remove outliers
  which_out <- find_outliers(x = vec)
  if (length(which_out) > 0) vec <- vec[-which_out]

  # Split in 5
  probs <- c(0.2,0.4,0.6,0.8,1)
  breaks <- stats::quantile(vec, probs = probs, na.rm = TRUE)

  # Loop to adjust breaks until no zeros and maintain minimum gap
  min_gap <- 0.025
  while (any(breaks == 0) || any(diff(probs) < min_gap)) {
    probs <- adjust_probs(probs, breaks, min_gap)
    breaks <- stats::quantile(vec, probs = probs, na.rm = TRUE)
  }

  breaks <- c(breaks, 0)
  # Try and make the second break a 2% increase (only if it's suitable: if
  # the following break is higher than a 3% increase)
  if (breaks[4] < -0.03) breaks[5] <- -0.02

  # Convert to character
  if (character) breaks <- convert_unit.pct(x = breaks, decimal = 0)

  out <- unname(breaks)

  if (!character) {
    return(as.numeric(out))
  }

  out
}

#' Find quintile breaks
#'
#' @param dist <`numeric`> Distribution (numerics) with no NAs.
#' @param q3_q5 <`character`> How many bins? `q3` or `q5`.
#'
#' @return Returns a numeric vector with quintile breaks.
#' @export
find_breaks_quintiles <- function(dist, q3_q5 = "q5") {
  # Remove outliers
  which_out <- find_outliers(x = dist)
  no_outliers <- dist[-which_out]

  # If there are not enough values once the outliers are gone, grab all the
  # distribution.
  dat <- if (length(unique(no_outliers)) >= 10) no_outliers else dist
  dat <- unique(dat)

  # Calculate quintiles
  by <- if (q3_q5 == "q5") {
    0.2
  } else if (q3_q5 == "q3") {
    0.33
  } else {
    stop("`q3_q5` argument needs to be q3 or q5")
  }

  q <- stats::quantile(dat, probs = seq(0, 1, by = by), names = FALSE)

  # Create empty breaks vector
  breaks <- numeric(length(q))

  # Initialize first break and previous_q
  previous_q <- 0

  # Loop through all the quantiles
  for (i in seq_along(q)) {
    # Check if difference between current quantile and previous one is zero
    if (q[i] - previous_q == 0) {
      round_base <- 1
    } else {
      # Determine the rounding base for each quantile difference
      round_base <- 10^floor(log10(abs(q[i] - previous_q)))
    }

    # Create a "pretty" break ensuring it's different from the previous one
    new_break <- round(q[i] / round_base) * round_base

    # Ensure there's always a value in the break
    while (!(any(dist >= new_break & dist < q[i + 1]) || (i == length(q) && any(dist >= new_break)))) {
      round_base <- round_base / 10
      new_break <- round(q[i] / round_base) * round_base
    }

    # If it's the first break and it's equal to zero, do nothing.
    # If the new break is the same as the previous one, decrease rounding base
    # until they are different
    if (!(i == 1 && new_break == 0)) {
      while (new_break %in% breaks) {
        round_base <- round_base / 10
        new_break <- round(q[i] / round_base) * round_base
      }
    }

    # Assign the new break to the breaks vector
    breaks[i] <- new_break
    previous_q <- new_break
  }

  # Check if the first break is much closer to 0 than the second break
  if (breaks[2] / breaks[1] > 10) {
    breaks[1] <- 0
  }

  # If the minimum value was already 0
  if (min(dist) == 0) {
    breaks[1] <- 0
  }

  # Make sure the order is lowest to highest
  breaks <- breaks[order(breaks)]

  return(breaks)
}


#' Find pretty q5 breaks
#'
#' @param min_val <`numeric`>
#' @param max_val <`numeric`>
#'
#' @return Returns a numeric vector with pretty q5 break values.
#' @export
find_breaks_q5 <- function(min_val, max_val) {
  breaks <- unlist(lapply(
    -4:7, \(x) (10^x) * c(0.75, 1, 1.5, 2, 2.5, 3, 4, 5, 6)
  ))
  range <- max_val - min_val
  break_val <- range / 5
  break_val <- breaks[as.numeric(cut(break_val, breaks)) + 1]
  break_digits <- floor(log10(break_val))
  new_min <- floor(min_val / (10^break_digits)) * 10^break_digits
  return(c(new_min + 0:5 * break_val))
}

use_quantiles <- function(data_vec) {
  # Test and see the q5 breaks (always use the latest date)
  min_val <- min(data_vec, na.rm = TRUE)
  max_val <- max(data_vec, na.rm = TRUE)
  breaks <- find_breaks_q5(min_val = min_val, max_val = max_val)

  # If there are no valid observations, return the NAs
  if (all(is.na(breaks))) return(FALSE)

  # Are bins WAY to unequals?
  bin_occ <- .bincode(data_vec, breaks, include.lowest = TRUE)
  bin_occ_pct <- table(bin_occ) / length(bin_occ)

  # If there is one bin that has more than half ot observations, use quintiles
  use_quintiles <- sum(bin_occ_pct > 0.5) > 0
  return(use_quintiles)

}

#' Append new columns based on quantile breaks
#'
#' This function appends new columns to the input data based on quantile breaks
#' for specific variables. The quantile breaks are calculated using the function
#' `find_breaks_quintiles`. The function also preserves other attributes of the
#' original data.
#'
#' @param var <`character`> var_code field, which will be renamed using `rename_col`.
#' @param data <`data.frame`> Data frame to append columns to.
#' @param q3_q5 <`character`> Specifies whether to use three or five quantiles.
#' Default is "q5".
#' @param rename_col <`character`> The column name to rename. Default is
#' "var_left". Can also be "var_right".
#' @param variables <`data.frame`> Dataframe of the variables dictionary, containing
#' both var_left and var_right, and any potential parent variable aswell.
#'
#' @return <`data.frame`> Modified data frame with additional columns.
data_append_breaks <- function(var, data, q3_q5 = "q5", rename_col = "var_left",
                               variables) {
  # Keep track of previous attributes
  prev_attr <- attributes(data)
  prev_attr <- prev_attr[!names(prev_attr) %in% c(
    "names", "row.names", "class",
    "quintiles", "breaks"
  )]

  # Calculate breaks
  data_val <- data[!names(data) %in% c("ID", "scale")]
  breaks_var <- variables$breaks_var[variables$var_code == var]
  if (is.na(breaks_var)) {
    recent <- max(variables$dates[variables$var_code == var][[1]])
    breaks_var <- sprintf("%s_%s", var, recent)
  }
  data_vec <- data[[breaks_var]]
  data_vec <- data_vec[!is.na(data_vec)]
  # Add it to the next data attributes
  prev_attr$breaks_var <- breaks_var
  # Take the opportunity to also add schema to the data
  prev_attr$schema <- variables$schema[variables$var_code == var][[1]]

  # Calculate break
  quintiles <- use_quantiles(data_vec)

  # Are breaks hardcoded in the variables table? Output that.
  brks <- variables$breaks_q5[variables$var_code == var][[1]]

  breaks <-   # Determine the appropriate break points based on given conditions
  if (q3_q5 == "q3") {
    # If q3_q5 is q3, always find breaks for quintiles
    find_breaks_quintiles(dist = data_vec, q3_q5 = q3_q5)
  } else {
    # For cases when q3_q5 is not q3
    if (!is.null(brks) & length(brks) > 0) {
      # Use brks if it's not NULL
      brks
    } else if (quintiles) {
      # Find breaks for quintiles if quintiles is TRUE
      find_breaks_quintiles(dist = data_vec, q3_q5 = q3_q5)
    } else {
      # Default case: find breaks based on q5 method
      find_breaks_q5(min_val = min(data_vec), max_val = max(data_vec))
    }
  }

  # Rework breaks just for assembling (we want to include ALL observations)
  assemble_breaks <- breaks
  assemble_breaks[1] <- -Inf
  assemble_breaks[length(assemble_breaks)] <- Inf

  # Assemble output
  out <- as.data.frame(lapply(data_val, .bincode, assemble_breaks, include.lowest = TRUE))
  out <- stats::setNames(out, sprintf("%s_%s", names(data_val), q3_q5))
  data <- cbind(data, out) # bind the data
  data <- data.frame(data)
  attr(data, sprintf("breaks_%s", rename_col)) <- breaks

  # Rename attributes so it's clearly assigned on var_left or var_right
  names(prev_attr) <- sprintf("%s_%s", names(prev_attr), rename_col)
  # Keep the attributes
  for (i in names(prev_attr)) {
    attr(data, i) <- prev_attr[[i]]
  }

  # Rename fields
  names(data) <- gsub(var, rename_col, names(data))

  return(list(data = data, attr = prev_attr))
}
