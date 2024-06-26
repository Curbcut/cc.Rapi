#' Compute region values based on different methods
#'
#' This function computes region values based on the method specified by `var`'s
#' class. It uses weighted means, sums, or other calculations.
#' It fetches the relevant data and parent data, and then calls the
#' appropriate method for calculation.
#'
#' @param var <`character`> The code of the variable of interest, with its class
#' to dispatch to the right method.
#' @param data <`data.frame`> The data frame containing the data.
#' @param time <`numeric`> The time period of interest.
#' @param scale <`character`> The scale of interest.
#' @param region  <`character`> The region of interest.
#' @param select_id <`character`>  The ID of the selected feature.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right`.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @param ... Additional arguments to pass to specific method functions.
#'
#' @return Varies based on the method specified in \code{var}. Commonly
#' returns a list with elements like \code{val} and \code{count}.
#' @export
region_value <- function(var, data, time, scale, region, select_id, col, schemas = NULL,
                         variables, ...) {
  # Get the parent variable data
  rv <- region_value_data_grab(
    var = var, data = data, time = time, col = col,
    scale = scale, region = region, schemas = schemas,
    variables = variables
  )

  # Return the output of every method
  region_value_method(
    var = var, data_vals = rv$data_vals,
    parent_vals = rv$parent_vals, data = data,
    time = time, col = col, schemas = schemas, ...
  )
}

#' Methods to compute regional values
#'
#' A set of methods to compute regional values based on the type of
#' variable provided. Supports weighted mean, sums, and specific
#' indicators.
#'
#' @param var <`character`> The code of the variable of interest, with its class
#' to dispatch to the right method.
#' @param data_vals <`numeric vector`> Numeric vector containing the data values
#' for the region.
#' @param parent_vals <`numeric vector`> Numeric vector containing the parent
#' data values.
#' @param ... Additional arguments (e.g., data, time) used in specific methods.
#'
#' @return Returns a list with calculated values for the specific method.
#' Commonly returns elements such as \code{val} and \code{count}.
#'
#' @export
region_value_method <- function(var, data_vals, parent_vals, ...) {
  UseMethod("region_value_method", var)
}

#' @describeIn region_value_method The method for percentage variables.
#' @export
region_value_method.pct <- function(var, data_vals, parent_vals, ...) {
  out <- list()

  # Make the region values
  out$val <- weighted_mean(data_vals, parent_vals, na.rm = TRUE)
  out$count <- out$val * sum(parent_vals, na.rm = TRUE)
  out$count <- round(out$count / 5) * 5

  # Return
  return(out)
}

#' @describeIn region_value_method The method for count variables.
#' @export
region_value_method.count <- function(var, data_vals, parent_vals, ...) {
  out <- list()

  # Make the region values
  out$val <- sum(data_vals, na.rm = TRUE)

  # Return
  return(out)
}

#' @describeIn region_value_method The method for `ind` variables.
#' @param data <`data.frame`> The data frame containing the data.
#' @param time <`numeric`> The time period of interest.
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right`.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#' @export
region_value_method.ind <- function(var, data_vals, parent_vals, data, time, col, schemas = schemas, ...) {
  # Which column breaks do we want to use
  col <- match_schema_to_col(data, time = time, col = col, schemas = schemas)
  brk_col <- sprintf("%s_q5", col)

  # Get the breaks
  brks <- data[c("ID", brk_col)]
  brks$parent_vals <- parent_vals

  # How many 'parent' in the two top brackets
  val <- brks$parent_vals[brks[[brk_col]] > 3]

  # Calculate count a value
  out <- list()
  out$count <- sum(val, na.rm = TRUE)
  out$val <- out$count / sum(brks$parent_vals, na.rm = TRUE)

  # Return
  return(out)
}

#' @describeIn region_value_method The people per object (ppo) method.
#' @export
region_value_method.default <- function(var, data_vals, parent_vals, ...) {
  out <- list()

  # Calculating total number of trees
  total_trees <- sum(parent_vals / data_vals)

  # Calculating total number of people
  total_population <- sum(parent_vals)

  # Calculating overall people per tree in the region
  total_population / total_trees
}

#' @describeIn region_value_method The default method (works for dollar, sqkm, per1k, ...).
#' Simple weighted mean.
#' @export
region_value_method.default <- function(var, data_vals, parent_vals, ...) {
  out <- list()

  # Make the region values
  out$val <- weighted_mean(data_vals, parent_vals, na.rm = TRUE)

  # Return
  return(out)
}

#' Retrieve data values and parent data for specific region and scale
#'
#' This function gathers data values and parent data based on the given
#' variable, data, time, scale, and region. The function utilizes
#' helper functions to identify the correct column and fetch parent data.
#'
#' @param var <`character`> The code of the variable of interest.
#' @param data <`data.frame`> The data frame containing the data.
#' @param time <`numeric`> The time period of interest.
#' @param scale <`character`> The scale of interest.
#' @param region  <`character`> The region of interest.
#' "data/".
#' @param col <`character`> Which column of `data` should be selected to grab the
#' value information. Defaults to `var_left`, but could also be `var_right` or
#' `var_left_1` in delta.
#' @param schemas <`named list`> Current schema information. The additional widget
#' values that have an impact on which data column to pick. Usually `r[[id]]$schema()`.
#'
#' @return Returns a list containing two elements: \code{data_vals}, which
#' holds the data values for the specified variable, and \code{parent_vals},
#' which holds the parent data values.
#' @export
region_value_data_grab <- function(var, data, time, scale, region, col, schemas = NULL,
                                   variables) {
  # Get the parent variable
  parent_string <- var_get_info(var, variables = variables, what = "parent_vec")
  if (parent_string == "population") parent_string <- "c_population"
  if (parent_string == "households") parent_string <- "private_households"

  # Grab parent data
  if ("count" %in% class(var)) {
    parent_vals <- NULL
  } else {
    vars <- vars_build(var_left = parent_string, scale = scale, variables = variables,
                       time = time$var_left)$vars
    parent_data <- data_get(
      vars = vars, scale = scale, region = region, vr_vl = col,
      variables = variables
    )
    # In the case where there is just one value, no time. Like `area`.
    if (col %in% names(parent_data)) {
      parent_vals <- parent_data[[col]]
    } else {
      pv_col <- match_schema_to_col(
        data = parent_data, time = time, col = col,
        schemas = NULL, closest_time = TRUE
      )
      parent_vals <- parent_data[[pv_col]]
    }
  }

  # Get the correct column name to draw data from
  current_col <- match_schema_to_col(data = data, time = time, col = col, schemas = schemas)
  # Make sure not to grab q5
  data_vals <- data[[current_col]]

  # Make sure it's all numeric
  data_vals <- as.numeric(data_vals)
  parent_vals <- as.numeric(parent_vals)

  # Return both the data and the parent data
  return(list(data_vals = data_vals, parent_vals = parent_vals))
}
