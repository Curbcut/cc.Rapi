#' Generate breaks for a Curbcut page
#'
#' @param var_left <`character`>
#' @param var_right <`character`>
#' @param scale <`character`>
#' @param time <`character`>
#' @param region <`character`>
#'
#' @return A list containing three components.
#' @export
api_breaks <- function(var_left, var_right = " ", scale, time, region = NULL) {
  start_time <- Sys.time()

  var_vec <- var_left
  if (var_right != " ") var_vec <- c(var_left, var_right)
  var_vec <- paste(sprintf("'%s'", var_vec), collapse = ", ")

  # Timing db_get_helper
  variables <- db_get_helper(sprintf("SELECT * FROM mtl.variables WHERE var_code IN (%s)", var_vec))

  # Timing vars_build
  vars <- vars_build(var_left, var_right = " ", scale, time, variables = variables)

  time_formatted <- vars$time
  vars <- vars$vars

  # Timing data_get
  data <- data_get(vars, scale, region, variables = variables)

  end_time <- Sys.time()
  end_time - start_time

  return(list(
    breaks = attr(data, "breaks_var_left"),
    timing = list(breaks = end_time - start_time)
  ))
}
