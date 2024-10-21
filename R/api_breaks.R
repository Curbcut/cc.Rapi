#' Generate breaks for a Curbcut page
#'
#' @param var_left <`character`>
#' @param var_right <`character`>
#' @param zoom_levels <`character`>
#' @param time <`numeric`>
#' @param region <`character`>
#'
#' @return A list containing three components.
#' @export
api_breaks <- function(var_left, var_right = " ", zoom_levels, time = NULL, region = NULL) {

  if (length(time) == 1) stop("`time` should be sent to breaks API only on delta mode")
  start_time <- Sys.time()

  var_vec <- var_left
  if (var_right != " ") var_vec <- c(var_left, var_right)
  var_vec <- paste(sprintf("'%s'", var_vec), collapse = ", ")
  variables <- db_get_helper(sprintf("SELECT * FROM mtl.variables WHERE var_code IN (%s)", var_vec))

  # Timing db_get_helper
  if (is.null(time)) {
    time <- variables$dates[[1]]
    time <- time[length(time)]
  }


  # Timing vars_build
  vars <- vars_build(var_left = var_left, var_right = var_right, scale = zoom_levels[1],
                     time = time, variables = variables)

  time_formatted <- vars$time
  vars <- vars$vars

  # Get data
  data <- data_get(vars, scale = zoom_levels, region, variables = variables,
                   time = time_formatted, schemas = schemas, reduce = FALSE)

  # Extract breaks
  breaks <- list()

  # Function to retrieve the breaks and add them to the list
  retrieve_breaks <- function(data_element) {
    breaks_var_left <- attr(data_element, "breaks_var_left")
    breaks_var_right <- attr(data_element, "breaks_var_right")

    result <- list()
    if (!is.null(breaks_var_left)) {
      result$breaks_var_left <- breaks_var_left
    }
    if (!is.null(breaks_var_right)) {
      result$breaks_var_right <- breaks_var_right
    }

    return(result)
  }

  # Apply the function to each scale in the data list
  breaks <- lapply(data, retrieve_breaks)

  end_time <- Sys.time()

  return(list(
    breaks = breaks,
    timing = list(breaks_var_left = as.numeric(end_time - start_time))
  ))
}
