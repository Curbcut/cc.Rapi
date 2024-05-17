# var_left <- "housing_tenant"
# var_right = " "
# scale <- "DA"
# region <- "CMA"
# time <- 2021
# select_id <- NA
# schema <- "mtl"
# lang <- NULL
# schemas = list(var_left = list(time = time), var_right = list(time = time))
# db_pool <- db_connection()


#' Generate Contextual Information for a Curbcut page
#'
#' This function generates contextual information such as legend, graph, and text
#' for the application based on provided parameters. It fetches variables from
#' the database, processes them, and then generates the necessary components
#' for the application's context.
#'
#' @param var_left <`character`>
#' @param var_right <`character`>
#' @param scale <`character`>
#' @param region <`character`>
#' @param time <`character`>
#' @param select_id <`character`>
#' @param lang <`character vector`>
#' @param top_scale <`character vector`>
#' @param schemas <`list`>
#'
#' @return A list containing three components.
#' @export
context <- function(var_left, var_right = " ", scale, region = NULL, time, select_id,
                    lang = NULL, top_scale, schemas = list(var_left = list(time = time),
                                                             var_right = list(time = time))) {

  var_vec <- var_left
  if (var_right != " ") var_vec <- c(var_left, var_right)
  var_vec <- paste(sprintf("'%s'", var_vec), collapse = ", ")

  # Execute the query using db_get_helper
  variables <- db_get_helper(sprintf("SELECT * FROM mtl.variables
                                     WHERE var_code IN (%s)
                                     UNION
                                     SELECT * FROM mtl.variables
                                     WHERE var_code IN (
                                     SELECT parent_vec
                                     FROM mtl.variables
                                     WHERE var_code IN (%s)
                                     )
                                     ", var_vec, var_vec))

  vars <- vars_build(var_left, var_right = " ", scale, time, variables = variables)
  time_formatted <- vars$time
  vars <- vars$vars
  data <- data_get(vars, scale, region, variables = variables)

  legend <- legend_render(vars = vars, scale = scale, data = data, variables = variables)
  graph <- explore_graph(vars = vars, select_id = select_id, scale = scale, data = data,
                         time = time_formatted, schemas = schemas, lang = lang, variables = variables)
  text <- explore_text(vars = vars, select_id = select_id, scale = scale, region = region,
                       data = data, time = time_formatted, schemas = schemas, lang = lang,
                       top_scale = top_scale, variables = variables)


  # Save the plot to a temporary file
  legend_file <- tempfile(fileext = ".png")
  ggplot2::ggsave(legend_file, plot = graph, device = "png")

  # Save the plot to a temporary file
  graph_file <- tempfile(fileext = ".png")
  ggplot2::ggsave(graph_file, plot = graph, device = "png")

  return(list(legend = base64enc::base64encode(legend_file),
              graph = base64enc::base64encode(graph_file),
              text = text))
}


#' Run Plumber API
#'
#' This function initializes and runs the Plumber API for the `cc.Rapi` package.
#' It also sets up a hook to close the database pool when the API is stopped.
#'
#' @param host <`character`> Which host to start the API? Defaults to 127.0.0.1
#' for localhost. Must be "0.0.0.0" in docker.
#' @param port <`numeric`> The port on which the API should be listening. Defaults
#' to 8000.
#'
#' @return This function does not return a value. It starts the Plumber API server.
#' @export
run_api <- function(host = "127.0.0.1", port = 8000) {
  api <- plumber::plumb_api("cc.Rapi", "context")

  api$registerHooks(list(
    exit = function() {
      print("database pooling connection disconnected on exit")
      pool::poolClose(get_from_globalenv("db_pool"))
    }
  ))

  plumber::pr_run(api, host = host, port = port)
}
