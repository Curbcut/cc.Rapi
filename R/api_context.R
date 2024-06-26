# var_left <- c("alp")
# var_right = " "
# scale <- "DA"
# region <- "CMA"
# time <- 2021
# select_id <- NA
# schema <- "mtl"
# lang <- NULL
# schemas = list(var_left = list(time = time), var_right = list(time = time))
# db_pool <- db_connection()


# Generate Contextual Information for a Curbcut page
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
#' @param zoom_levels <`character vector`>
#' @param schemas <`list`>
#'
#' @return A list containing three components.
#' @export
context <- function(var_left, var_right = " ", scale, region = NULL, time, select_id,
                    lang = NULL, zoom_levels,
                    schemas = list(var_left = list(time = time),
                                   var_right = list(time = time))) {

  # var_vec <- var_left
  # if (var_right != " ") var_vec <- c(var_left, var_right)
  # var_vec <- paste(sprintf("'%s'", var_vec), collapse = ", ")
  #
  # # Execute the query using db_get_helper
  # variables <- db_get_helper(sprintf("SELECT * FROM mtl.variables
  #                                    WHERE var_code IN (%s)
  #                                    UNION
  #                                    SELECT * FROM mtl.variables
  #                                    WHERE var_code IN (
  #                                    SELECT parent_vec
  #                                    FROM mtl.variables
  #                                    WHERE var_code IN (%s)
  #                                    )
  #                                    ", var_vec, var_vec))
  #
  # vars <- vars_build(var_left, var_right = " ", scale, time, variables = variables)
  # time_formatted <- vars$time
  # vars <- vars$vars
  # data <- data_get(vars, scale, region, variables = variables)
  #
  # legend <- legend_render(vars = vars, scale = scale, data = data, variables = variables)
  # graph <- explore_graph(vars = vars, select_id = select_id, scale = scale, data = data,
  #                        time = time_formatted, schemas = schemas, lang = lang, variables = variables)
  # text <- explore_text(vars = vars, select_id = select_id, scale = scale, region = region,
  #                      data = data, time = time_formatted, schemas = schemas, lang = lang,
  #                      top_scale = zoom_levels[[1]], variables = variables)
  #
  #
  # # Save the plot to a temporary file
  # legend_file <- tempfile(fileext = ".png")
  # # Calculate the dimensions in inches, given that 1 inch is 96 pixels
  # width_in <- 268 / 96
  # height_in <- 60 / 96
  # # Save the plot with specified dimensions
  # ggplot2::ggsave(legend_file, plot = legend, device = "png", width = width_in,
  #                 height = height_in, units = "in", dpi = 96)
  #
  # # Save the plot to a temporary file
  # graph_file <- tempfile(fileext = ".png")
  # # Calculate the dimensions in inches, given that 1 inch is 96 pixels
  # width_in <- 268 / 96
  # height_in <- 150 / 96
  # # Save the plot with specified dimensions
  # ggplot2::ggsave(graph_file, plot = graph, device = "png", width = width_in,
  #                 height = height_in, units = "in", dpi = 96)
  #
  # return(list(legend = base64enc::base64encode(legend_file),
  #             graph = base64enc::base64encode(graph_file),
  #             text = text))

  start_time <- Sys.time()

  var_vec <- var_left
  if (var_right != " ") var_vec <- c(var_left, var_right)
  var_vec <- paste(sprintf("'%s'", var_vec), collapse = ", ")

  # Timing db_get_helper
  db_get_start <- Sys.time()
  variables <- db_get_helper(sprintf("SELECT * FROM mtl.variables
        WHERE var_code IN (%s)
        UNION
        SELECT * FROM mtl.variables
        WHERE var_code IN (
            SELECT
                CASE
                    WHEN parent_vec = 'households' THEN 'private_households'
                    WHEN parent_vec = 'population' THEN 'c_population'
                    ELSE parent_vec
                END
            FROM mtl.variables
            WHERE var_code IN (%s)
        )", var_vec, var_vec))
  db_get_end <- Sys.time()

  # Timing vars_build
  vars_build_start <- Sys.time()
  vars <- vars_build(var_left, var_right = " ", scale, time, variables = variables)
  vars_build_end <- Sys.time()

  time_formatted <- vars$time
  vars <- vars$vars

  # Timing data_get
  data_get_start <- Sys.time()
  data <- data_get(vars, scale, region, variables = variables)
  data_get_end <- Sys.time()

  # Timing legend_render
  legend_render_start <- Sys.time()
  legend <- legend_render(vars = vars, scale = scale, data = data, variables = variables)
  legend_render_end <- Sys.time()

  # Timing explore_graph
  explore_graph_start <- Sys.time()
  graph <- explore_graph(vars = vars, select_id = select_id, scale = scale, data = data,
                time = time_formatted, schemas = schemas, lang = lang, variables = variables)
  explore_graph_end <- Sys.time()

  # Timing explore_text
  explore_text_start <- Sys.time()
  text <- explore_text(vars = vars, select_id = select_id, scale = scale, region = region,
                       data = data, time = time_formatted, schemas = schemas, lang = lang,
                       top_scale = zoom_levels[[1]], variables = variables)
  explore_text_end <- Sys.time()

  # Map colors
  map_colors_start <- Sys.time()
  map_colors <- data_get_colours(vars = vars, region = region, time = time_formatted,
                                 zoom_levels = zoom_levels, variables = variables,
                                 schemas = schemas)
  map_colors_end <- Sys.time()

  # Timing legend file save
  legend_save_start <- Sys.time()
  legend_file <- tempfile(fileext = ".png")
  width_in <- 268 / 96
  height_in <- 60 / 96
  ggplot2::ggsave(legend_file, plot = legend, device = "png", width = width_in,
                  height = height_in, units = "in", dpi = 96)
  legend_save_end <- Sys.time()

  # Timing graph file save
  graph_save_start <- Sys.time()
  graph_file <- tempfile(fileext = ".png")
  width_in <- 268 / 96
  height_in <- 150 / 96
  ggplot2::ggsave(graph_file, plot = graph, device = "png", width = width_in,
                  height = height_in, units = "in", dpi = 96)
  graph_save_end <- Sys.time()

  end_time <- Sys.time()

  timing <- list(
    db_get_variables = db_get_end - db_get_start,
    vars_build = vars_build_end - vars_build_start,
    data_get = data_get_end - data_get_start,
    legend_render = legend_render_end - legend_render_start,
    explore_graph = explore_graph_end - explore_graph_start,
    explore_text = explore_text_end - explore_text_start,
    legend_save = legend_save_end - legend_save_start,
    graph_save = graph_save_end - graph_save_start,
    map_colors = map_colors_end - map_colors_start,
    total = end_time - start_time
  )
  timing <- lapply(timing, as.numeric)

  return(list(
    legend = base64enc::base64encode(legend_file),
    graph = base64enc::base64encode(graph_file),
    text = text,
    map_colors = map_colors,
    timing = timing
  ))
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
