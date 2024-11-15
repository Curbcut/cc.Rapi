# var_left <- c("alp")
# var_right = " "
# scale <- "CT"
# region <- "CMA"
# time <- c(2011, 2021)
# select_id <- NA
# schema <- "mtl"
# lang <- NULL
# schemas = list(var_left = list(time = time), var_right = list(time = time))
# zoom_levels <- c("boroughCSD", "CT", "DA")
# db_pool <- db_connection()
# font_family <-  "acidgrotesk-book"


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
#' @param zoom_levels <`character vector`>
#' @param schemas <`list`>
#'
#' @return A list containing three components.
#' @export
api_context <- function(var_left, var_right = " ", scale, region = NULL, time, select_id,
                    lang = NULL, zoom_levels,
                    schemas = list(var_left = list(time = time),
                                   var_right = list(time = time))) {
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

  # Update the var_right schema if it's needed.
  if (var_right != " ") {
    # By default, it will use the default break variable
    brk_v <- variables$breaks_var[variables$var_code == var_right][[1]]
    sch <- variables$schema[variables$var_code == var_right][[1]]
    sch <- sch[names(sch) != "time"]
    if (length(sch) > 0) {
      for (i in names(sch)) {
        schemas$var_right[[i]] <- s_extract(sch[[i]], brk_v)
        schemas$var_right[[i]] <- gsub("^_|_$", "", schemas$var_right[[i]])
      }
    }
  }

  # Timing vars_build
  vars_build_start <- Sys.time()
  vars <- vars_build(var_left, var_right, scale, time, variables = variables)
  vars_build_end <- Sys.time()

  time_formatted <- vars$time
  vars <- vars$vars

  # Timing data_get
  data_get_start <- Sys.time()
  data <- data_get(vars, scale, region, variables = variables, time = time_formatted,
                   schemas = schemas)
  data_get_end <- Sys.time()

  # Timing legend_render
  legend_render_start <- Sys.time()
  legend <- legend_render(vars = vars, scale = scale, data = data, variables = variables,
                          time = time_formatted)
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

  # # Map colors
  # map_colors_start <- Sys.time()
  # map_colors <- data_get_colours(vars = vars, region = region, time = time_formatted,
  #                                zoom_levels = zoom_levels, variables = variables,
  #                                schemas = schemas)
  # map_colors_end <- Sys.time()

  # Timing legend file save
  legend_save_start <- Sys.time()
  legend_file <- tempfile(fileext = ".png")
  width_in <- 260
  height_in <- if ("bivar" %in% class(vars)) 160 else 60
  ggplot2::ggsave(legend_file, plot = legend, device = "png", width = width_in,
                  height = height_in, units = "px", dpi = 125)
  legend_save_end <- Sys.time()

  # Timing graph file save
  graph_save_start <- Sys.time()
  graph_file <- tempfile(fileext = ".png")
  width_in <- 250
  height_in <- 150
  ggplot2::ggsave(graph_file, plot = graph, device = "png", width = width_in,
                  height = height_in, units = "px", dpi = 125)
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
    # map_colors = map_colors_end - map_colors_start,
    total = end_time - start_time
  )
  timing <- lapply(timing, as.numeric)

  return(list(
    legend = base64enc::base64encode(legend_file),
    graph = base64enc::base64encode(graph_file),
    text = text,
    # map_colors = map_colors,
    timing = timing
  ))
}
