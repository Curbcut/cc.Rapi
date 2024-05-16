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

context <- function(conn, var_left, var_right = " ", scale, region = NULL, time, select_id,
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

  return(list(legend = legend, graph = graph, text = text))
}
