# var_left <- "housing_tenant"
# var_right = " "
# scale <- "DA"
# region <- "CMA"
# region = NULL
# time <- 2021
# select_id <- NA
# schema <- "mtl"
# lang <- NULL
#
# db_pool <- db_connection()

context <- function(conn, var_left, var_right = " ", scale, region = NULL, time, select_id,
                    lang = NULL, schemas = list(var_left = list(time = time), var_right = list(time = time))) {

  variables <- db_get(select = "*", from = "variables", schema = "mtl",
                      where = list(var_code = c(var_left, var_right, "private_households")))#TKTK PRIVATE HOUSEHOLDS (just because it's parent vector!! must be dynamic)


  # TKTK
  variables$schema <- list(time = "_\\d{4}$")

  vars <- vars_build(var_left, var_right = " ", scale, time, variables = variables)
  time_formatted <- vars$time
  vars <- vars$vars
  data <- data_get(vars, scale, region, variables = variables)

  legend <- legend_render(vars = vars, scale = scale, data = data, variables = variables)
  graph <- explore_graph(vars = vars, select_id = select_id, scale = scale, data = data,
                         time = time_formatted, schemas = schemas, lang = lang, variables = variables)
  text <- explore_text(vars = vars, select_id = select_id, scale = scale, region = region,
                       data = data, time = time_formatted, schemas = schemas, lang = lang,
                       variables = variables)

  return(list(legend = legend, graph = graph, text = text))
}
