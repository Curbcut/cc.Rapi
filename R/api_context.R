var_left <- "housing_tenant"
var_right = " "
scale <- "DA"
region = NULL
time <- 2021
select_id <- NA
schema <- "mtl"

context <- function(conn, var_left, var_right = " ", scale, region = NULL, time, select_id) {

  variables <- db_get(select = "*", from = "variables", schema = schema,
                      where = list(var_code = c(var_left, var_right)))
  variables$schema <- list(time = "_\\d{4}$")
  variables$breaks_var <- NA

  vars <- vars_build(var_left, var_right = " ", scale, time, variables = variables)
  time_formatted <- vars$time
  vars <- vars$vars
  data <- data_get(vars, scale, region, variables = variables)

  legend_render(vars = vars, scale = scale, data = data, variables = variables)

}
