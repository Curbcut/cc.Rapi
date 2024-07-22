
db_pool <- db_connection()

# Loop over all possible combinations for ALP (bivar)
page <- db_get(select = c("dates", "var_right"),
               from = "modules",
               where = list(id = "alp"),
               schema = "mtl")

z <- lapply(page$var_right[[1]], \(var_right) {
  lapply(page$dates[[1]], \(date) {
    tryCatch(api_context(var_left = "alp", var_right = var_right,
                         scale = "CT", region = "CMA", time = date,
                         select_id = NA, lang = NULL,
                         zoom_levels = c("CSD", "CT", "DA")),
             error = \(e) {
               stop(sprintf("Error at %s in %s", var_right, date))
             })
  })
})
rm(z)


var_right <- "housing_tenant"
date <- 2001
api_context(var_left = "alp", var_right = var_right,
            scale = "CT", region = "CMA", time = date,
            select_id = NA, lang = NULL,
            zoom_levels = c("CSD", "CT", "DA"))


# Loop over all possible left_variable for housing
page <- db_get(select = c("dates", "var_left"),
               from = "modules",
               where = list(id = "housing"),
               schema = "mtl")

z <- lapply(page$var_left[[1]], \(var_left) {
  lapply(page$dates[[1]], \(date) {
    tryCatch(api_context(var_left = var_left, var_right = " ",
                         scale = "CT", region = "city", time = date,
                         select_id = NA, lang = NULL,
                         zoom_levels = c("CSD", "CT", "DA")),
             error = \(e) {
               stop(sprintf("Error at %s in %s", var_right, date))
             })
  })
})
rm(z)

