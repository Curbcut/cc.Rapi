# Initialize future plan
future::plan("multisession")

# Initialize database pool
db_pool <- db_connection()

# Ensure the pool is closed when the R session ends
on.exit(pool::poolClose(db_pool), add = TRUE)


#* Echo back the input
#* @param var_left etc
#* @param var_right etc
#* @param scale etc
#* @param region etc
#* @param time etc
#* @param select_id etc
#* @get /echo
function(var_left, var_right = " ", scale, region = NULL, time, select_id,
         lang = NULL, schemas = list(var_left = list(time = time), var_right = list(time = time))) {
  promises::future_promise({
    context(var_left = var_left, var_right = var_right, scale = scale,
            region = region, time = time, select_id = select_id,
            lang = lang, schemas = schemas)
  })
}
