# Initialize future plan
future::plan("multisession")

# Initialize database pool
db_pool <<- cc.Rapi::db_connection()

#* Echo back the input
#* @param var_left etc
#* @param var_right etc
#* @param scale etc
#* @param region etc
#* @param time etc
#* @param select_id etc
#* @get /context
function(var_left, var_right = " ", scale, region, time, select_id = NA,
         lang = NULL,
         schemas = jsonlite::toJSON(
           list(var_left = list(time = time), var_right = list(time = time)))) {
  schemas <- jsonlite::fromJSON(schemas)

  promises::future_promise({
    # Just make sure the pool is in the environment
    db_pool
    cc.Rapi::context(var_left = var_left, var_right = var_right, scale = scale,
                     region = region, time = time, select_id = select_id,
                     lang = lang, schemas = schemas)
  })
}
