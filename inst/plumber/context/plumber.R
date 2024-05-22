# Initialize future plan
future::plan("multisession")

# Initialize database pool
db_pool <<- cc.Rapi::db_connection()

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* Echo back the input
#* @param var_left etc
#* @param var_right etc
#* @param scale etc
#* @param region etc
#* @param time etc
#* @param select_id etc
#* @get /context
function(var_left, var_right = " ", scale, region, time, select_id = NA,
         lang = NULL, top_scale,
         schemas = jsonlite::toJSON(
           list(var_left = list(time = time), var_right = list(time = time)))) {
  schemas <- jsonlite::fromJSON(schemas)

  promises::future_promise({
    tryCatch({
      # Just make sure the pool is in the environment
      db_pool

      # Execute the query
      cc.Rapi::context(var_left = var_left, var_right = var_right, scale = scale,
                       region = region, time = time, select_id = select_id,
                       top_scale = top_scale, lang = lang, schemas = schemas)

    }, error = function(e) {
      # Handle individual query error
      list(error = paste("500 - Internal server error:", e$message))
    })
  })
}

