# # Initialize future plan
# future::plan("multisession")

# Initialize database pool
db_pool <<- cc.Rapi::db_connection()

#* Send context (legend, graph, text)
#* @param var_left etc
#* @param var_right etc
#* @param scale etc
#* @param region etc
#* @param time etc
#* @param select_id etc
#* @get /context
function(var_left, var_right = " ", scale, region, time, select_id = NA,
         lang = NULL, zoom_levels,
         schemas = jsonlite::toJSON(
           list(var_left = list(time = time), var_right = list(time = time)))) {
  schemas <- jsonlite::fromJSON(schemas)
  zoom_levels <- jsonlite::fromJSON(zoom_levels)

  tryCatch({
    cc.Rapi::context(var_left = var_left, var_right = var_right, scale = scale,
                     region = region, time = time, select_id = select_id,
                     zoom_levels = zoom_levels, lang = lang, schemas = schemas)
  }, error = function(e) {
    # Handle individual query error
    list(error = paste("500 - Internal server error:", e$message))
  })
}

#* Health check
#* @get /
#* @response 200 A simple health check response.
function(res) {
  res$status <- 200
  list(message = "Welcome to the Curbcut Rapi")
}

