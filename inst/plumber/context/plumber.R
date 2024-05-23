# # Initialize future plan
# future::plan("multisession")

# Initialize database pool
db_pool <<- cc.Rapi::db_connection()

# Enable CORS Filtering
#' @filter cors
cors <- function(req, res) {
  safe_domains <- c("http://localhost:3000",
                    "https://main.d1siyubu8xsn5n.amplifyapp.com",
                    "https://productiondomain.com")

  if (any(grepl(pattern = paste0(safe_domains,collapse="|"), req$HTTP_REFERER,ignore.case=T))) {
    res$setHeader("Access-Control-Allow-Origin", sub("/$","",req$HTTP_REFERER)) #Have to remove last slash, for some reason

    if (req$REQUEST_METHOD == "OPTIONS") {
      res$setHeader("Access-Control-Allow-Methods","GET,HEAD,PUT,PATCH,POST,DELETE") #This is how node.js does it
      res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
      res$status <- 200
      return(list())
    } else {
      plumber::forward()
    }
  } else {
    plumber::forward()
  }
}

#* Send context (legend, graph, text)
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

  # promises::future_promise({
  tryCatch({
    # Just make sure the pool is in the environment
    # db_pool

    # Execute the query
    cc.Rapi::context(var_left = var_left, var_right = var_right, scale = scale,
                     region = region, time = time, select_id = select_id,
                     top_scale = top_scale, lang = lang, schemas = schemas)

  }, error = function(e) {
    # Handle individual query error
    list(error = paste("500 - Internal server error:", e$message))
  })
  # })
}

#* Health check
#* @get /
#* @response 200 A simple health check response.
function(res) {
  res$status <- 200
  list(message = "Welcome to the Curbcut Rapi")
}

