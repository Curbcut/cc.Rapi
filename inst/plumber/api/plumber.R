# # Initialize future plan
# future::plan("multisession")

# Initialize database pool
db_pool <<- cc.Rapi::db_connection()

# Enable CORS Filtering
#' @filter cors
cors <- function(req, res) {

  # This really works!
  safe_domains <- c("http://localhost:3000",
                    "http://localhost:8000",
                    "https://main.d1siyubu8xsn5n.amplifyapp.com", # rapi-frontend
                    "https://main.dlfdwv74s9cmj.amplifyapp.com/", # summercs2024
                    "https://v3.curbcut.ca/",
                    "https://cho.curbcut.ca/",
                    "\\.curbcut\\.ca$",
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
#* @param breaks etc
#* @param schema etc
#* @get /context
function(var_left, var_right = " ", scale, region, time, select_id = paste0("\"NA\""),
         lang = NULL, zoom_levels, breaks, schema) {
  breaks <- jsonlite::fromJSON(breaks)
  time <- jsonlite::fromJSON(time)
  zoom_levels <- jsonlite::fromJSON(zoom_levels)
  select_id <- jsonlite::fromJSON(select_id)
  if (is.na(select_id) | select_id == "NA") select_id <- NA
  schemas <- list(var_left = list(time = time), var_right = list(time = time))

  tryCatch({
    cc.Rapi::api_context(var_left = var_left, var_right = var_right, scale = scale,
                         region = region, time = time, select_id = select_id,
                         zoom_levels = zoom_levels, lang = lang, schemas = schemas,
                         breaks = breaks, schema = schema)
  }, error = function(e) {
    # Handle individual query error
    list(error = paste("500 - Internal server error:", e$message))
  })
}

#* Send context (legend, graph, text)
#* @param var_left etc
#* @param var_right etc
#* @param zoom_levels etc
#* @param region etc
#* @get /breaks
function(var_left, var_right = " ", zoom_levels, time = NULL, region = NULL) {
  time <- jsonlite::fromJSON(time)
  zoom_levels <- jsonlite::fromJSON(zoom_levels)
  tryCatch({
    cc.Rapi::api_breaks(var_left = var_left, var_right = var_right,
                        zoom_levels = zoom_levels, region = region, time = time)
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
