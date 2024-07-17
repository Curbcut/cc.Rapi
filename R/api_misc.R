#' Run Plumber API
#'
#' This function initializes and runs the Plumber API for the `cc.Rapi` package.
#' It also sets up a hook to close the database pool when the API is stopped.
#'
#' @param host <`character`> Which host to start the API? Defaults to 127.0.0.1
#' for localhost. Must be "0.0.0.0" in docker.
#' @param port <`numeric`> The port on which the API should be listening. Defaults
#' to 8000.
#'
#' @return This function does not return a value. It starts the Plumber API server.
#' @export
run_api <- function(host = "127.0.0.1", port = 8000) {
  api <- plumber::plumb_api("cc.Rapi", "api")

  api$registerHooks(list(
    exit = function() {
      print("database pooling connection disconnected on exit")
      pool::poolClose(get_from_globalenv("db_pool"))
    }
  ))

  plumber::pr_run(api, host = host, port = port)
}
