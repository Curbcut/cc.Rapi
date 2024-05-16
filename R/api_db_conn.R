#' Establish a Database Connection Pool
#'
#' This function creates a database connection pool using the `pool` package
#' and connects to the prod PostgreSQL database using environment variables for
#' configuration.
#'
#' @return <`DBI::dbPool`> A database connection pool object.
#' @export
db_connection <- function() {
  pool::dbPool(RPostgres::Postgres(),
               dbname = Sys.getenv("DB_NAME"),
               host = Sys.getenv("DB_HOST"),
               port = as.integer(Sys.getenv("DB_PORT")),
               user = Sys.getenv("DB_USER"),
               password = Sys.getenv("DB_PASSWORD"),
               minSize = 1,
               maxSize = Inf,
               idleTimeout = 60*10, # When created, the pool will initialize minSize
               # connections, and keeps them around until they’re requested. If all the
               # idle connections are taken up when another request for a connection
               # comes up, the pool will create a new connection. It’ll keep doing
               # this as needed until it gets to maxSize connections at which point
               # it will error. Any connection that is created when we’re over minSize
               # will have a timer attached to it: from the moment it is returned back
               # to the pool, a countdown of idleTimeout seconds will start
               validationInterval = 60*4,
               validateQuery = "SELECT 1")
}
