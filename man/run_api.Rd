% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_misc.R
\name{run_api}
\alias{run_api}
\title{Run Plumber API}
\usage{
run_api(host = "127.0.0.1", port = 8000)
}
\arguments{
\item{host}{<\code{character}> Which host to start the API? Defaults to 127.0.0.1
for localhost. Must be "0.0.0.0" in docker.}

\item{port}{<\code{numeric}> The port on which the API should be listening. Defaults
to 8000.}
}
\value{
This function does not return a value. It starts the Plumber API server.
}
\description{
This function initializes and runs the Plumber API for the \code{cc.Rapi} package.
It also sets up a hook to close the database pool when the API is stopped.
}
