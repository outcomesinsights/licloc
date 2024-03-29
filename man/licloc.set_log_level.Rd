% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/licloc.R
\name{licloc.set_log_level}
\alias{licloc.set_log_level}
\title{Sets the level at which tictoc messages should be logged}
\usage{
licloc.set_log_level(level = logger::DEBUG, reset_indent = TRUE)
}
\arguments{
\item{level}{The logging threshold which tictoc messages should use}

\item{reset_indent}{By default, resets the indentation of logging.
Set to FALSE to preserve the existing indentation
This was done as a convenience because a process that dies does not
reset the indentation of licloc but most programs will set the log
level as part of initialization}
}
\description{
Setting this value means that tictoc will use logger as opposed to printing
to the screen directly
}
