env.licloc <- new.env()
assign(".licloc.indent", -1, envir = env.licloc)
assign(".licloc.symbols", "-*+#~$^@!", envir = env.licloc)
assign(".licloc.log_level", NULL, envir = env.licloc)

#' Gets the list of symbols used with nested messages
licloc.get_symbols <- function() {
  get(".licloc.symbols", envir = env.licloc)
}

#' Gets the symbol for a specific indentation level
licloc.get_symbol_at <- function(num) {
  substr(licloc.get_symbols(), num, num)
}

#' Gets the current indentation level
licloc.get_indent <- function() {
  get(".licloc.indent", envir = env.licloc)
}

#' Sets the current indentation level
licloc.set_indent <- function(indent) {
  assign(".licloc.indent", indent, envir = env.licloc)
}

#' Increments the current indentation level
licloc.increment_indent <- function() {
  licloc.set_indent(licloc.get_indent() + 1)
}

#' Decrements the current indentation level
licloc.decrement_indent <- function() {
  licloc.set_indent(licloc.get_indent() - 1)
}

#' Returns a string for left padding based on the current indentation level
#' @returns string used for left padding
licloc.get_indent_string <- function() {
  str <- paste0(rep(" ", licloc.get_indent() * 2), collapse = "")
  str
}

#' Returns a string with the symbol for the current indentation level
licloc.get_symbol_string <- function() {
  paste0(licloc.get_symbol_at(licloc.get_indent() + 1), " ")
}

#' Sets the level at which tictoc messages should be logged
#'
#' Setting this value means that tictoc will use logger as opposed to printing
#' to the screen directly
#'
#' @param level The logging threshold which tictoc messages should use
#' @param reset_indent By default, resets the indentation of logging.
#'   Set to FALSE to preserve the existing indentation
#'   This was done as a convenience because a process that dies does not
#'   reset the indentation of licloc but most programs will set the log
#'   level as part of initialization
#' @export
licloc.set_log_level <- function(level = logger::DEBUG, reset_indent = TRUE) {
  if (reset_indent) {
    licloc.set_indent(-1)
  }

  invisible(assign(".licloc.log_level", level, envir = env.licloc))
}

#' Checks to see if licloc is using a logger
licloc.use_logger <- function() {
  !is.null(licloc.get_log_level())
}

#' Get the logging threshold for licloc
#'
#' @returns the logging threshold for licloc
licloc.get_log_level <- function() {
  get(".licloc.log_level", envir = env.licloc)
}

#' Used to construct licloc's default for calls to tic
msg.lic <- function(tic, msg = NULL) {
  if (is.null(msg) || is.na(msg) || length(msg) == 0) {
    outmsg <-
      paste0(licloc.get_indent_string(),
             licloc.get_symbol_string(),
             "Tic...",
             collapse = "")
  } else {
    outmsg <-
      paste0(
        licloc.get_indent_string(),
        licloc.get_symbol_string(),
        "Starting ",
        msg,
        "...",
        collapse = ""
      )
  }
}

#' Used to construct licloc's default for calls to tocs
msg.loc <- function(tic, toc, msg, ...) {
  if (is.null(msg) || is.na(msg) || length(msg) == 0) {
    outmsg <-
      paste(
        licloc.get_indent_string(),
        licloc.get_symbol_string(),
        round(toc - tic, 3),
        " seconds elapsed",
        sep = ""
      )
  } else {
    outmsg <-
      paste(
        licloc.get_indent_string(),
        licloc.get_symbol_string(),
        msg,
        ": ",
        round(toc - tic, 3),
        " seconds elapsed",
        sep = ""
      )
  }
}

#' Wrapper function for tic
#'
#' Uses nested messages and, if desired, logging over printing to console
#'
#' @param ... Compiled into a message if \code{msg} is NULL
#' @param quiet Passthru to  \code{\link[tictoc]{tic}}
#' @param func.tic Passthru to  \code{\link[tictoc]{tic}}
#' @param msg Passthru to  \code{\link[tictoc]{tic}}
#' @param namespace Passthru to \code{\link[logger]{log_level}}
#' @param .logcall Passthru to \code{\link[logger]{log_level}}
#' @param .topcall Passthru to \code{\link[logger]{log_level}}
#' @param .topenv Passthru to \code{\link[logger]{log_level}}
#' @param level Passthru to \code{\link[logger]{log_level}}
#' @returns Nothing
#' @export
lic <-
  function(...,
           quiet = FALSE,
           func.tic = NULL,
           msg = NULL,
           namespace = NA_character_,
           .logcall = sys.call(),
           .topcall = sys.call(-1),
           .topenv = parent.frame(),
           level = licloc.get_log_level()) {
    licloc.increment_indent()
    if (is.null(func.tic)) {
      func.tic <- msg.lic
    }
    if (is.null(msg)) {
      msg <- .makeMessage(...)
    }

    if (licloc.use_logger()) {
      quiet <- TRUE
    }

    tic <- tictoc::tic(func.tic = func.tic,
                       quiet = quiet,
                       msg = msg)

    if (licloc.use_logger()) {
      logger::log_level(
        level = level,
        func.tic(tic = tic, msg = msg),
        namespace = namespace,
        .logcall = .logcall,
        .topcall = .topcall,
        .topenv = .topenv
      )
    }
  }

#' Wrapper function for toc
#'
#' Uses nested messages and, if desired, logging over printing to console
#'
#' @param ... Compiled into a message if \code{msg} is NULL
#' @param quiet Passthru to  \code{\link[tictoc]{toc}}
#' @param func.tic Passthru to  \code{\link[tictoc]{toc}}
#' @param msg Passthru to  \code{\link[tictoc]{toc}}
#' @param namespace Passthru to \code{\link[logger]{log_level}}
#' @param .logcall Passthru to \code{\link[logger]{log_level}}
#' @param .topcall Passthru to \code{\link[logger]{log_level}}
#' @param .topenv Passthru to \code{\link[logger]{log_level}}
#' @param level Passthru to \code{\link[logger]{log_level}}
#' @returns Nothing
#' @export
loc <-
  function(...,
           log = TRUE,
           quiet = FALSE,
           func.toc = NULL,
           namespace = NA_character_,
           .logcall = sys.call(),
           .topcall = sys.call(-1),
           .topenv = parent.frame(),
           level = licloc.get_log_level()) {
    if (is.null(func.toc)) {
      func.toc <- msg.loc
    }

    if (licloc.use_logger()) {
      quiet <- TRUE
    }

    res <- tictoc::toc(func.toc = func.toc,
                       log = log,
                       quiet = quiet)

    if (licloc.use_logger()) {
      logger::log_level(
        level = level,
        func.toc(
          tic = res[["tic"]],
          toc = res[["toc"]],
          msg = res[["msg"]]
        ),
        namespace = namespace,
        .logcall = .logcall,
        .topcall = .topcall,
        .topenv = .topenv
      )
    }

    licloc.decrement_indent()
  }
