env.licloc <- new.env()
assign(".licloc.indent", -1, envir = env.licloc)
assign(".licloc.symbols", "-*+#~$^@!", envir = env.licloc)
assign(".licloc.log_level", NULL, envir = env.licloc)

loc.get_symbols <- function() {
  get(".licloc.symbols", envir = env.licloc)
}

loc.get_symbol_at <- function(num) {
  substr(loc.get_symbols(), num, num)
}

loc.get_indent <- function() {
  get(".licloc.indent", envir = env.licloc)
}

loc.set_indent <- function(indent) {
  assign(".licloc.indent", indent, envir = env.licloc)
}

loc.increment_indent <- function() {
  loc.set_indent(loc.get_indent() + 1)
}

loc.decrement_indent <- function() {
  loc.set_indent(loc.get_indent() - 1)
}

loc.get_indent_string <- function() {
  str <- paste0(rep(" ", loc.get_indent() * 2), collapse = "")
  str
}

loc.get_symbol_string <- function() {
  paste0(loc.get_symbol_at(loc.get_indent() + 1), " ")
}

licloc.set_log_level <- function(level = logger::DEBUG) {
  assign(".licloc.log_level", level, envir = env.licloc)
}

licloc.use_logger <- function() {
  !is.null(licloc.get_log_level())
}

licloc.get_log_level <- function() {
  get(".licloc.log_level", envir = env.licloc)
}

msg.lic <- function(tic, msg = NULL) {
  if (is.null(msg) || is.na(msg) || length(msg) == 0) {
    outmsg <- paste0(loc.get_indent_string(), loc.get_symbol_string(), round(toc - tic, 3), " seconds elapsed")
  } else {
    outmsg <- paste0(loc.get_indent_string(), loc.get_symbol_string(), "Starting ", msg, "...", collapse = "")
  }
}

msg.loc <- function(tic, toc, msg, ...) {
  if (is.null(msg) || is.na(msg) || length(msg) == 0) {
    outmsg <- paste(loc.get_indent_string(), loc.get_symbol_string(), round(toc - tic, 3), " seconds elapsed", sep = "")
  } else {
    outmsg <- paste(loc.get_indent_string(), loc.get_symbol_string(),
                    msg,
                    ": ",
                    round(toc - tic, 3),
                    " seconds elapsed",
                    sep = ""
    )
  }
}

lic <- function(..., quiet = FALSE, func.tic = NULL, msg = NULL, namespace = NA_character_,
                .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame(), level = licloc.get_log_level()) {
  loc.increment_indent()
  if (is.null(func.tic)) {
    func.tic <- msg.lic
  }
  if (is.null(msg)) {
    msg <- .makeMessage(...)
  }

  if (licloc.use_logger()) {
    quiet <- TRUE
  }

  tic <- tictoc::tic(func.tic = func.tic, quiet = quiet, msg = msg)

  if (licloc.use_logger()) {
    logger::log_level(level = level, func.tic(tic = tic, msg = msg), namespace = namespace, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
  }
}

loc <- function(..., log = TRUE, quiet = FALSE, func.toc = NULL, namespace = NA_character_,
                .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame(), level = licloc.get_log_level()) {
  if (is.null(func.toc)) {
    func.toc <- msg.loc
  }

  if (licloc.use_logger()) {
    quiet <- TRUE
  }

  res <- tictoc::toc(func.toc = func.toc, log = log, quiet = quiet)

  if (licloc.use_logger()) {
    logger::log_level(
      level = level,
      func.toc(tic = res[["tic"]], toc = res[["toc"]], msg = res[["msg"]]),
      namespace = namespace,
      .logcall = .logcall,
      .topcall = .topcall,
      .topenv = .topenv
    )
  }

  loc.decrement_indent()
}
