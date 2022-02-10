library(licloc)

testthat::test_that("licloc works", {
  licloc.set_log_level(logger::INFO)
  logger::log_appender(logger::appender_stdout)
  output <- capture.output({
    lic("Test")
    loc()
  })

  testthat::expect_true(any(stringr::str_detect(output, "Starting")))
  testthat::expect_true(any(stringr::str_detect(output, "elapsed")))
})

testthat::test_that("licloc can log to a file", {
  t <- tempfile()
  licloc.set_log_level(logger::INFO)
  logger::log_appender(logger::appender_file(t))
  lic("Test")
  loc()
  output <- readLines(t)
  testthat::expect_true(any(stringr::str_detect(output, "INFO")))
  testthat::expect_true(any(stringr::str_detect(output, "Starting")))
  testthat::expect_true(any(stringr::str_detect(output, "elapsed")))
})

testthat::test_that("licloc resets indent when set_log_level is called", {
  licloc.set_indent(100)
  testthat::expect_equal(licloc.get_indent(), 100)
  licloc.set_log_level(logger::INFO)
  testthat::expect_equal(licloc.get_indent(), -1)
})

testthat::test_that("licloc can be told not to reset indent when set_log_level is called", {
  licloc.set_indent(100)
  testthat::expect_equal(licloc.get_indent(), 100)
  licloc.set_log_level(logger::INFO, reset_indent = FALSE)
  testthat::expect_equal(licloc.get_indent(), 100)
  licloc.set_indent(-1)
})
