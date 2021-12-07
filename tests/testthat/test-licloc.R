library(licloc)

testthat::test_that("licloc works", {
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
