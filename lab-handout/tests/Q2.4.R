test = list(
  name = "Q2.4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      failure_message = "the model should be assigned to bin_log_model",
      code = {
        testthat::expect_true(exists("bin_log_model"))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "the correct fitting function is not being used",
      code = {
        testthat::expect_true("glm" %in% class(bin_log_model))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "check the formula and data arguments in the fitting function",
      code = {
        testthat::expect_equal(digest(round(sum(bin_log_model$coefficients), 2)), "2d5a88a9304983ce583b433b7974c08c")
      }
    )
  )
)