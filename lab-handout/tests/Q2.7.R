test = list(
  name = "Q2.7",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      failure_message = "the model should be assigned to bin_log_model_2",
      code = {
        testthat::expect_true(exists("bin_log_model_2"))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "the correct fitting function is not being used",
      code = {
        testthat::expect_true("glm" %in% class(bin_log_model_2))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "check the formula and data arguments in the fitting function",
      code = {
        testthat::expect_equal(digest(round(sum(bin_log_model_2$coefficients), 2)), "6e76d62738b6d511f99f4c042626a94d")
      }
    )
  )
)