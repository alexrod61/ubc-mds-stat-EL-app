test = list(
  name = "Q2.11",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      failure_message = "pred_CH_purchase should be a vector",
      code = {
        testthat::expect_true("numeric" %in% class(pred_CH_purchase))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "wrong predicted probability",
      code = {
        testthat::expect_equal(digest(round(pred_CH_purchase, 2)), "8be870677916d7b455552a0366ee1502")
      }
    )
  )
)