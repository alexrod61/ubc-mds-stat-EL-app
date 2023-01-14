test = list(
  name = "Q2.1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "Purchase be assigned numeric",
      code = {
        testthat::expect_true(is.numeric(OJ$Purchase))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "labelling is not correct",
      code = {
        testthat::expect_equal(digest(sum(OJ$Purchase)), "654a4f520a845cf7f771156c7921a51d")
      }
    )
  )
)