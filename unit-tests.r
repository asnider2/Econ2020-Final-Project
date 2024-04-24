library(testthat)

# Test clean_data function
test_that("clean_data function correctly cleans and renames data", {
  # Create sample data
  sample_data <- data.frame(date = c("2022-01-01", "2022-01-02", "2022-01-03"),
                            value = c("10", "20", NA))

  # Apply clean_data function
  cleaned_data <- clean_data(sample_data, "test_series")

  # Check if date column is of Date class
  expect_is(cleaned_data$date, "Date")

  # Check if value column is numeric
  expect_is(cleaned_data$test_series_value, "numeric")

  # Check if NA values are removed
  expect_false(anyNA(cleaned_data))
# Check renaming of value column
  expect_true("test_series_value" %in% names(cleaned_data))
})
