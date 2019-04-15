context("Main functions")
test_that("Darwinizer", {
  # With given Darwin Cloud version
  # Sample data should return specific result
  if (bdDwC:::data_darwin_cloud$date == "2018-08-20") {
    result <- darwinize_names(
      data_user = bdDwC:::data_reptiles,
      data_dwc = bdDwC:::data_darwin_cloud$data
    )
    # Darwinized type
    expect_equal(unique(result$match_type), "Darwinized")
    # Number of records darwinized
    expect_equal(nrow(result), 3)
    # Number of columns
    expect_equal(ncol(result), 3)
  }
})

test_that("Rename User Data", {
  input <- darwinize_names(
    data_user = bdDwC:::data_reptiles,
    data_dwc = bdDwC:::data_darwin_cloud$data
  )
  result <- rename_user_data(bdDwC:::data_reptiles, input)
  # Renamed data should have same dimensions
  expect_equal(ncol(result), ncol(bdDwC:::data_reptiles))
  expect_equal(nrow(result), nrow(bdDwC:::data_reptiles))
})

context("Helper functions")
test_that("Download Darwin cloud", {
  result <- download_cloud_data()
  # We should have only two columns
  expect_equal(ncol(result), 2)
  # Number of records
  expect_gt(nrow(result), 10)
  # Columns should be character
  expect_equal(unique(apply(download_cloud_data(), 2, class)), "character")
})