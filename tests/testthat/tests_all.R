context("Main Functions")
library(bdDwC)
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
    expect_equal(nrow(result), 4)
    # Number of columns
    expect_equal(ncol(result), 3)
  }
})
