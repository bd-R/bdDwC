context("Darwinizer functions")

test_that("Darwinizer", {
  # With given Darwin Cloud version
  # Sample data should return specific result
  if (bdDwC:::data_darwin_cloud$date == "2018-08-20") {
    # Test provided data
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

    # Test identical match
    result <- darwinize_names(
      data_user = data.frame(year = 1, DIA = 1),
      data_dwc = bdDwC:::data_darwin_cloud$data
    )
    expect_equal(length(unique(result$match_type)), 2)
    # Number of records darwinized
    expect_equal(nrow(result), 2)
    # Number of columns
    expect_equal(ncol(result), 3)
  }

  # Test what happens when all records have identical match
  result <- darwinize_names(
    data_user = data.frame(year = 1, day = 1),
    data_dwc = data.frame(fieldname = letters[1:2], standard = c("year", "day"))
  )
  # Darwinized type
  expect_equal(unique(result$match_type), "Identical")
  # Number of records darwinized
  expect_equal(nrow(result), 2)
  # Number of columns
  expect_equal(ncol(result), 3)

  # Test no lower case matches
  result <- darwinize_names(
    data_user = data.frame(year = 1, day = 1),
    data_dwc = data.frame(fieldname = letters[1:2], standard = LETTERS[1:2])
  )
  expect_equal(nrow(result), 0)
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
  # No renaming could be made
  result <- rename_user_data(mtcars, input)
  expect_true(is.null(result))
})

test_that("Linking names", {
  # Expect wrong input
  expect_error(bdDwC:::link_old_new(mtcars))
  expect_error(bdDwC:::link_old_new(data.frame()))
  expect_error(bdDwC:::link_old_new(matrix(1:10)))
  expect_error(bdDwC:::link_old_new(matrix(1:10)))
  # Expect correct input/output
  input <- darwinize_names(
    data_user = bdDwC:::data_reptiles,
    data_dwc = bdDwC:::data_darwin_cloud$data
  )
  expect_equal(length(bdDwC:::link_old_new(input)), nrow(input))
  # Test wrong linker
  expect_error(bdDwC:::link_old_new(input, 0))
  expect_error(bdDwC:::link_old_new(input, NA))
})

context("Dictionary functions")

test_that("Download Cloud Data", {
  # Test for wrong path to Cloud Data
  expect_error(download_cloud_data(1, 2, 3, 4))
  expect_warning(download_cloud_data("https://"))
  expect_silent(download_cloud_data())
  # Test for wrong columns
  expect_silent(download_cloud_data(
    column_field = "fieldname", column_stand = "standard"
  ))
  expect_error(download_cloud_data(
    column_field = "fieldname", column_stand = "fieldname"
  ))
  expect_error(download_cloud_data(
    column_field = "fieldname", column_stand = c("fieldname", "fieldname")
  ))
  expect_error(download_cloud_data(
    column_field = "fieldname", column_stand = 1
  ))
  expect_error(download_cloud_data(
    column_field = "fieldname", column_stand = "A"
  ))
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