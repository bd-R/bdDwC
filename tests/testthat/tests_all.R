context("Darwinizer functions")

test_that("Darwinizer", {
  # With given Darwin Cloud version
  # Sample data should return specific result
  if (data_darwin_cloud$date == "2018-08-20") {
    # Test provided data
    result <- darwinize_names(
      data_user = data_reptiles,
      data_dwc = data_darwin_cloud$data
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
      data_dwc = data_darwin_cloud$data
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
    data_user = data_reptiles,
    data_dwc = data_darwin_cloud$data
  )
  result <- rename_user_data(data_reptiles, input)
  # Renamed data should have same dimensions
  expect_equal(ncol(result), ncol(data_reptiles))
  expect_equal(nrow(result), nrow(data_reptiles))
  # No renaming could be made
  result <- rename_user_data(mtcars, input)
  expect_true(is.null(result))
})

test_that("Linking Names", {
  # Expect wrong input
  expect_error(link_old_new(mtcars))
  expect_error(link_old_new(data.frame()))
  expect_error(link_old_new(matrix(1:10)))
  expect_error(link_old_new(matrix(1:10)))
  # Expect correct input/output
  input <- darwinize_names(
    data_user = data_reptiles,
    data_dwc = data_darwin_cloud$data
  )
  expect_equal(length(link_old_new(input)), nrow(input))
  # Test wrong linker
  expect_error(link_old_new(input, 0))
  expect_error(link_old_new(input, NA))
})

context("Dictionary functions")

test_that("Download Cloud Data", {
  # Test if works
  expect_silent(download_cloud_data())
  foo <- download_cloud_data()
  expect_equal(ncol(foo), 2)
  expect_gt(nrow(foo), 100)
  expect_equal(colnames(foo), c("fieldname", "standard"))
  expect_true(any(grepl("coordinateUncertaintyInMeters", foo$standard)))
  expect_equal(unique(apply(foo, 2, class)), "character")
  # Test for wrong path to Cloud Data
  expect_error(download_cloud_data(1, 2, 3, 4))
  expect_warning(download_cloud_data("https://"))
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

test_that("Clean Cloud Data", {
  # Test if works
  expect_silent(clean_dwc(data_darwin_cloud$data))
  foo <- clean_dwc(data_darwin_cloud$data)
  expect_equal(ncol(foo), 2)
  expect_equal(colnames(foo), c("fieldname", "standard"))
  expect_lte(nrow(foo), nrow(data_darwin_cloud$data))
  # Test for wrong columns
  expect_error(clean_dwc(mtcars))
  expect_error(clean_dwc(data_darwin_cloud$data, "a"))
  # Test for wrong data
  expect_error(
    clean_dwc(data.frame(fieldname = c(NA, NA), standard = c(NA, NA)))
  )
})

test_that("Download Darwin Core Info", {
  # Test if works
  expect_silent(get_darwin_core_info())
  foo <- get_darwin_core_info()
  expect_equal(ncol(foo), 2)
  expect_true(any(grepl("previousIdentifications", foo$name)))
  expect_equal(colnames(foo), c("name", "definition"))
  # Test for wrong path to Darwin Core
  expect_warning(get_darwin_core_info("http://"))
  expect_equal(nrow(get_darwin_core_info("http://")), 1)
  # Test for wrong regex
  foo <- get_darwin_core_info(regex_term = "foo bar buz this wont occur")
  expect_equal(nrow(foo), 1)
  expect_equal(nrow(foo), 1)
  expect_equal(foo$name, NA)
  # Test for impossible offset to names definition
  foo <- get_darwin_core_info(name_to_def = 1e6)
  expect_equal(class(foo$name), "character")
  expect_gt(length(foo$name), 10)
  expect_equal(length(unique(foo$definition)), 1)
  expect_true(
    all(bdDwC:::data_darwin_core_info == bdDwC:::get_darwin_core_info())
  )
})

context("Tests")
# These tests should either return error (s it's intended or be silent
test_that("User Data", {
  expect_error(test_data_user())
  expect_error(test_data_user(matrix()))
  expect_error(test_data_user(data.frame()))
  foo <- data.frame(1)
  names(foo) <- NULL
  expect_error(test_data_user(foo))
  expect_warning(test_data_user(data.frame(a = 1, a = 2, check.names = FALSE)))
  expect_silent(mtcars)
})

test_that("DWC Dictionary Data", {
  expect_error(test_data_dwc())
  expect_error(test_data_dwc(matrix()))
  expect_error(test_data_dwc(NA))
  expect_error(test_data_dwc(data.frame()))
  expect_silent(test_data_dwc(
    data.frame(foo = 1, bar = 1), "foo", "bar"
  ))
  expect_error(test_data_dwc(
    data.frame(foo = 1, bar = 1), "foo2", "bar2"
  ))
  expect_error(test_data_dwc(
    data.frame(foo = 1, foo = 1, bar = 1, check.names = FALSE), "foo", "bar"
  ))
  expect_error(test_data_dwc(
    data.frame(foo = 1, bar = 1, bar = 1, check.names = FALSE), "foo", "bar"
  ))
})

test_that("Renaming Data", {
  expect_error(test_data_renamed())
  expect_error(test_data_renamed(NA))
  expect_error(test_data_renamed(matrix()))
  expect_error(test_data_renamed(data.frame()))
  expect_error(test_data_renamed(data.frame(1)))
  expect_error(test_data_renamed(data.frame(1, 2)))
  expect_error(test_data_renamed(data.frame(name_old = 1, name_old = 2)))
  expect_error(test_data_renamed(
    data.frame(name_new = 1, name_new = 2, name_old = 3, check.names = FALSE)
  ))
  expect_error(test_data_renamed(
    data.frame(name_old = 1, name_old = 2, name_new = 3, check.names = FALSE)
  ))
  expect_silent(test_data_renamed(data.frame(name_new = 1, name_old = 2)))
})

test_that("Cloud Path", {
  expect_error(test_cloud())
  expect_error(test_cloud(NA))
  expect_error(test_cloud(c("a", "b")))
  expect_silent(test_cloud("valid path to cloud"))
})

test_that("Cloud Path", {
  expect_error(test_columns_cloud())
  expect_error(test_columns_cloud(NA))
  expect_error(test_columns_cloud(c(1, 1)))
  expect_error(test_columns_cloud(c("a", "a")))
  expect_silent(test_columns_cloud(c("a", "b")))
})