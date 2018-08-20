context("Main Functions")
library(bdDwC)

test_that("Darwinizer", {
    # With given Darwin Cloud version
    # Sample data should return specific result
    if (bdDwC:::dataDarwinCloud$date == "2018-08-20") {
        result <- darwinizeNames(dataUser = bdDwC:::dataReptiles,
                                 dataDWC   = bdDwC:::dataDarwinCloud$data)

        # Darwinized type
        expect_equal(unique(result$matchType), "Darwinized")
        # Number of records darwinized
        expect_equal(nrow(result), 4)
        # Number of columns
        expect_equal(ncol(result), 3)
    }
})