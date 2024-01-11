test_that("year is a valid year", {
    expect_error(get_climatedata(place="squamish", year="A"),
                 "year must be numeric")
    expect_error(get_climatedata(place="squamish", year=TRUE),
                 "year must be numeric")
    expect_error(get_climatedata(place="squamish", year=1839),
                 "year must be between 1840 and present")
    expect_error(get_climatedata(place="squamish", year=999),
                 "year must be between 1840 and present")
    expect_error(get_climatedata(place="squamish", year=(as.integer(format(Sys.Date(), "%Y")) + 1 )),
                 "year must be between 1840 and present")
})

test_that("interval is a valid format", {

    expect_error(get_climatedata(place="squamish", year=2014,interval=666),
                 "interval must be a string")
    
})
