test_that("year is a valid year", {
    expect_error(get_climatedata(place="squamish", year="A")) # year should be numeric
    expect_error(get_climatedata(place="squamish", year=TRUE)) # year should be numeric
    expect_error(get_climatedata(place="squamish", year=1839)) # year should be between 1840 and present 
    expect_error(get_climatedata(place="squamish", year=999)) # year should be between 1840 and present
    expect_error(get_climatedata(place="squamish", year=(as.integer(format(Sys.Date(), "%Y")) + 1 ))) # year should be between 1840 and present
})

test_that("interval is a valid format", {

    expect_error(get_climatedata(place="squamish", year=2014,interval=666)) # interval should be a string
    expect_error(get_climatedata(place="squamish", year=2014,interval=666,"garbage"))
})
