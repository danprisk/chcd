test_that("year is a valid year", {
    ## year should be numeric
    expect_error(get_climatedata(place="squamish", year="A")) 
    expect_error(get_climatedata(place="squamish", year=TRUE))

    ## year should be between 1840 and present
    expect_error(get_climatedata(place="squamish", year=1839)) 
    expect_error(get_climatedata(place="squamish", year=999)) 
    expect_error(get_climatedata(place="squamish", year=(as.integer(format(Sys.Date(), "%Y")) + 1 )))
})

test_that("interval is a valid format", {

    ## interval should be a string
    expect_error(get_climatedata(place="squamish", year=2014,interval=666))

    ## interval string has correct format
    expect_error(get_climatedata(place="squamish", year=2014,"garbage"))
})

test_that("files merge correctly", {

    ## The following are a little slow right now as the involve
    ## downloading data from CHCD. At some point as these tests grow
    ## will likely need to move to holding example data locally to
    ## test more specifically.

    ## files download with different col types for total_rain_flag
    expect_no_error(get_climatedata("squamish",1980,"d"))

    ## files download with different col types for snow_on_grnd_flag
    expect_no_error(get_climatedata("squamish",1990,"d"))
})
    
