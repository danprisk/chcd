#' Build a valid URL for grabbing a file from CHCD
#'
#' @param station_id The ID of the station to be downloaded
#' @param timecode The numeric timecode to be downloaded
#' @param year The year of data to be loaded
#' @param month The month of the year to download data. Setting to NA
#'     gets all months.
#'
#' @return A URL string

build_url <- function(station_id, timecode, year, month = NA) {

    ## This is a little ungainly, but basically we try and account for
    ## the function being given a text based interval such as "month"
    ## here.
    if(!is.numeric(timecode)) timecode <- get_timecode(timecode)
    if(!is.numeric(timecode)) return(timecode)

    if (is.na(month)) month <- 1

    url <- paste0(
        "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=",
        station_id,
        "&Year=",
        year,
        "&Month=",
        month,
        "&Day=14&timeframe=",
        timecode,
        "&submit=Download+Data")

    return(url)
  
}

#' Get historical climate data for the specified time and place
#'
#' @param place The place that data should be downloaded for. This can
#'     be a climate station ID, or a text place name.
#' @param year The year, or years, to get data for. Can be either a
#'     single numeric year or a list of years.
#' @param interval The interval that data should be returned for. Must
#'     be one of: "h", "hourly", "d", "daily", "m", "monthly". Defaults
#'     to monthly.
#'
#' @return A single tibble containing all the requested data
#' @export

get_climatedata <- function(place, years, interval) {

    ## CHECK INPUT -----------------------------------------------------
    ##
    ## Here we do the checks in order of fastest to slowest.

    ## Check missing data
    if(missing(place)) stop("place is required")
    if(missing(years)) stop("year is required")
    if(missing(interval)) interval <- "m"
    
    ## Check if the input is a valid year (numeric and within a reasonable range)
    if(!is.numeric(place) && !is.character(place)) stop("place must be numeric or string")
    if(!is.numeric(years)) stop("year must be numeric") # make sure the year is numeric
    if(!is.character(interval)) stop("interval must be a string")

    ## Check that all years supplied are in the correct format
    lapply(1:length(years), function(i) {
        if (
            nchar(years[i]) != 4 ||
            years[i] < 1840 ||
            years[i] > as.integer(format(Sys.Date(), "%Y"))
        ) stop("year must be between 1840 and present")
    })

    ## Convert the interval to a timecode
    timecode <- get_timecode(interval)

    ## get_timecode() will either return a number or an error string
    if(!is.numeric(timecode)) return(timecode)
    
    ## Find and check the relevant stations
    locations <- lapply(place, get_station) %>%
        dplyr::bind_rows()

    if (is.null("locations")) {
        return("Place not found")
    }

    ## DOWNLOAD DATA ---------------------------------------------------
    ##

    ## Create a list containing each combination of year & location in
    ## order to download the relevant file.
    combinations <- expand.grid(station_id = locations$station_id, year = years)
    
    ## Loop through all the locations that we have and download each one.
    dat_list <- lapply(1:nrow(combinations), function(i) {

        # Assign the data for the current location for easy ref
        loc <- locations[locations$station_id == combinations$station_id[i],]
        
        # Skip this iteration if this station wasn't active for this year
        if (loc$first_year > combinations$year[i] |
            loc$last_year < combinations$year[i]) return(NA)
        
        url <- build_url(
            combinations$station_id[i],
            timecode,
            combinations$year[i]
        )
        
        data <- get_file(url)

        return(data)
        
    })

    ## filter out stations that don't have data for this year
    cdata <- dat_list[!is.na(dat_list)] %>%
        dplyr::bind_rows() %>%
        # this filters out records that are not within the requested
        # years as monthly data returns all data for the requested station.
        dplyr::filter(year %in% years)

    return(cdata)
    
    
}

get_file <- function(url) {

    ## First download the file
    file <- dl_csv(url)

    file <- file %>%
        dplyr::rename_all( # remove spaces and special characters
                   ~ stringr::str_to_lower(
                                  stringr::str_replace_all(
                                               gsub("[^A-Za-z0-9]", "_", .),
                                               "_+",
                                               "_"
                                           )
                              ) %>%
                       stringr::str_remove_all("^_") %>%
                       stringr::str_remove_all("_$")) %>%
        dplyr::mutate(
            latitude_y = as.double(.data$latitude_y),
            longitude_x = as.double(.data$longitude_x),
            climate_id = as.character(.data$climate_id), # some climate_ids have characters in them
            year = as.character(.data$year),
            dplyr::across(  # sets all flags to character
                          dplyr::ends_with("_flag"),
                          as.character
                   ),
            dplyr::across( # sets all temperatures to numeric
                       dplyr::ends_with("_temp_c"),
                       as.numeric
                   ),
            dplyr::across( # sets all precip measurements to numeric
                       dplyr::ends_with("_mm"),
                       as.numeric
                   ),
             dplyr::across( # sets all precip measurements to numeric
                       dplyr::ends_with("_cm"),
                       as.numeric
                   )

            ## This section needs significant work to clean the files.
            
            )

    return(file)
    
    
}


#' Converts a text based interval into a CHCD timecode
#' 
#' @param interval A text based interval. Must be one of: "h",
#'     "hourly", "d", "daily", or "m", "monthly".
#'
#' @return A numeric code that can be used in a CHCD URL
#'
#' @importFrom rlang .data
#' 
get_timecode <- function(interval) {

    
    rows <- tibble::tibble(
                        short = c("h","d","m"),
                        long = c("hourly", "daily", "monthly"),
                        code = 1:3
                    ) %>%
        dplyr::filter(
                   .data$short == interval | .data$long == interval
               )

    if(nrow(rows) == 0) stop("Invalid interval")
    
    output <- rows %>%
        dplyr::pull(
                   .data$code
               )
    
    return(output)
    
    
}

#' Find climate station or stations from a given place. This also
#' confirms if a given climate station ID is valid.
#'
#' @param place Either a numeric station ID or a place name.
#'
#' @return Returns a tibble containing id, name, and location for all
#'     valid stations corresponding to place. Or NA if none are found.
#' @export

get_station <- function(place) {

    if (is.numeric(place)) {
        rows <- stations %>%
            dplyr::filter(
                       .data$station_id == place | .data$climate_id == place
                   )
        
        if(nrow(rows) > 0) return(rows)
    } else if (is.character(place)) {
        rows <- stations %>%
            dplyr::filter(
                       stringr::str_detect(
                                    .data$name, stringr::regex(place, ignore_case = TRUE)
                                )|
                       stringr::str_detect(
                                    .data$province, stringr::regex(place, ignore_case = TRUE)
                                )
                   )

        if(nrow(rows) > 0) return(rows)
    }

    return(NULL)

}

#' Finds climate stations near to a given location.
#'
#' @param longitude The longitude of the point
#' @param latitude The latitude of the point
#' @param distance The distance in KM from the point to pull in stations
#'
#' @return A tibble containing the id, name, and locations for all
#'     valid stations within the radius of distance from point.
#' @export

stations_near <- function(longitude, latitude, distance) {

    
    
}
     

#' Downloads a csv from the internet
#'
#' @param url A string containing the URL of the CSV to download
#'
#' @return The content of the downloaded CSV 
dl_csv <- function(url) {

    max_retries <- 3
    retry_count <- 0  # Initialize the retry count
    download_success <- FALSE

    ## Here we retry the download up to max_retries number of
    ## times. Often we are hitting temporary connection errors with
    ## downloads that crash the whole process without this and can be
    ## resolved by simply waiting a few seconds and trying again.
    while (retry_count < max_retries) {
        retry_count <- retry_count + 1
        
        tryCatch({
            ## Attempt to download the file
            dld <- readr::read_csv(
                              url,
                              show_col_types = FALSE
                          )

            ## Now check the file we downloaded to see if it looks correct
            if (test_climatefile(dld) == TRUE) {

                download_success <- TRUE
                
                ## Break the while loop if download is successful
                break
                
            } else {
                rm(dld)
            }
            
        }, error = function(err) {

            ## Wait 2 seconds and try again
            Sys.sleep(2)
        })
    }

    if (download_success == TRUE) return(dld)
    else return(NULL)
    
}

#' Given a file and a list of expected columns this will test to make
#' sure our data looks good. Returns TRUE if all expected columns are
#' present in the file, otherwise returns FALSE.
#'
#' @param file A file downloaded from CHCD for testing
#'
#' @return TRUE if file contains expected columns, FALSE otherwise.

test_climatefile <- function(file) {

    ## These columns are in all formats, so provide a rough check that
    ## our data looks correct
    expected_columns <- c(
        "Station Name",
        "Climate ID"
    )
    
    missing_columns <- setdiff(expected_columns, colnames(file))
    
    if (length(missing_columns) > 0) {
        message("Missing columns in download file")
        return(FALSE)
    }

    return(TRUE)
}
