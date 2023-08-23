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
#'     be one of: "h", "hourly", "m", "monthly", or "y",
#'     "yearly". Defaults to monthly.
#'
#' @return A single tibble containing all the requested data
#' @export

get_climatedata <- function(place, year, interval) {

    ## CHECK INPUT -----------------------------------------------------
    ##
    ## Here we do the checks in order of fastest to slowest.

    ## Check if the input is a valid year (numeric and within a reasonable range)
    year <- as.numeric(year) # make sure the year is numeric

    if (
        nchar(year) != 4 ||
        year < 1840 ||
        year > as.integer(format(Sys.Date(), "%Y"))
    ) return("Invalid year")

    ## Convert the interval to a timecode
    timecode <- get_timecode(interval)

    ## get_timecode() will either return a number or an error string
    if(!is.numeric(timecode)) return(timecode)
    
    ## Find and check the relevant stations
    locations <- get_station(place)

    if (is.null("locations")) {
        return("Place not found")
    }

    ## DOWNLOAD DATA ---------------------------------------------------
    ##

    ## Loop through all the locations that we have and download each one.
    dat_list <- lapply(seq(nrow(locations)), function(i) {

        # Skip any stations that weren't active this year
        if (locations$first_year[i] > year |
            locations$last_year[i] < year) return(NULL)
        
        url <- build_url(
            locations$station_id[i],
            timecode,
            year
        )
        
        data <- get_file(url)

        return(data)
        
    })

    ## Combine the list of data into a single tibble
    cdata <- dplyr::bind_rows(dat_list)

    return(cdata)
    
    
}

get_file <- function(url) {

    ## First download the file
    file <- dl_csv(url)

    file <- file %>%
        dplyr::rename_all( # remove spaces and special characters
                   ~ str_to_lower(str_replace_all(gsub("[^A-Za-z0-9]", "_", .), "_+", "_")) %>%
                       str_remove_all("^_") %>%
                       str_remove_all("_$")) %>%
        dplyr::mutate(
            latitude_y = as.double(latitude_y),
            longitude_x = as.double(longitude_x),
            climate_id = as.character(climate_id), # some climate_ids have characters in them
            year = as.character(year)
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
get_timecode <- function(interval) {

    
    rows <- tibble::tibble(
                        short = c("h","d","m"),
                        long = c("hourly", "daily", "monthly"),
                        code = 1:3
                    ) %>%
        dplyr::filter(
                   short == interval | long == interval
               )

    if(nrow(rows) == 0) return("Invalid interval")

    output <- rows %>%
        dplyr::pull(
                   code
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
                       station_id == place | climate_id == place
                   )
        
        if(nrow(rows) > 0) return(rows)
    } else if (is.character(place)) {
        rows <- stations %>%
            dplyr::filter(
                       stringr::str_detect(
                                    name, stringr::regex(place, ignore_case = TRUE)
                                )|
                       stringr::str_detect(
                                    province, stringr::regex(place, ignore_case = TRUE)
                                )
                   )

        if(nrow(rows) > 0) return(rows)
    }

    return(NULL)

}
     

#' Downloads a csv from the internet
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
            if (test_climate_file(dld) == TRUE) {

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
