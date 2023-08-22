#' Used to generate the station data for inclusion in the
#' package. This code is not included in package bundles.
#'
#'
update_stationdata <- function() {

    library(magrittr) # This is ok here as we are not including this
                      # in the package but running it in user space
                      # prior to bundling.
    
    stn_url <-
        "https://collaboration.cmc.ec.gc.ca/cmc/climate/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv"

    ## Define the columns that are useful to us
    cols <- c(
        "Name",
        "Province",
        "Climate ID",
        "Station ID",
        "Latitude (Decimal Degrees)",
        "Longitude (Decimal Degrees)",
        "First Year",
        "Last Year"
    )
    
    stations <- readr::read_csv(
                           stn_url,
                           skip = 3,
                           show_col_types = FALSE
                       ) %>%
        dplyr::select(
            all_of(cols)
        ) %>%
        dplyr::rename(
                   name = "Name",
                   province = "Province",
                   climate_id = "Climate ID",
                   station_id = "Station ID",
                   latitude = "Latitude (Decimal Degrees)",
                   longitude = "Longitude (Decimal Degrees)",
                   first_year = "First Year",
                   last_year = "Last Year"
               )

    usethis::use_data(stations, internal = TRUE)
        
}
