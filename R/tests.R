test_climate_file <- function(data) {

    expected_columns <- c(
        "Station Name",
        "Climate ID",
        "Date/Time"
    )
    
    missing_columns <- setdiff(expected_columns, colnames(data))
    
    if (length(missing_columns) > 0) {
        message("Missing columns in file: ", file_path)
        return(FALSE)
    }
    
    return(TRUE)
}
