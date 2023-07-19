#' Test a climate file has the expected columns
#'
#' @param file_path The path to find the file in the local filesystem
#' @param expected_columns A list of expected columns
#'
#' @return A boolean vector
#' @export
#'
#' @examples
#' file_path <- "/path/to/file/file_name.csv"
#' expected_columns <- list("Name", "Data Column 1")
#' test_climate_file(file_path, expected_columns)

test_climate_file <- function(file_path, expected_columns) {
  df <- fread(file_path)
  missing_columns <- setdiff(expected_columns, colnames(df))
  
  if (length(missing_columns) > 0) {
    message("Missing columns in file: ", file_path)
    return(FALSE)
  }
  
  return(TRUE)
}
