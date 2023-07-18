## TEST CLIMATE FILE
##
## Given a file and a list of expected columns this will test to make
## sure our data looks good. Returns TRUE if all expected columns are
## present in the file, otherwise returns FALSE. 

test_climate_file <- function(file_path, expected_columns) {
  df <- fread(file_path)
  missing_columns <- setdiff(expected_columns, colnames(df))
  
  if (length(missing_columns) > 0) {
    message("Missing columns in file: ", file_path)
    return(FALSE)
  }
  
  return(TRUE)
}
