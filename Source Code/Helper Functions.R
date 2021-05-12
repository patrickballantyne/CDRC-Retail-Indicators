## Helper Functions 

get_drivetimes <- function(x) {
  
  if(x$Classification == "Regional Centre" | x$Classification == "Out of Town Shopping Centres") {
    iso <- isoline(x, range = (20 * 60), range_type = "time", transport_mode = "car")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "20 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  if(x$Classification == "Major Town Centre" | x$Classification == "Retail Park") {
    iso <- isoline(x, range = (15 * 60), range_type = "time", transport_mode = "car")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "15 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  if(x$Classification == "Town Centre") {
    iso <- isoline(x, range = (10 * 60), range_type = "time", transport_mode = "car")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "10 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  if(x$Classification == "Market Town" | x$Classification == "District Centre" | x$Classification == "Local Centre") {
    iso <- isoline(x, range = (5 * 60), range_type = "time", transport_mode = "car")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "5 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  return(iso)
}


