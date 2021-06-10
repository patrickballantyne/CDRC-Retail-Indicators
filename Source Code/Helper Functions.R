## Helper Functions 

## Function for delineating drive-time catchments for the centres
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

## Function for delineating walking catchments for the retail centres
get_walking_isolines <- function(x) {
  
  if(x$Classification == "Regional Centre" | x$Classification == "Out of Town Shopping Centres" | x$Classification == "Major Town Centre") {
    iso <- isoline(x, range = (10 * 60), range_type = "time", transport_mode = "pedestrian")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "10 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  if(x$Classification == "Retail Park" | x$Classification == "Town Centre") {
    iso <- isoline(x, range = (10 * 60), range_type = "time", transport_mode = "pedestrian")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "10 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  if(x$Classification == "District Centre" | x$Classification == "Market Town" | x$Classification == "Local Centre") {
    iso <- isoline(x, range = (5 * 60), range_type = "time", transport_mode = "pedestrian")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "5 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  return(iso)
}
  
  

