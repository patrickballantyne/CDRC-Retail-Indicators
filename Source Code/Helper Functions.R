## Helper Functions 

## Function for delineating drive-time catchments for the centres
get_drivetimes <- function(x) {
  
  if(x$Classification == "Regional Centre" | x$Classification == "Large Shopping Centre") {
    iso <- isoline(x, range = (20 * 60), range_type = "time", transport_mode = "car")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "20 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  if(x$Classification == "Major Town Centre" | x$Classification == "Large Retail Park" | x$Classification == "Small Shopping Centre") {
    iso <- isoline(x, range = (15 * 60), range_type = "time", transport_mode = "car")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "15 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  if(x$Classification == "Town Centre" | x$Classification == "Small Retail Park") {
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
  if(x$Classification == "Regional Centre" | x$Classification == "Major Town Centre" | x$Classification == "Large Shopping Centre" | x$Classification == "Town Centre") {
    iso <- isoline(x, range = (15 * 60), range_type = "time", transport_mode = "pedestrian")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "15 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  if(x$Classification == "Large Retail Park" | x$Classification == "Small Retail Park" | x$Classification == "Small Shopping Centre") {
    iso <- isoline(x, range = (10 * 60), range_type = "time", transport_mode = "pedestrian")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "10 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  if(x$Classification == "District Centre" | x$Classification == "Market Town" | x$Classification == "Local Centre") {
    iso <- isoline(x, range = (10 * 60), range_type = "time", transport_mode = "pedestrian")
    iso$RC_ID <- x$RC_ID
    iso$RC_Name <- x$RC_Name
    iso$Classification <- x$Classification
    iso$Duration <- "10 mins"
    iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    Sys.sleep(1)
  }
  return(iso)
}
  
## Function for delineating walking catchments for the retail centres
get_boundary_walking <- function(x) {
  
  ## Create a 20m buffer from the boundary
  rc_lines <- st_cast(x, "LINESTRING")
  rc_buffer <- st_buffer(rc_lines, 20)
  
  ## Pull out the H3's for the retail centre
  rc_h3 <- polyfill(x, 11, FALSE)
  rc_h3 <- h3_to_polygon(unlist(rc_h3$h3_polyfillers), simple = FALSE)
  rc_h3 <- st_transform(rc_h3, 27700)
  
  ## Get hexagons on the boundary
  bound_h3 <- rc_h3[rc_buffer, op = st_intersects]
  bound_cent <- st_centroid(bound_h3)
  bound_cent <- bound_cent %>% select(geometry)
  
  ## Add centroid for rc
  x_cent <- st_centroid(x)
  x_cent <- x_cent %>% select(geom) %>% rename(geometry = geom)
  centres <- st_as_sf(rbind(bound_cent, x_cent))
  
  ## Split
  b_ls <- split(centres, seq(nrow(centres)))
  
  ## Function
  getWalk <- function(x) {
    
    if(rc_lines$Classification == "Regional Centre" | rc_lines$Classification == "Major Town Centre" | rc_lines$Classification == "Large Shopping Centre" | rc_lines$Classification == "Town Centre") {
      iso <- isoline(x, range = (15 * 60), range_type = "time", transport_mode = "pedestrian")
      iso$RC_ID <- rc_lines$RC_ID
      iso$RC_Name <- rc_lines$RC_Name
      iso$Classification <- rc_lines$Classification
      iso$Duration <- "15 mins"
      iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
      Sys.sleep(1)
    }
    if(rc_lines$Classification == "Large Retail Park" | rc_lines$Classification == "Small Retail Park" | rc_lines$Classification == "Small Shopping Centre") {
      iso <- isoline(x, range = (10* 60), range_type = "time", transport_mode = "pedestrian")
      iso$RC_ID <- rc_lines$RC_ID
      iso$RC_Name <- rc_lines$RC_Name
      iso$Classification <- rc_lines$Classification
      iso$Duration <- "10 mins"
      iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
      Sys.sleep(1)
    }
    if(rc_lines$Classification == "District Centre" | rc_lines$Classification == "Market Town" | rc_lines$Classification == "Local Centre") {
      iso <- isoline(x, range = (10 * 60), range_type = "time", transport_mode = "pedestrian")
      iso$RC_ID <- rc_lines$RC_ID
      iso$RC_Name <- rc_lines$RC_Name
      iso$Classification <- rc_lines$Classification
      iso$Duration <- "10 mins"
      iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
      Sys.sleep(1)
    }
    
    return(iso)
}
  
  ## Get the individual hexagon catchments
  catch <- do.call(rbind, lapply(b_ls, getWalk))
  catch_diss <- catch %>%
    summarise()
  catch <- catch %>%
    as.data.frame() %>%
    select(RC_ID, RC_Name, Classification, Duration) %>%
    distinct()
  catch_diss$RC_ID <- catch$RC_ID
  catch_diss$RC_Name <- catch$RC_Name
  catch_diss$Classification <- catch$Classification
  catch_diss$Duration <- catch$Duration
  
  ## Remove any holes
  catch_diss <- sfheaders::sf_remove_holes(catch_diss)
  print(paste0(rc_lines$RC_Name, " ", "Completed"))
  return(catch_diss)
  
}

