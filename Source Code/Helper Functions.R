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
getWalkingCatchment <- function(x) {
  
  # Part 1 - Identifying multiple points from which to derive bounda --------
  ## Pull out the H3's for the retail centre
  rc_h3 <- polyfill(x, 11, FALSE)
  rc_h3 <- h3_to_polygon(unlist(rc_h3$h3_polyfillers), simple = FALSE)
  rc_h3 <- st_transform(rc_h3, 27700)
  
  ## Get centroid for every H3 tile
  rc_h3_cent <- st_centroid(rc_h3)
  rc_h3_cent <- rc_h3_cent %>% dplyr::select(geometry)
  
  ## Add centroid for rc
  x_cent <- st_centroid(x)
  x_cent <- x_cent %>% dplyr::select(geom) %>% dplyr::rename(geometry = geom)
  centres <- st_as_sf(rbind(rc_h3_cent, x_cent))
  
  # Part 2 - Building multiple catchments from multiple points ------------------------
  ## Split
  b_ls <- split(centres, seq(nrow(centres)))
  
  
  ## Function for extracting walking catchments
  getWalk <- function(x) {
    
    if(rc_lines$Classification == "Regional Centre" | rc_lines$Classification == "Major Town Centre" | rc_lines$Classification == "Large Shopping Centre" | rc_lines$Classification == "Town Centre") {
      iso <- isoline(x, range = (15 * 60), range_type = "time", transport_mode = "pedestrian")
      iso$RC_ID <- rc_lines$RC_ID
      iso$RC_Name <- rc_lines$RC_Name
      iso$Classification <- rc_lines$Classification
      iso$Duration <- "15 mins"
      iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    }
    if(rc_lines$Classification == "Large Retail Park" | rc_lines$Classification == "Small Retail Park" | rc_lines$Classification == "Small Shopping Centre") {
      iso <- isoline(x, range = (10* 60), range_type = "time", transport_mode = "pedestrian")
      iso$RC_ID <- rc_lines$RC_ID
      iso$RC_Name <- rc_lines$RC_Name
      iso$Classification <- rc_lines$Classification
      iso$Duration <- "10 mins"
      iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    }
    if(rc_lines$Classification == "District Centre" | rc_lines$Classification == "Market Town" | rc_lines$Classification == "Local Centre") {
      iso <- isoline(x, range = (10 * 60), range_type = "time", transport_mode = "pedestrian")
      iso$RC_ID <- rc_lines$RC_ID
      iso$RC_Name <- rc_lines$RC_Name
      iso$Classification <- rc_lines$Classification
      iso$Duration <- "10 mins"
      iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]
    }
    if(rc_lines$Classification == "Small Local Centre") {
      iso <- isoline(x, range = (7 * 60), range_type = "time", transport_mode = "pedestrian")
      iso$RC_ID <- rc_lines$RC_ID
      iso$RC_Name <- rc_lines$RC_Name
      iso$Classification <- rc_lines$Classification
      iso$Duration <- "7 mins"
      iso <- iso[, c("RC_ID", "RC_Name", "Classification", "Duration")]}
    return(iso)
    }
    
    ## Run to get catchments
    catch <- do.call(rbind, mclapply(b_ls, getWalk))
    catch <- catch %>% st_transform(27700)
    
    # Part 3 - Reducing to one catchment --------------------------------------
    
    ## Clean to one catchment
    catch_diss <- catch %>%
      summarise()
    
    ## Reattaching variables
    catch <- catch %>%
      as.data.frame() %>%
      select(RC_ID, RC_Name, Classification, Duration) %>%
      distinct()
    catch_diss$RC_ID <- catch$RC_ID
    catch_diss$RC_Name <- catch$RC_Name
    catch_diss$Classification <- catch$Classification
    catch_diss$Duration <- catch$Duration
    
    ## Merge with boundary of retail centre
    
    ## Merge with boundary of retail centre - accounts for points not on network
    x_un <- x %>% select(geom) %>% st_transform(27700)
    union <- st_union(catch_diss, x_un)
    union <- sfheaders::sf_remove_holes(union)
    return(union)}

