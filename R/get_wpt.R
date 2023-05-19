#' Extract waypoint data from .gpx files
#'
#' @param gpx A .gpx file generated using a Garmin GPSmap 78s  
#'
#' @return data frame containing point ID, lat, long and elevation
#' @export
#'
#' @examples
#' # df <- get_wpt('Waypoints_01-DEC-22.gpx')
get_wpt <- function(gpx) {
  
  doc <- XML::xmlTreeParse(gpx, useInternal = TRUE)
  
  top <- XML::xmlRoot(doc)
  
  XML::xmlName(top)
  
  names(top)
  
  wpt <- 
    XML::xmlSApply(top, function(x) XML::xmlSApply(x, xmlValue))[names(top) == "wpt"] %>% 
    lapply(., function(x) data.frame(t(x))) %>% 
    dplyr::bind_rows(.id = "column_names") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(ele = as.numeric(ele),
           time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ"))
  
  coords <- 
    top %>%
    XML::xmlSApply(., function(x) xmlAttrs(x)) %>%
    .[names(.) == "wpt"] %>%
    lapply(., function(x) data.frame(t(x))) %>% 
    dplyr::bind_rows(.id = "column_names") %>%
    tibble::as_tibble() %>% 
    dplyr::mutate(across(lat:lon, as.numeric))
  
  
  
  coords %>% 
    dplyr::bind_cols(wpt %>% select(-column_names)) %>%
    dplyr::rename(wpt_id = column_names) %>%
    dplyr::mutate(wpt_id = name)
  
}