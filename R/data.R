#' LTM Fish Community Data for Newnans Lake, FL
#' 
#' Dataset containing fish collection for Newnans Lake for the period 2006-
#' 2020. Data is output of "Standard Fish Query"
#' 
#' @format A data frame with 13,326 rows and 40 variables:
#' \describe{
#'     \item{ID}{unique record ID #}
#'     \item{WaterBody}{lake, river or other resource where data were collected, Factor}
#'     \item{County}{FL County of Collection}
#'     \item{Date}{collection date}
#'     \item{Time}{collection time}
#'     \item{SamplingType}{Sampling protocol: "Standard", "Haphazard", "NS-Random", etc TODO FILL IN ALL PROTOCOLS}
#'     \item{Target}{Target species}
#'     \item{Season}{Sampling season}
#'     \item{Gear}{Sampling gear used}
#'     \item{Effort}{Effort, expressed as time in seconds}
#'     \item{DistanceM}{Distance covered during sample, in meters}
#'     \item{Site}{site identification name or number}
#'     \item{BeginLat}{Latitude of starting location, decimal degrees}
#'     \item{BeginLong}{Longitude of starting location, decimal degrees}}
"newnans"


#' Lochloosa Black Crappie Fall 2020 Aged Subsample
#'
#' Contains total length and ages for the aged subsample of Black Crappie
#' collected on Lochloosa Lake, FL during fall 2020 LTM trawl survyes
#'
#' @format A dataframe with 103 obs and 5 variables
#' \describe{
#'     \item{WaterBody}{Waterbody where collection was made}
#'     \item{collected}{collection date}
#'     \item{Species}{Species code; BLCR = Black Crappie Pomoxis nigromaculatus}
#'     \item{tl}{total length in mm}
#'     \item{age}{age determined from otolith reads assuming Jan 1 birthdate, in yrs}}
"loch_blcr_2020_age"