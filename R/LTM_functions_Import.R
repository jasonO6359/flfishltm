# #LTM Data Object
#
# # write code to identify an "LTM data object and format field classes appropriately
# #
#
#' Standard Fish Query Test
#'
#' Check if data is from LTM Standard Fish Query
#' @param x dataset
#' @return true/false
#' @examples
#' data(newnans)
#' is.StdFishQry(newnans)
#' ####
#' is.StdFishQry(newnans[, -1])
#' ###
#' new2 <- newnans
#' new2$newcolumn <- NA
#' is.StdFishQry(new2)
#' @export
is.StdFishQry <- function(x) {
  factor_list <- c(
    "WaterBody", "SamplingType", "Target", "Season", "Gear",
    "Stratum", "Grid", "MiniGrid", "Segment", "Reach", "TransType",
    "SpeciesCode", "SpeciesCommon", "SpeciesScientific", "EcoType",
    "SizeClass", "ExcludeWt", "Sex", "IsBatchWt"
  )
  char_list <- c(
    "ID", "County", "Date", "Time", "Site", "NumSegments", "Tag",
    "FishNote", "SequenceNote", "FishHealthCode"
  )
  int_list <- c("Effort", "FS_ID", "FM_ID", "Count", "TotalLength", "TL_CM_Group")
  num_list <- c("DistanceM", "BeginLat", "BeginLong", "TotalWeight", "BatchWtValue")
  all_fields <- c(factor_list, char_list, int_list, num_list)
  unrec_names <- base::names(x)[!(base::names(x) %in% all_fields)]
  miss_fields <- all_fields[!(all_fields %in% base::names(x))]
  #
  if (base::length(unrec_names) > 0) {
    out <- FALSE
    warning(base::paste("The following are unrecognized: ", unrec_names, collapse = ""))
  } else if (base::length(miss_fields) > 0) {
    out <- FALSE
    warning(base::paste("The following Standard Fish Query fields are missing: ", miss_fields, collapse = ""))
  } else {
    out <- TRUE
  }
  base::return(out)
}


#
has.NS <- function(x) {
  if(!("SamplingType" %in% names(x))) { warning("No SamplingType field in dataset") } else {
    types = unique(x$SamplingType)
    type_check = types != "Standard"
    if("Standard" %in% types) {
    if(sum(type_check) > 0) {warning(base::paste("Dataset contains the following nonstandard SamplingTypes: ",
    toString(types[type_check]),
    ". If you intended to include nonstandard samples, be aware some 'flfishltm' functions may filter out data from non-standard samples", collapse =""))
    }} else {warning("No Standardized Samples in dataset, 
                      many functions assume standardized data")}}
  }

target.check <- function(dataset, target) {
  target %in% unique(x$Target)
}

ltm.summary <- function(x) {
  if(is.StdFishQry(x)) {
    waterbodies = unique(x$WaterBody)
    targets = unique(x$Target)
    samptypes = unique(x$SamplingType)
    gear = unique(x$Gear)
    print(cat(paste("Data contains"),
          paste("Waterbodies: ",toString(waterbodies)),
          paste("Target Species: ", toString(targets)),
          paste("Sample Types: ", toString(samptypes)),
          paste("Gear Types: ", toString(gear)),sep ="\n"))
  } else {stop("Data isn't recognized as a standard fish query, check
               dataset")}
}

