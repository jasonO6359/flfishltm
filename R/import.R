
#' Import data from FWC LTM Query
#'
#' Imports LTM Query data and performs a set of checks that will output warnings
#' if file does not contain expected input, or if file contains data that may
#' not work with 'flfishltm' functions
#' @param datafile an ltm query dataset, can be either ".csv" or ".Rdata" format
#'
#' @return imported dataset
#' @export
#'
#' @examples
#' 
#' data(newnans)
#' newn <- ltm.import(newnans)
ltm.import <- function(datafile) {
  #datafile = "/home/jason/Newn.csv"
  #datafile = "Electrofishing/LMB/Orange_LMB_2021.csv"
  if(typeof(datafile) == "list") {rawdat = data.frame(datafile)} else {
    rawdat = data.frame(read.csv(datafile, header = TRUE))}
    
    if(is.StdFishQry(rawdat)) {} else {
    stop("This function only accepts output from 
    the standard fish query")}
    
    #Identify targets in dataset
    Targets = unique(rawdat$Target)
    
    #Replace undesirable characters in Species Common Names
    rawdat$SpeciesCommon = stringr::str_replace_all(rawdat$SpeciesCommon, "/", "-")
    rawdat$SpeciesCommon = stringr::str_replace_all(rawdat$SpeciesCommon, " x ", "-")
    
    #Check for Non-Standard SamplingTypes
    has.NS(rawdat)
    
    # Waterbodies
    
    # Print summary to console
    ltm.summary(rawdat)
    #return
    rawdat
    # a <- ltm_import(datafile)
    # b <- ltm_import(newnans)
}

