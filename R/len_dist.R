#########################################################################################
################################################################################
#' Length Distribution Plot
#' 
#' Create Length Distribution histograms for species and years selected
#' @param datafile data, should be output from ltm.data.summary function
#' @param speciesList list of selected species, can specify by common name, scientific name or species code
#' @param years list of years to include in figure
#' @param seasons ---currently functionless, will update in future version, to avoid errors make sure that all seasons in input dataset are the same---
#' @param print boolean, if TRUE figure will be saved to file
#' @param figure_filename if print=TRUE, figure will be saved to this filename
#' @param fig_scale adjust to scale output figure size
#' @param return_object *string* if "data" then function returns the dataset 
#'   that is used to generate the figure, else if "ggplot" then function returns
#'   ggplot object
#' @return either summarised data or a ggplot object. Specify the return object type with `return_object`
#' @examples
#' data(newnans)
#' newn_sum <- ltm.data.summary("Newnans",newnans)
#' newnans_LD <- len_dist(newn_sum, speciesList=c("BLUE","LMB", "BRBU"), years = c(2016:2020))
#' @export
len_dist<- function(datafile,
                    speciesList = list(),
                    years = list(),
                    seasons = list(),
                    print = FALSE,
                    figure_filename = NA,
                    fig_scale = 1,
                    return_object = "data") {
  
  if(length(speciesList) == 0) {
    species = datafile$SpeciesList
  } else {
    species = speciesList}
  
  ## internal functions ----------------------------------------------------------  
  internal_get_year <- function(data, date_field) {
    formats <- 
      data %>% 
      pull({{ date_field }}) %>% 
      lubridate::guess_formats(orders = c("mdy", "ymd")) %>% 
      unique() %>% 
      .[!stringr::str_detect(.,"O")]
    
    if(length(formats) > 1) {
      cli::cli_abort("more than 1 date format detected")
    } else if (formats[[1]] == "%m/%d/%Y") {
      return(data %>%
               mutate(yr = lubridate::year(lubridate::mdy({{ date_field }}))) %>%
               pull(yr))
    } else if (formats[[1]] == "%Y-%m-%d") {
      return(data %>%
               mutate(yr = lubridate::year(lubridate::ymd({{ date_field }}))) %>%
               pull(yr))
    } else {
      cli::cli_abort("date format not recognized")
    }
    
  }
  
  ## Process ---------------------------------------------------------------------  
  workingdat <- 
    datafile$RawData %>% 
    dplyr::filter((SpeciesCode %in% species) | 
                    (SpeciesCommon %in% species)|
                    (SpeciesScientific %in% species)) %>% 
    mutate(Year = internal_get_year(., Date)) %>% 
    mutate(SeasYr = base::paste0(Season,"-", Year))
  
  
  if(length(years) != 0) {workingdat = workingdat[which(workingdat$Year %in% years), ]}
  if(length(seasons) != 0) { workingdat = workingdat[which(workingdat$Season %in% seasons), ]}
  
  
  workingsum = workingdat %>% 
    dplyr::group_by(SpeciesCode, SeasYr, TL_CM_Group) %>% 
    dplyr::summarise(Count = sum(Count, na.rm = TRUE))
  
  
  sl = unique(workingsum$SpeciesCode)
  
  tbl = workingsum[-c(1:nrow(workingsum)), -grep("Count", names(tbl))]
  # print("loop through each target species")
  for(x in 1:length(unique(workingsum$SpeciesCode))) {
    #  print(unique(workingsum$SpeciesCode)[x])
    subdat = workingsum[which(workingsum$SpeciesCode == sl[x]),]
    mx = max(subdat$TL_CM_Group, na.rm = TRUE)
    #  print(mx)
    splenvec = data.frame(TL_CM_Group = c(0:mx))
    #   print("loop through each target season")
    for(i in 1:length(unique(subdat$SeasYr))) {
      #    print(base::paste("season:",unique(subdat$SeasYr)[i]))
      newrows = splenvec
      newrows$SpeciesCode = subdat$SpeciesCode[1]
      newrows$SeasYr = unique(subdat$SeasYr)[i]
      newrows = newrows[, c("SpeciesCode", "SeasYr", "TL_CM_Group")]
      tbl = rbind(tbl, newrows)
    }
    
  }
  # print('merge data')
  workingsum = merge(tbl, workingsum, all.x = TRUE)
  workingsum$Count[is.na(workingsum$Count)] = 0
  workingsum$percent = NA
  #  print('loop through each row in temp dataset and build ss table')
  for(x in 1:nrow(workingsum)) {
    ss = workingsum[which(workingsum$Species == workingsum[x,1] & workingsum$SeasYr == workingsum[x,2]),]
    workingsum[x,5] = (workingsum[x,4])/sum(ss$Count)
  }
  #### Working Sum formatted and ready to go!
  #print("create length distribution plot, facet by species")
  lendist = ggplot2::ggplot(data = workingsum, ggplot2::aes(x = TL_CM_Group, y = percent)) + 
    geom_bar(stat = "identity", width = 1) + 
    facet_grid(SeasYr ~ SpeciesCode, scales = "free_x") + 
    theme_bw() + scale_y_continuous(expand = expansion(add = c(0,0.05))) + 
    scale_x_continuous(breaks = scales::breaks_extended(n=5)) +
    ylab("Percent Total") + 
    xlab("Total Length (cm)")
  print(lendist)
  
  if(print == TRUE) {
    if(is.na(figure_filename)){figure_filename="length.distribution.tiff"}
    if(substr(figure_filename,-6,-1) != ".tiff") {figure_filename = base::paste(figure_filename,".tiff",collapse="")}
    figHt = length(years)
    figWid = length(speciesList)
    scaler= fig_scale
    tiff(figure_filename, res = 300, height = (figHt*scaler*300), width = (figWid*scaler*2*300), compression = 'lzw')
    print(lendist)
    dev.off()
  }
  return(workingsum)
}