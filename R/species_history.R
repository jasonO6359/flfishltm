#' Species detection history
#' 
#' Create Species detection history plot
#' @param ltm_dataset data, should be output from ltm.data.summary function
#' @param exclude_species vector of *string* identifying the species names to exclude from the output figure. Should match the spelling of the species names in the figure. 
#' @param save boolean, if TRUE figure will be saved to file
#' @param filename = filename for saved figure
#' @param fig_res = specify figure resolution
#' @param fig_width = specify figure width
#' @param fig_height = specify figure height
#' @param return_object *string* if "data" then function returns the dataset 
#'   that is used to generate the figure, else if "ggplot" then function returns
#'   ggplot object
#' @return either summarised data or a ggplot object. Specify the return object type with `return_object`
#' @examples
#' data(newnans)
#' newn_sum <- ltm.data.summary(file=newnans)
#' newnans_dethist <- species.history(newn_sum,save=FALSE)
#' @export
species_history <- function(LTMdataset,
                            exclude_species = c(),
                            save = FALSE,
                            filename = NA,
                            fig_res = NA,
                            fig_width = NA,
                            fig_height = NA,
                            return_object = "data") {
  
    if(!return_object %in% c("data", "ggplot")) {
    cli::cli_abort(c("invalid {.arg return_object}",
                     "i" = "{.arg return_object} must be either \"data\" or \"ggplot\""))
  }
  
  # require(tidyr)
  # require(tibble)
  # require(ggplot2)
  # require(stringr)
  
  ## DEBUG ## LTMdataset = newn
  
  plotData <- 
    data.frame(LTMdataset$SpeciesHistory) %>% 
    tibble::rownames_to_column(var="Year") %>%
    tidyr::pivot_longer(-Year, names_to = "Species", values_to = "detection") %>% 
    dplyr::filter(!Species %in% exclude_species)
  
  yr_ct <-
    plotData %>% 
    dplyr::group_by(Year) %>% 
    dplyr::summarise(total = sum(detection,na.rm=T)) 
  
  sp_ct <-
    plotData %>% 
    dplyr::group_by(Species) %>% 
    dplyr::summarise(total = sum(detection, na.rm=T))
  
  detection_plot <- 
    ggplot2::ggplot(data = plotData, 
                    ggplot2::aes(x=Year,
                                 y=reorder(Species, desc(Species)), 
                                 fill = as.factor(detection))) + 
    geom_tile(width = 0.95, height = 0.95) +
    ylab("Species") +  
    scale_fill_manual(name="Detection", 
                      labels= c("0" = "Not Detected","1" = "Detected"),
                      values= c("1" = "#689f38","0" = "#DEDEDE")) +
    scale_x_discrete(position="top") +
    coord_cartesian(clip= "off",
                    expand=FALSE,
                    ylim=c(0.5,nrow(sp_ct)+0.5),
                    xlim=c(0.5,nrow(yr_ct)+0.5)) + #, ylim=c(0,NA), expand = FALSE) # +
    geom_text(data = yr_ct, 
              ggplot2::aes(x=Year, y=-0.25,
                           fill = NULL,
                           label=total), 
              size =3) + 
    geom_text(data = yr_ct[1,],
              ggplot2::aes(x= -1, 
                           y=-0.25, 
                           fill=NULL, 
                           label = "Total Species:"),
              hjust=0.25,size=3) +
    geom_text(data = sp_ct, 
              ggplot2::aes(x=nrow(yr_ct)+1,
                           y=Species, 
                           fill = NULL,
                           label=total), 
              size =3) + 
    ggplot2::geom_text(data = sp_ct[1,],
                       ggplot2::aes(x= nrow(yr_ct)+1, 
                           y=nrow(sp_ct)+1, 
                           fill=NULL, 
                           label = "Total\nYears"),
              hjust=0.5,
              vjust= 0.25,
              size=3) +
    theme_bw() + 
    theme(legend.title = element_blank(),
          panel.background = element_rect(fill ="black"),
          panel.grid = element_blank(),
          legend.position = "bottom",
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.box.margin = margin(t = 6),
          axis.text = element_text(color="black"),
          plot.margin= margin(r= 30))
  
  print(detection_plot)
  
  #
  fn = "Species_Detection_History.tiff"
  if(!is.na(filename)) {fn = filename}
  if(substr(fn,-6,-1) != ".tiff") {fn = base::paste(fn,".tiff",collapse="")}
  
  rs = ifelse(is.na(fig_res), 300, fig_res)
  ht = ifelse(is.na(fig_height), length(unique(plotData$Species))*rs/4.5, fig_height)
  wid = ifelse(is.na(fig_width), length(unique(plotData$Year))*rs/1.75, fig_width)
  
  if(save == TRUE) {  
    tiff(fn, 
         height = ht, 
         width = wid, 
         res = rs,
         compression = "lzw")
    print(detection_plot)
    dev.off()
  }
  
  if(return_object == "data") {
    return(plotData)
  } else if(return_object == "ggplot") {
    return(detection_plot)
  }
  
}