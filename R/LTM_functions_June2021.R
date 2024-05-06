#' @import tidyr dplyr tibble 
#' @import ggplot2 viridis stringr
NULL
###############
## Functions ##
###############


################################################################################
################################################################################
# ltm.data.summary() moved to 'ltm.data.summary.R'


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
#' @return Returns the summarized data used to construct the histogram plot
#' @examples
#' data(newnans)
#' newn_sum <- ltm.data.summary("Newnans",newnans)
#' newnans_LD <- len.dist(newn_sum, speciesList=c("BLUE","LMB", "BRBU"), years = c(2016:2020))
#' @export
len.dist<- function(datafile,
                         speciesList = list(),
                         years = list(),
                         seasons = list(),
                         print = FALSE,
                         figure_filename = NA,
                         fig_scale = 1) {
  require(ggplot2)
  require(tidyr)
  require(stringr)
  
  
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

#####################################################################################################################



################################################################################################################
################################################################################################################
#' Check if Outlier
#' 
#' Checks whether value is an outlier
#' @param x vector of numerical values
#'
#' @return boolean
#' @examples
#' numbers = c(1:10,1:10,1:10,1000)
#' numbers_outlier <- is.outlier(numbers)
#' numbers[numbers_outlier]#'
#' @export
is.outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


####################################################################################################################################################
################################################################################
#' Guild Composition Plot
#' 
#' Create Guild Composition plot
#' @param ltm_dataset data, should be output from ltm.data.summary function
#' @param years list of years to include in figure
#' @param waterbody_list ---currently functionless, will update in future version---
#' @param color_scale color scale for output figure defaults to default r color scale, other options include "grayscale" for black and white, and "viridis" for viridis magma scale
#' @param save boolean, if TRUE figure will be saved to file
#' @return returns summarize data used to construct figures
#' @examples
#' data(newnans)
#' newn_sum <- ltm.data.summary(file=newnans)
#' newnans_guild <- guild.comp(newn_sum)
#' @export
guild.comp <- function(ltm_dataset,
                       waterbody_list = list(),
                       years = list(),
                       color_scale = "default",
                       save = FALSE) {
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  require(viridis)
  
    #ltm_dataset=orange_all
import_guild_table <- function() {
  guilda=c("African Jewelfish","AFJE","Exotic","NonGame","Alabama Shad","ALSH","Other","NonGame","Alligator Gar","AGAR","Rough","NonGame","American Eel","AMEE","Other","NonGame","American Shad","AMSH","Other","Game","Atlantic Croaker","ATCR","Other","NonGame","Atlantic Needlefish","ATNE","Rough","NonGame","Atlantic Stingray","ATSR","Other","NonGame","Bay Anchovy","BAAN","Other","NonGame","Banded Sunfish","BASU","Sunfishes","NonGame","Banded Topminnow","BATO","Forage","NonGame","Blackbanded Sunfish","BBSU","Sunfishes","NonGame","Blue Tilapia","BETI","Exotic","NonGame","Bigmouth Sleeper","BISL","Other","NonGame","Black Acara","BKAC","Exotic","NonGame","Black Bullhead","BLBU","Catfish","NonGame","Black Crappie","BLCR","BLCR","Game","Blackbanded Darter","BLDA","Forage","NonGame","Blueback Herring","BLHE","Other","NonGame","Bluefin Killifish","BLKI","Forage","NonGame","Blacktail Redhorse","BLRE","Rough","NonGame","Blacktail Shiner","BLSH","Forage","NonGame","Bluegill","BLUE","Sunfishes","Game","bluegill-redear hybrid","CODE","Sunfishes","Game","Bluenose Shiner","BNSH","Forage","NonGame","Bowfin","BOW","Rough","NonGame","Banded Pygmy Sunfish","BPS","Other","NonGame","Brown Bullhead","BRBU","Catfish","Game","Brown Darter","BRDA","Forage","NonGame","Brown Hoplo","BRHO","Exotic","NonGame","Brook Silverside","BRSI","Forage","NonGame","Bluestripe Shiner","BSSH","Forage","NonGame","Bluespotted Sunfish","BSSU","Sunfishes","NonGame","Butterfly Peacock Bass","BUTT","Exotic","Game","Catfish- Ictaluridae","CASP","Catfish","Game","Channel Catfish","CHCA","Catfish","Game","Clear Chub","CLCH","Forage","NonGame","Clown Goby","CLGO","Forage","NonGame","Common Carp","COCA","Exotic","NonGame","Coastal Shiner","COSH","Forage","NonGame","Common Snook","COSN","Other","Game","Chain Pickerel","CPIK","Rough","NonGame","Crevalle Jack","CRJA","Other","NonGame","Dollar Sunfish","DOSU","Sunfishes","NonGame","Fathead Minnow","FAMI","Exotic","NonGame","Fat Snook","FASN","Other","Game","Florida Gar","FGAR","Rough","NonGame","Flagfish","FLAG","Forage","NonGame","Flathead Catfish","FLCA","Exotic","NonGame","Flier","FLIE","Sunfishes","Game","Flagfin Shiner","FLSH","Forage","NonGame")
  guildb=c("Freshwater Goby","FWGO","Forage","NonGame","Gizzard Shad","GISH","Forage","NonGame","Golden Shiner","GOSH","Forage","NonGame","Golden Topminnow","GOTO","Forage","NonGame","Gulf Coast Pygmy Sunfish","GPSU","Other","NonGame","Grass Carp","GRCA","Exotic","NonGame","Grass Pickerel","GRPK","Rough","NonGame","Greyfin Redhorse","GRRE","Rough","NonGame","Grey Snapper","GRSN","Other","Game","Green Sunfish","GRSU","Exotic","NonGame","Gulf Pipefish","GUPI","Forage","NonGame","Highfin Carpsucker","HICA","Rough","NonGame","Hickory Shad","HISH","Other","NonGame","Hogchoker","HOG","Other","NonGame","Inland Silverside","INSI","Forage","NonGame","Ironcolor Shiner","IRSH","Forage","NonGame","North African Jewelfish","JEWE","Exotic","NonGame","Lake Chubsucker","LACH","Forage","NonGame","Ladyfish","LADY","Other","NonGame","Least Killifish","LEKI","Forage","NonGame","Lake Eustis Pupfish","LEMI","Forage","NonGame","Longnose Gar","LGAR","Rough","NonGame","Orinoco Sailfin Catfish","LIMU","Exotic","NonGame","Lined Topminnow","LITO","Forage","NonGame","Largemouth Bass","LMB","LMB","Game","Longear Sunfish","LOSU","Sunfishes","Game","Mayan Cichlid","MACI","Exotic","NonGame","Madtom","MADT","Catfish","NonGame","Marsh Killifish","MAKI","Forage","NonGame","Mozambique-Blie Tilapia","MBTI","Exotic","NonGame","Midas Cichlid","MICI","Exotic","NonGame","Eastern Mosquitofish","MOSQ","Forage","NonGame")
  guildc=c("Mozambique Tilapia","MOTI","Exotic","NonGame","Naked Goby","NAGO","Forage","NonGame","Nile-Blue Tilapia","NBTI","Exotic","NonGame","Nile Tilapia","NITI","Exotic","NonGame","Other Freshwater Fish","OFF","Other","NonGame","Okefenokee Pygmy Sunfish","OPS","Other","NonGame","Oscar","OSCA","Exotic","NonGame","Other Saltwater Fish","OSF ","Other","NonGame","Pinfish","PIN ","Other","NonGame","Pirate Perch","PIPE","Forage","NonGame","Pugnose Minnow","PUMI","Forage","NonGame","Pygmy Sunfish","PYSU","Other","NonGame","Quillback","QUIL","Rough","NonGame","Rainwater Killifish","RAKI","Forage","NonGame","Redbreast Sunfish","RBSU","Sunfishes","Game","Red Drum","REDR","Other","Game","Redfin Needlefish","RENE","Other","NonGame","Redear Sunfish","RESU","Sunfishes","Game","Rough Silverside","ROSI","Forage","NonGame","Redfin Pickerel","RPIK","Rough","NonGame","Russetfin Topminnow","RUTO","Forage","NonGame","Sailfin Catfish","SACA","Exotic","NonGame","Sailfin Molly","SAMO","Forage","NonGame","Sand Seatrout","SASE","Other","NonGame","Striped Bass Hybrid","SBHY","Other","Game","Seminole Killifish","SEKI","Forage","NonGame","sunfish species","SFSP","Sunfishes","Game","Shortnose Gar","SGAR","Rough","NonGame","Sheepshead","SHEE","Other","Game","Shiner Species - Notropis","SHIN","Forage","NonGame","Silver Jenny","SIJE","Other","NonGame","Silver Seatrout","SISE","Other","Game","Skipjack Herring","SKHE","Other","NonGame","Snook - Centrapominae","SNOO","Other","Game","Southern Flouder","SOFL","Other","Game","Spotted Bass","SPBA","Other","Game","Spotted Bullhead","SPBU","Catfish","NonGame","Spotfin Mojarra","SPMO","Other","NonGame")
  guildd=c("Spotted Seatrout","SPSE","Other","Game","Spotted Sunfish Hybrid","SPSH","Sunfishes","NonGame","Spotted Sucker","SPSK","Rough","NonGame","Spotted Sunfish","SPSU","Sunfishes","Game","Spotted Tilapia","SPTI","Exotic","NonGame","Sunshine Bass","SSBA","Other","Game","Striped Bass","STBA","Other","Game","Striped Mojarra","STMO","Other","NonGame","Striped Mullet","STMU","Other","NonGame","Suckermouth Catfish","SUCA","Exotic","NonGame","Sunfish Species - Lepomis","SUSP","Sunfishes","Game","Shadow Bass","SWBA","Other","Game","Swamp Darter","SWDA","Forage","NonGame","Tadpole Madtom","TAMA","Catfish","NonGame","Tarpon","TARP","Other","Game","Taillight Shiner","TASH","Forage","NonGame","Threadfin Shad","THSH","Forage","NonGame","Tidewater Silverside","TISI","Forage","NonGame","Unnamed Shiner","UNSH","Forage","NonGame","Vermiculated Sailfin Catfish","VSCA","Exotic","NonGame","Walking Catfish","WACA","Exotic","NonGame","Warmouth","WAR","Sunfishes","Game","Weed Shiner","WESH","Forage","NonGame","White Bass","WHBA","Other","Game","White Catfish","WHCA","Catfish","Game","White Mullet","WHMU","Other","NonGame","Yellow Bullhead","YEBU","Catfish","Game","Yellowbelly Cichlid","YECI","Exotic","NonGame","Yellow Perch","YEPE","Forage","Game")
  
guild_lkup = c(guilda,guildb,guildc,guildd)
out = data.frame(matrix(ncol=4, data=guild_lkup, byrow=TRUE))
names(out) = c("SpeciesCommon", "SpeciesCode", "Group1", "Group2")
return(out)
}
guilds = import_guild_table()

#convert cpue table from wide to long
cpue_long = ltm_dataset$CPUE_number
if(length(years) > 0) {cpue_long <- cpue_long %>% dplyr::filter(yr %in% years)}

cpue_long <- 
  cpue_long %>% 
  gather(key="Species", value="CPUE", -Year, -yr) %>% 
  na.omit()

# join cpue table with guild table
cpue_long <-
  cpue_long %>% 
  dplyr::left_join(guilds %>% 
                     dplyr::select(-SpeciesCode) %>% 
                     rename(Species = SpeciesCommon))

#percent composition formula
percent_comp <- function(x) { x/sum(x)}

# remove the "total group (All)", and calculate percent compositions by year
cpue_long <- cpue_long %>% dplyr::filter(Species != "All") %>% dplyr::group_by(yr) %>% mutate(comp= percent_comp(CPUE)) 
guild_fig_data <- cpue_long %>% dplyr::group_by(yr,Group1) %>% dplyr::summarise(comp=sum(comp))
gfd2 <- cpue_long %>% dplyr::group_by(yr,Group2) %>% dplyr::summarise(comp=sum(comp))

# plot

guild_plot <- ggplot2::ggplot(data= guild_fig_data, ggplot2::aes(x=yr,
                                               y=comp,
                                               fill=Group1)) +
geom_bar(position="stack", stat="identity") + 
  theme_bw() + 
  scale_y_continuous(expand = expansion(add = c(0,0))) + 
  scale_x_continuous(expand= expansion(mult = c(0.01,.01)),
                     breaks= c(min(guild_fig_data$yr):max(guild_fig_data$yr))) +
  theme(axis.text.x = element_text(angle=325, vjust=0.25,hjust=0.25),
        legend.title = element_blank(),
        panel.grid.minor= element_blank(),
        panel.grid.major= element_blank(),
        panel.background = element_rect(fill="white",color="white"),
        legend.position= "bottom") + 
  ylab("Percent Composition by Number") + 
  xlab("Year")
if(color_scale == "grayscale") {guild_plot = guild_plot + scale_fill_grey()} else
  if(color_scale == "viridis") {guild_plot = guild_plot + scale_fill_viridis(
    discrete=TRUE, option="magma", alpha = 0.7, direction=-1)}
print(guild_plot)

print(cpue_long %>% ungroup() %>% select (Species, Group1, Group2) %>% distinct(), n = 100)

guild_plot2 <- ggplot2::ggplot(data= gfd2, ggplot2::aes(x=yr,
                                               y=comp,
                                               fill=Group2)) +
  geom_bar(position="stack", stat="identity") + 
  theme_bw() + 
  scale_y_continuous(expand = expansion(add = c(0,.0))) + 
  scale_x_continuous(expand= expansion(mult = c(0.01,.01)),
                     breaks= c(min(gfd2$yr):max(gfd2$yr))) +
  theme(axis.text.x = element_text(angle=325, vjust=0.25,hjust=0.25),
        legend.title = element_blank(),
        panel.grid.minor= element_blank(),
        panel.grid.major= element_blank(),
        panel.background = element_rect(fill="white",color="white"),
        legend.position = "bottom") + 
  ylab("Percent Composition by Number") + 
  xlab("Year")
if(color_scale == "grayscale") {guild_plot2 = guild_plot2 + scale_fill_grey()} else
  if(color_scale == "viridis") {guild_plot2 = guild_plot2 + scale_fill_viridis(
    discrete=TRUE, option="magma", alpha = 0.7, direction=-1)}
print(guild_plot2)


  if(save==TRUE) {
    tiff("Percent_Comp_Guild1.tiff", res = 300, height=1800, width=2100, compression = 'lzw')
    print(guild_plot)
    dev.off()
    tiff("Percent_Comp_Guild2.tiff", res = 300, height=1800, width=2100, compression = 'lzw')
    print(guild_plot2)
    dev.off()
  }

return(list(Guild1 = guild_fig_data,
            Guild2 = gfd2))
}

# species.history --------------------------------------------------------------

################################################################################
################################################################################

## moved to species_history.R



# age.key --------------------------------------------------------


################################################################################
################################################################################
#' Create age-length key
#' 
#' Generate age-length key from a subsample of aged fish
#' @param dataset dataset containing at least 2 columns, one containing an age field, and one containing length field
#' @param age_column string specifying the name of the column containing ages (in years)
#' @param length_column string specifying the name of the column containing lengths
#' @param length_unit string specifying the length units options are: "mm" = millimeters; "cm" = centimeters, if none specified defaults to "mm"
#' @return returns age-length key
#' @examples 
#' data(loch_blcr_2020_age)
#' loch_blcr_alk <- age.key(dataset=loch_blcr_2020_age, age_column="age", length_column="tl", length_unit = 'mm')
#' @export
age.key <- function(dataset,age_column, length_column, length_unit = NA) {
  
  dat = dataset %>% dplyr::select(age = age_column, len = length_column) 
  
  if(is.na(length_unit)) {print("no length unit specified, assuming mm")
    length_unit = "mm"}
  
  #convert length to cm
  if(length_unit != "cm") {
    if(length_unit != "mm") {
      stop("length unit not recognized")
    } else {
      dat$len = floor(dat$len/10)
    }
  }
  
  # Add in missing length and age categories 
  len_range = data.frame(cbind(len = 0:max(dat$len,na.rm=T)))
  age_range = data.frame(cbind(age = min(dat$age, na.rm=T):max(dat$age,na.rm=T)))
  dat = dat %>% full_join(len_range) %>% full_join(age_range) 
  
  count_table = with(dat,table(len,age))
  prop_table = prop.table(count_table, margin = 1)
  prop_table = data.frame(matrix(ncol=ncol(prop_table), data = prop_table, byrow=FALSE))
  names(prop_table) = age_range$age
  prop_table[is.na(prop_table)] = 0
  len_props = apply(prop_table,1,sum)
  
  #set age of lengths smaller than the smallest aged fish to the age of the smallest aged fish
  step=1
  while(step < length(len_props)){
    if(len_props[step] == 0) {prop_table[step,1] = 1} else {break}
    step = step+1
  }
  
  return(prop_table)
  
}

################################################################################
################################################################################
#' Calculate Relative Weight
#' 
#' Calculates the Relative Weight for a sample of fish with both lengths and weights
#' @param dataset dataset containing at least 2 columns, one containing a length field, and one containing weight field
#' @param length_col string specifying the name of the column containing length
#' @param weight_col string specifying the name of the column containing weights
#' @param a value of the alpha parameter for relative weight calculation (specific to species of interest)
#' @param b value of the beta parameter for relative weight calculation (specific to species of interest)
#' @return returns vector of relative weights
#' @examples 
#' library(dplyr)
#' data(newnans)
#' #Calculate relative weight for all LMB collected from Newnans Lake
#' newn_lmb <- newnans %>% dplyr::filter(SpeciesCode == "LMB" )
#' newnans_lmb_Wr <- Wr(dataset = newn_lmb, 
#'                  length_col = "TotalLength",
#'                  weight_col = "TotalWeight",
#'                  a = .3245,
#'                  b = 3.21)
#' @export
Wr <- function(dataset = NA, 
               length_col = NA, 
               weight_col = NA,
               a= NA, 
               b= NA) {
  dat = dataset %>% dplyr::select(len = all_of(length_col),wt = all_of(weight_col))
  wt_pred = with(dat,10^((b*log10(len) + a)))
  return(dat$wt/wt_pred*100)
}