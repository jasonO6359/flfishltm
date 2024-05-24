#' LTM Data Summary
#' 
#' Summarize raw LTM data from "Standard Fish Query"
#' @param waterbodyname The name of the selected Waterbody as you would like it to appear in filenames and figure titles
#' @param file path to the raw query file, should be a .csv file
#' @param outtables option to specify which summary tables to save to working directory, default 0 exports no summary tables
#' @param printfigs option to specify which figures to save to print directory, default 0 prints no figures, 1 prints all figures
#' @param print_directory specify directory to save exported figures
#' @return list of summarized datasets
#' @examples
#' #Import demo data for Newnans Lake
#' data(newnans)
#' #dplyr::summarise Newnans Lake data
#' newn_sum <- ltm.data.summary("Newnans Lake", file = newnans)
#' # Access summary tables within the newn_sum object
#' newn_sum$SpeciesList
#' newn_sum$CPUE_number
#' newn_sum$RawData
#' newn_sum$Comp_num
#' @export
ltm.data.summary <- function(waterbodyname = "No Waterbody Specified",
                             file,
                             outtables = 0,
                             printfigs = 0,
                             print_directory = getwd()) {

  
  ImportedData = lds_ImpData(file) 
  
  if("community" %in% names(ImportedData)) {
    Data.proc = lds_process_data(ImportedData$community,outtables,waterbodyname)
    lds_plot_figs(Data.proc,printfigs,print_directory,waterbodyname)
  }
  if("LMB" %in% names(ImportedData)) {
    Data.proc = lds_process_data(ImportedData$LMB,outtables,waterbodyname)
    lds_plot_figs(Data.proc,printfigs,print_directory,waterbodyname)
  }
  return(Data.proc)
}


lds_ImpData <- function(datafile) {
  #datafile = "Electrofishing/LMB/Orange_LMB_2021.csv"
  if(typeof(datafile) == "list") {rawdat = data.frame(datafile)} else {
    rawdat <- data.frame(read.csv(datafile, header = TRUE, colClasses = c(
      ID = 'character',
      WaterBody = 'factor',
      County = 'character',
      Date = 'character',
      Time = 'character', #revist
      SamplingType = 'factor',
      Target = 'factor',
      Season = 'factor',
      Gear = 'factor',
      Effort = 'integer',
      DistanceM = 'numeric',
      Site = 'character',
      BeginLat = "numeric",
      BeginLong = 'numeric',
      Stratum = 'factor',
      Grid = 'factor',
      MiniGrid = 'factor',
      Segment = 'factor',
      NumSegments = 'character',
      Reach = 'factor',
      TransType = 'factor',
      FS_ID = 'integer',
      FM_ID = 'integer',
      SpeciesCode = "factor",
      SpeciesCommon = 'factor',
      SpeciesScientific = 'factor',
      EcoType = 'factor',
      SizeClass ="factor",
      Count = 'integer',
      TotalLength = 'integer',
      TL_CM_Group = 'integer',
      TotalWeight = 'numeric',
      IsBatchWt = 'factor',
      BatchWtValue = 'numeric',
      ExcludeWt = 'factor',
      Sex = 'factor',
      Tag = 'character',
      FishNote = 'character',
      SequenceNote = 'character',
      FishHealthCode = 'character')))
    
    # names(rawdat) = c("ID",
    #                   "WaterBody" ,
    #                   "County",
    #                   "Date",
    #                   "Time",
    #                   "Type",
    #                   "Target",
    #                   "Season",
    #                   "Gear" ,
    #                   "Effort" ,
    #                   "DistanceM",
    #                   "Site"  ,
    #                   "BeginLat",
    #                   "BeginLong",
    #                   "Stratum",
    #                   "Grid",
    #                   "Minigrid",
    #                   "Segment",
    #                   "NumSegs",
    #                   "Reach",
    #                   "TransType",
    #                   "FS_ID",
    #                   "FM_ID",
    #                   "Species",
    #                   "SpeciesCommon",
    #                   "SpeciesSci",
    #                   "Ecotype",
    #                   "SizeClass",
    #                   "Count",
    #                   "TotalLength",
    #                   "TL_CM_Group" ,
    #                   "TotalWeight" ,
    #                   "IsBatchWt",
    #                   "BatchWtValue",
    #                   "ExcludeWt",
    #                   "Sex",
    #                   "Tag",
    #                   "FishNote",
    #                   "SequenceNote")
  }
  Targets = unique(rawdat$Target)
  
  #Replace undesirable characters in Species Common Names
  rawdat$SpeciesCommon = stringr::str_replace_all(rawdat$SpeciesCommon, "/", "-")
  rawdat$SpeciesCommon = stringr::str_replace_all(rawdat$SpeciesCommon, " x ", "-")
  
  outlist <- list(rawdat=rawdat)
  if("ALL" %in% Targets) {outlist$community = rawdat %>% 
    dplyr::filter(Target == "ALL")}
  
  if("LMB" %in% Targets) {outlist$LMB = rawdat %>% dplyr::filter(Target == "LMB")}
  
  if("BLCR" %in% Targets) {outlist$LMB = rawdat %>% dplyr::filter(Target == "BLCR")}
  
  
  #outlist$community <- outlist$community %>% dplyr::filter(SpeciesCode != "MOSQ")
  return(outlist)
}

lds_process_data <- function(rawfile,OutTab = 0, name) {
  #Function to add missing years into final summary table outputs
  
  
  #create temp data
  #rawfile=outlist$LMB
  dat <- rawfile
  # dat = ds
  dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
  
  #Create a factor variable to identify each sample year
  dat$LTM_sample <- ifelse(as.numeric(format(dat$Date, "%m")) %in% c(7:12),
                           base::paste(format(as.Date(dat$Date, format="%d/%m/%Y"),"%Y"),"-",(as.numeric(format(as.Date(dat$Date, format="%d/%m/%Y"),"%Y"))+1)),
                           base::paste((as.numeric(format(as.Date(dat$Date, format="%d/%m/%Y"),"%Y"))-1),"-",(as.numeric(format(as.Date(dat$Date, format="%d/%m/%Y"),"%Y"))))
  )
  
  #Remove rows without a date (should remove empty rows)
  dat <- dat[!dat$LTM_sample == "NA - NA",]
  # Fill empty counts with 0
  dat$Count[which(is.na(dat$Count))] <- 0
  
  
  
  # Create a species list without a blank first item
  SpeciesList <- unique(as.character(dat$SpeciesCommon))
  dat$Species = dat$SpeciesCommon
  
  # Create a table to calc CPUE by Year
  CPUE_Tab <- data.frame(matrix(ncol = 2 + length(SpeciesList)))
  names(CPUE_Tab) <- c("Sample", "Site", SpeciesList)
  
  # Summarize fish counts and weights by Site and year
  
  sum_tab <- data.frame(stats::aggregate(list(SiteCount = dat$Count, SiteWt = dat$TotalWeight), 
                                         by = list(Year = dat$LTM_sample, Site = dat$Site, Species = dat$Species, Effort = dat$Effort/60), 
                                         FUN = sum, na.rm = TRUE))
  sum_tab$Ctmin <- sum_tab$SiteCount/sum_tab$Effort
  sum_tab$Wtmin <- sum_tab$SiteWt/sum_tab$Effort
  year_site_sum <- stats::aggregate(list(Year = sum_tab$Year, Site= sum_tab$Site), by = list(Year = sum_tab$Year, Site= sum_tab$Site), FUN = identity)[,c(1,2)]
  TotalCount <- stats::aggregate(list(TotalCountAll = sum_tab$SiteCount), by = list(Year = sum_tab$Year), FUN = sum)
  # Finish setting up CPUE table
  # Add blank rows to allow year and site to be copied in
  CPUE_Tab[length(year_site_sum$Year),] <- rep(NA, length(CPUE_Tab))
  # Copy in year and site 
  CPUE_Tab$Sample <- year_site_sum$Year
  CPUE_Tab$Site <- year_site_sum$Site
  CPUE_Tab_number <- CPUE_Tab
  CPUE_Tab_weight <- CPUE_Tab
  
  # Transcrib sum_tab into CPUE_Tab
  for (x in 1:nrow(sum_tab)) {
    x <- x
    CPUE_Tab_number[which(CPUE_Tab$Sample == base::paste(sum_tab[x,"Year"]) & CPUE_Tab$Site == base::paste(sum_tab[x,"Site"])), base::paste(sum_tab[x,"Species"])] <- as.numeric(sum_tab[x,"Ctmin"])
    CPUE_Tab_weight[which(CPUE_Tab$Sample == base::paste(sum_tab[x,"Year"]) & CPUE_Tab$Site == base::paste(sum_tab[x,"Site"])), base::paste(sum_tab[x,"Species"])] <- as.numeric(sum_tab[x,"Wtmin"])
  }
  #Replace NA with 0
  CPUE_Tab_number[is.na(CPUE_Tab_number)] <- 0
  CPUE_Tab_weight[is.na(CPUE_Tab_weight)] <- 0
  #Create Total species count and weight field
  CPUE_Tab_number$All <- apply(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], 1, sum)
  CPUE_Tab_weight$All <- apply(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], 1, sum)
  
  #CPUE by number Table
  CPUE_number <- stats::aggregate(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], by = list(Year = CPUE_Tab_number$Sample), FUN = mean)
  CPUE_number$yr <- as.numeric(substr(CPUE_number[,1],1,4))
  CPUE_number <- CPUE_number[,c(1,(ncol(CPUE_number)),2:((ncol(CPUE_number))-1))]
  CPUE_number <- helper_addgapyears(CPUE_number)
  
  #CPUE by Number Standard Error
  CPUE_number_SE <- stats::aggregate(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], by = list(Year = CPUE_Tab_number$Sample), FUN = function(x) sd(x)/sqrt(length(x)))
  CPUE_number_SE$yr <- as.numeric(substr(CPUE_number_SE[,1],1,4))
  CPUE_number_SE <- CPUE_number_SE[,c(1,(ncol(CPUE_number_SE)),2:((ncol(CPUE_number_SE))-1))]
  CPUE_number_SE <- helper_addgapyears(CPUE_number_SE)
  #CPUE by Number Standard Deviation
  CPUE_number_SD <- stats::aggregate(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], by = list(Year = CPUE_Tab_number$Sample), FUN = function(x) sd(x))
  CPUE_number_SD$yr <- as.numeric(substr(CPUE_number_SD[,1],1,4))
  CPUE_number_SD <- CPUE_number_SD[,c(1,(ncol(CPUE_number_SD)),2:((ncol(CPUE_number_SD))-1))]
  CPUE_number_SD <- helper_addgapyears(CPUE_number_SD)
  #CPUE by number MIN Table
  CPUE_number_min <- stats::aggregate(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], by = list(Year = CPUE_Tab_number$Sample), FUN = min)
  CPUE_number_min$yr <- as.numeric(substr(CPUE_number_min[,1],1,4))
  CPUE_number_min <- CPUE_number_min[,c(1,(ncol(CPUE_number_min)),2:((ncol(CPUE_number_min))-1))]
  CPUE_number_min <- helper_addgapyears(CPUE_number_min)
  #CPUE by number MAX Table
  CPUE_number_max <- stats::aggregate(CPUE_Tab_number[3:ncol(CPUE_Tab_number)], by = list(Year = CPUE_Tab_number$Sample), FUN = max)
  CPUE_number_max$yr <- as.numeric(substr(CPUE_number_max[,1],1,4))
  CPUE_number_max <- CPUE_number_max[,c(1,(ncol(CPUE_number_max)),2:((ncol(CPUE_number_max))-1))]
  CPUE_number_max <- helper_addgapyears(CPUE_number_max)
  
  CPUE_weight <- stats::aggregate(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], by = list(Year = CPUE_Tab_weight$Sample), FUN = mean)
  CPUE_weight$yr <- as.numeric(substr(CPUE_weight[,1],1,4))
  CPUE_weight <- CPUE_weight[,c(1,(ncol(CPUE_weight)),2:((ncol(CPUE_weight))-1))]
  CPUE_weight <- helper_addgapyears(CPUE_weight)
  #CPUE by Number Standard Error
  CPUE_weight_SE <- stats::aggregate(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], by = list(Year = CPUE_Tab_weight$Sample), FUN = function(x) sd(x)/sqrt(length(x)))
  CPUE_weight_SE$yr <- as.numeric(substr(CPUE_weight_SE[,1],1,4))
  CPUE_weight_SE <- CPUE_weight_SE[,c(1,(ncol(CPUE_weight_SE)),2:((ncol(CPUE_weight_SE))-1))]
  CPUE_weight_SE <- helper_addgapyears(CPUE_weight_SE)
  #CPUE by weight Standard Deviation
  CPUE_weight_SD <- stats::aggregate(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], by = list(Year = CPUE_Tab_weight$Sample), FUN = function(x) sd(x))
  CPUE_weight_SD$yr <- as.numeric(substr(CPUE_weight_SD[,1],1,4))
  CPUE_weight_SD <- CPUE_weight_SD[,c(1,(ncol(CPUE_weight_SD)),2:((ncol(CPUE_weight_SD))-1))]
  CPUE_weight_SD <- helper_addgapyears(CPUE_weight_SD)
  #CPUE by number MIN Table
  CPUE_weight_min <- stats::aggregate(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], by = list(Year = CPUE_Tab_weight$Sample), FUN = min)
  CPUE_weight_min$yr <- as.numeric(substr(CPUE_weight_min[,1],1,4))
  CPUE_weight_min <- CPUE_weight_min[,c(1,(ncol(CPUE_weight_min)),2:((ncol(CPUE_weight_min))-1))]
  CPUE_weight_min <- helper_addgapyears(CPUE_weight_min)
  #CPUE by weight MAX Table
  CPUE_weight_max <- stats::aggregate(CPUE_Tab_weight[3:ncol(CPUE_Tab_weight)], by = list(Year = CPUE_Tab_weight$Sample), FUN = max)
  CPUE_weight_max$yr <- as.numeric(substr(CPUE_weight_max[,1],1,4))
  CPUE_weight_max <- CPUE_weight_max[,c(1,(ncol(CPUE_weight_max)),2:((ncol(CPUE_weight_max))-1))]
  CPUE_weight_max <- helper_addgapyears(CPUE_weight_max)
  
  #Create % Composition Tables
  if(ncol(CPUE_number) > 4) {Comp_Sum <- data.frame(list(Year = substr(CPUE_number$Year,1,4),
                                                         Tot_CPUE_num = as.numeric(apply(CPUE_number[,3:(ncol(CPUE_number)-1)], 1, sum)),
                                                         Tot_CPUE_wt = as.numeric(apply(CPUE_weight[,3:(ncol(CPUE_weight)-1)], 1, sum))))} else {Comp_Sum <- "No Composition table for Single Species data"
                                                         }
  
  if(is.data.frame(Comp_Sum)) {
    Comp_num <- CPUE_number[,3:(ncol(CPUE_number)-1)]/Comp_Sum$Tot_CPUE_num
    Comp_wt <- CPUE_weight[,3:(ncol(CPUE_weight)-1)]/Comp_Sum$Tot_CPUE_wt
    Comp_Sum$SimpsonsD <- (1/(apply(Comp_num^2,1,sum)))} else {
      Comp_num <- "No Composition tables for single species data"
      Comp_wt <- "No Composition tables for single species data"
    }
  
  #Summarize Sample size by year
  Year_Sites <- CPUE_Tab %>%
    dplyr::group_by(Sample) %>%
    dplyr::summarise(dplyr::n_distinct(Site))
  
  #FORMAT ANNUAL SUMMARY FOR MOST RECENT YR in DATASET
  
  a = CPUE_number %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_num", c(-Year,-yr))
  b = CPUE_number_SE %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_num_SE", c(-Year,-yr))
  c = CPUE_weight %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_wt", c(-Year,-yr))
  d = CPUE_weight_SE %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_wt_SE", c(-Year,-yr))
  e = CPUE_number_min %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_num_min", c(-Year,-yr))
  f = CPUE_number_max %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_num_max", c(-Year,-yr))
  g = CPUE_weight_min %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_wt_min", c(-Year,-yr))
  h = CPUE_weight_max %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_wt_max", c(-Year,-yr))
  i = CPUE_number_SD %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_num_SD", c(-Year,-yr))
  j = CPUE_weight_SD %>% dplyr::filter(yr==max(yr, na.rm=T)) %>% tidyr::pivot_longer(names_to="Species", values_to="CPUE_wt_SD", c(-Year,-yr))
  
  
  
  annual_summary = a %>% 
    dplyr::left_join(b, by = join_by(Year, yr, Species)) %>% 
    dplyr::left_join(i, by = join_by(Year, yr, Species)) %>% 
    dplyr::left_join(e, by = join_by(Year, yr, Species)) %>% 
    dplyr::left_join(f, by = join_by(Year, yr, Species)) %>%
    dplyr::left_join(c, by = join_by(Year, yr, Species)) %>% 
    dplyr::left_join(d, by = join_by(Year, yr, Species)) %>% 
    dplyr::left_join(j, by = join_by(Year, yr, Species)) %>% 
    dplyr::left_join(g, by = join_by(Year, yr, Species)) %>% 
    dplyr::left_join(h, by = join_by(Year, yr, Species)) %>%
    dplyr::arrange(Species)
  
  
  # Generate Species History Table
  
  species.history = CPUE_number %>% dplyr::select(-Year,-All) %>%
    tibble::rownames_to_column() %>% dplyr::select(-rowname) %>%
    tibble::column_to_rownames("yr")
  species.history = species.history %>% dplyr::relocate(sort(names(species.history)))
  species.history = data.frame(ifelse(species.history > 0, 1, 0))
  
  ## Print out tables
  PrintTables <- function(OutTab) {
    prtCPUENum <- function(x) {
      write.csv(CPUE_number, base::paste(name,"-","CPUE_number.csv",sep =""))
      write.csv(CPUE_number_SE, base::paste(name,"-","CPUE_number_SE.csv",sep =""))
    }
    prtCPUEWt <- function(x) {
      write.csv(CPUE_weight, base::paste(name,"-","CPUE_weight.csv",sep =""))
      write.csv(CPUE_weight_SE, base::paste(name,"-","CPUE_weight_SE.csv",sep =""))
    }
    prtSpeciesList <- function(x) {
      write.csv(SpeciesList, base::paste(name,"-","SpeciesList.csv",sep =""))
    }
    prtYearEffort <- function(x) {
      write.csv(Year_Sites, base::paste(name,"-","EffortByYear.csv",sep =""))
    }
    prtAnnSum <- function(x) {
      write.csv(annual_summary, base::paste(name,"-",max(annual_summary$yr,na.rm=T),"-","Annual_Summary.csv", sep=""))
    }
    
    if(OutTab == 0) {} else if (OutTab == 1) {
      prtCPUENum()
      prtCPUEWt()
      prtSpeciesList()
      prtYearEffort()
      prtAnnSum()
    } else if (OutTab == 2) {
      prtCPUENum()
    } else if (OutTab == 3) {
      prtCPUEWt()
    } else if (OutTab == 4) {
      prtSpeciesList()
    } else if (OutTab == 5) {
      prtYearEffort()
    }
  }
  PrintTables(OutTab)
  
  
  
  #### function output
  processed_data <- list(SpeciesList = SpeciesList,
                         SpeciesHistory = species.history,
                         CPUE_Tab_number = CPUE_Tab_number,
                         CPUE_number = CPUE_number,
                         CPUE_number_SE = CPUE_number_SE,
                         CPUE_Tab_weight = CPUE_Tab_weight,
                         CPUE_weight = CPUE_weight,
                         CPUE_weight_SE = CPUE_weight_SE,
                         Comp_num = Comp_num,
                         Comp_wt = Comp_wt,
                         Year_Sites = Year_Sites,
                         Comp_Sum = Comp_Sum,
                         TotalCount = TotalCount,
                         RawData = rawfile,
                         annual_summary = annual_summary
  )
  return(processed_data)
}

lds_plot_figs <- function (processed_data, FigstoPrint = 0, printdirectory = getwd(), name = "No name Specified") {
  #require(ggplot2)
  #Function to create figures for CPUE by number for each species
  #processed_data = orange_all
  Ctfigs <- function(x, name) {
    print("...CPUE by number for each species")
    simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      base::paste(toupper(substring(s, 1,1)), substring(s, 2),
                  sep="", collapse=" ")
    }
    for (i in 3:ncol(processed_data$CPUE_number)) {
      #i=3
      i_CPUE <- processed_data$CPUE_number |> pull(i)
      i_CPUE_SE <- processed_data$CPUE_number_SE |> pull(i)
      lower <- i_CPUE - (2*(i_CPUE_SE))
      lower[lower < 0] <- 0
      #lower[is.na(lower)] <- 0
      upper <- i_CPUE + ( 2 * i_CPUE_SE )
      #upper[ is.na(upper) ] <- 0
      xmn = min( na.omit( processed_data$CPUE_number$yr ) )
      xmx =  max( na.omit( processed_data$CPUE_number$yr ) )
      upperQ <- quantile(i_CPUE,0.75, na.rm = TRUE)
      med <- median(i_CPUE, na.rm = TRUE)
      lowerQ <- quantile(i_CPUE,0.25, na.rm = TRUE)
      Fig <- ggplot2::ggplot(data = processed_data$CPUE_number, ggplot2::aes(x = yr, y = i_CPUE)) + 
        geom_point() + 
        labs(title = base::paste(name,"-" ,simpleCap(names(processed_data$CPUE_number)[i])),
             x = names(processed_data$CPUE_number)[1],
             y = "Mean Catch Per Unit Effort by Number (#/minute \u00b1 2 SE)" ) +
        theme_bw() +
        scale_x_continuous(breaks= seq(xmn,xmx,1)) + 
        #labels = c(substr(processed_data$CPUE_number[,c(1)],1,4))) + 
        geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.2, size = 1) + 
        geom_ribbon(ggplot2::aes(ymax=upperQ,ymin=lowerQ),alpha=0.2, stat="identity", fill = "red") +
        geom_hline(yintercept = med, color = "dark red", lwd=1, lty=2) +
        scale_y_continuous(limits = c(0, 
                                      max(i_CPUE+(2*i_CPUE_SE)*1.05, na.rm = TRUE)),
                           expand = c( 0, 0 ) ) +
        theme(plot.title = element_text(hjust = 0.5))
      tiff(base::paste(printdirectory,"/",name,"-" ,names(processed_data$CPUE_number)[i],"-","#CPUE",".tiff", sep = ""), 
           height = 1800, 
           width = 2100, 
           res = 300, 
           compression = 'lzw')
      print(Fig)
      dev.off()
    }
  }
  #Function to create figures for CPUE by Weight for each species
  Wtfigs <- function(x, name) {
    print("...CPUE by weight for each species")
    simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      base::paste(toupper(substring(s, 1,1)), substring(s, 2),
                  sep="", collapse=" ")
    }
    for (i in 3:ncol(processed_data$CPUE_weight)) {
      i_CPUE <- processed_data$CPUE_weight |> pull(i)
      i_CPUE_SE <- processed_data$CPUE_weight_SE |> pull(i)
      lower <- i_CPUE-(2*(i_CPUE_SE))
      lower[lower < 0] <- 0
      upper <- i_CPUE+(2*i_CPUE_SE)
      
      Fig <- ggplot2::ggplot(data = processed_data$CPUE_weight, ggplot2::aes(x = Year, y = i_CPUE)) + geom_point() + 
        labs(title = base::paste(name,"-" ,simpleCap(names(processed_data$CPUE_weight)[i])), x = names(processed_data$CPUE_weight)[1], y = "Mean Catch Per Unit Effort by Weight (g/minute \u00b1 2 SE)" ) +
        theme_bw() +
        scale_x_discrete(labels = c(substr(processed_data$CPUE_weight[,c(1)],1,4))) + 
        geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper, width = 0.2), size = 1) + 
        scale_y_continuous(limits = c(0, (max(na.omit(i_CPUE+(2*i_CPUE_SE)*1.05)))), expand = c(0,0)) +
        theme(plot.title = element_text(hjust = 0.5))
      tiff(base::paste(printdirectory,"/",name,"-" ,names(processed_data$CPUE_weight)[i],"-","WtCPUE",".tiff", sep = ""), 
           height = 1800, 
           width = 2100, 
           res = 300, 
           compression = 'lzw')
      print(Fig)
      dev.off()
    }
  }
  #Function to create summary figs for a sample (total CPUE, Simpsons D, etc)
  Sumfigs <- function(x,name) {
    print("...Summary Figures")
    for (i in 2:ncol(processed_data$Comp_Sum)) {
      sumname <- names(processed_data$Comp_Sum)[i]
      titlename <- 'text'
      yname = 'text'
      if(sumname == "Tot_CPUE_num") {titlename = "Total Catch Per Minute by Number (All Species)"
      yname = "Total Catch Per Unit Effort (#/min)"} else 
        if (sumname == "Tot_CPUE_wt") {titlename = "Total Catch Per Minute by Weight (All Species)"
        yname = "Total Catch Per Unit Effort (g/min)"} else 
          if(sumname == "SimpsonsD") {titlename = "Modified Simpson's Diversity Index"
          yname = "Modified Simpsons Diversity Index (1/D)"}
      Fig <- ggplot2::ggplot(data = processed_data$Comp_Sum, ggplot2::aes(x = Year, y = processed_data$Comp_Sum[,c(i)])) + geom_line(ggplot2::aes(group = 1)) + 
        geom_point() +
        labs(title = base::paste(name,titlename, sep = "\n"), x = names(processed_data$Comp_Sum$Year)[1], y = yname) +
        theme_bw() +
        scale_x_discrete(labels = processed_data$Comp_Sum$Year) + 
        scale_y_continuous(limits = c((min(na.omit(processed_data$Comp_Sum[,c(i)]*0.95))), (max(na.omit(processed_data$Comp_Sum[,c(i)]*1.05)))), expand = c(0,0)) +
        theme(plot.title = element_text(hjust = 0.5))
      tiff(base::paste(printdirectory,"/",name,"-",sumname,".tiff", sep = ""), 
           height = 1800, 
           width = 2100, 
           res = 300, 
           compression = 'lzw')
      print(Fig)
      dev.off()
    }
  }
  
  if(FigstoPrint == 0) {
    # print("No Figures Printed")
  } else if(FigstoPrint == 1) {
    #  print("Printing All Figures")
    Ctfigs(processed_data, name)
    Wtfigs(processed_data, name)
    Sumfigs(processed_data, name)} else if(FigstoPrint ==2) {
      #   print('Printing CPUE by number figures only')
      Ctfigs(processed_data, name)} else if(FigstoPrint == 3) {
        #    print("Printing CPUE by wt figures only")
        Wtfigs(processed_data, name)} else if(FigstoPrint == 4) {
          #     print("printing summary figures only")
          Sumfigs(processed_data, name)} else {print ("Incorrect Figure type selection")}
}
